(cl:defpackage #:benchmark-util
  (:use
   #:cl
   #:sb-ext)

  (:export
   #:measurement-values
   #:measurement-values/sorted
   #:measurement-min
   #:measurement-max
   #:measurement-sum
   #:measurement-mean
   #:measurement-variance
   #:measurement-quantile
   #:measurement-median

   #:likelihood-of-common-distribution

   #:result-runtime
   #:result-bytes-consed

   #:parameter-result-parameters

   #:corresponding-results-p

   #:with-benchmark
   #:measuring

   #:report-benchmark-status

   #:result-file #:result-name #:result-status #:result-condition #:make-result
           #:result-failure-p #:result-error-p
           #:with-test #:report-test-status #:*results*
           #:really-invoke-debugger
           #:*break-on-failure* #:*break-on-expected-failure*
           #:make-kill-thread #:make-join-thread
           #:runtime))

(cl:in-package #:benchmark-util)

(defvar *benchmark-file* nil)
(defvar *results* '())
(defvar *break-on-failure* nil)

;;; Utilities

(defun required-argument (&optional name)
  (error "Missing required argument~@[ ~S~]" name))

;;; Measurements

(defstruct (measurement #+later (:constructor make-measurement (values)))
  (values (required-argument :values) :type list :read-only t))

(defun %make-measurement (values)
  (make-measurement :values values))

(macrolet ((define-reducing-reader (name reduce-by)
             `(defun ,name (measurement)
                (reduce #',reduce-by (measurement-values measurement)))))
  (define-reducing-reader measurement-min min)
  (define-reducing-reader measurement-max max)
  (define-reducing-reader measurement-sum +))

(defun measurement-mean (measurement)
  (/ (measurement-sum measurement)
     (length (measurement-values measurement))))

(defun measurement-variance (measurement)
  (let ((n (length (measurement-values measurement)))
        (mean (measurement-mean measurement)))
    (/ (reduce (lambda (sum x) (+ sum (expt (- mean x) 2)))
               (measurement-values measurement)
               :initial-value 0)
       n)))

(defun measurement-values/sorted (measurement)
  (sort (copy-list (measurement-values measurement)) #'<))

(defun measurement-quantile (measurement q k)
  (declare (type (integer 1) q k))
  (when (measurement-values measurement)
    (let ((index (1- (ceiling (* k (length (measurement-values measurement))) q))))
      (elt (measurement-values/sorted measurement) index))))

(defun measurement-median (measurement)
  (measurement-quantile measurement 2 1))

(defgeneric likelihood-of-common-distribution (left right))

(defmethod likelihood-of-common-distribution ((left measurement)
                                              (right measurement))
  (let* ((n (length (measurement-values right)))
         (mean (measurement-mean left))
         (variance (measurement-variance left))
         (sum (reduce (lambda (sum x) (+ sum (expt (- mean x) 2)))
                      (measurement-values right)
                      :initial-value 0)))
    (cond
      ((not (zerop variance))
       (- (+ (* (/ n 2) (log (* 2 pi)))
             (* (/ n 2) (log variance))
             (/ sum 2 variance))))
      ((zerop sum)
       0)
      (t
       most-negative-double-float))))

;;; Benchmark results

(defstruct result
  (file (required-argument :file) :type pathname :read-only t)
  (name nil :type (or null string symbol cons) :read-only t)
  (status (required-argument :status) :type keyword :read-only t)
  (condition nil :type (or null string condition) :read-only t)
  (runtime nil :type (or null measurement) :read-only t)
  (bytes-consed nil :type (or null measurement) :read-only t)
  (code-size nil :type (or null measurement) :read-only t))

(defstruct (parameter-result (:include result))
  (parameters nil :type list :read-only t))

(defun result-failure-p (result)
  (member (result-status result) '(:unexpected-failure)))

(defun result-error-p (result)
  (member (result-status result) '(:unhandled-error :invalid-exit-status)))

(defmethod print-object ((object result) stream)
  (if *print-escape*
      (call-next-method)
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "~A ~A" (result-name object) (result-status object)))))

(defgeneric corresponding-results-p (left right))

(defmethod corresponding-results-p ((left result)
                                    (right result))
  (and (eq (class-of left) (class-of right))
       (equalp (result-name left) (result-name right))))

(defmethod corresponding-results-p ((left parameter-result)
                                    (right parameter-result))
  (and (call-next-method)
       (equalp (parameter-result-parameters left)
               (parameter-result-parameters right))))

;;; TODO

(defun report-benchmark-status ()
  (with-standard-io-syntax
    (with-open-file (stream "benchmark-result.lisp-expr"
                            :direction :output
                            :if-exists :supersede)
      (format stream "~s~%" *results*))))

;;; Benchmark execution runtime support

(defun log-msg (&rest args)
  (format *trace-output* "~&::: ")
  (apply #'format *trace-output* args)
  (terpri *trace-output*)
  (force-output *trace-output*))

(defun start-benchmark ()
  (unless (eq *benchmark-file* *load-pathname*)
    (setf *benchmark-file* *load-pathname*)))

(defun really-invoke-debugger (condition)
  (with-simple-restart (continue "Continue")
    (let ((*invoke-debugger-hook* *invoke-debugger-hook*))
      (enable-debugger)
      (invoke-debugger condition))))

(defun fail-benchmark (type name condition)
  (if (stringp condition)
      (log-msg "~@<~A ~S ~:_~A~:>"
               type name condition)
      (log-msg "~@<~A ~S ~:_due to ~S: ~4I~:_\"~A\"~:>"
               type name condition condition))
  (push (make-result :file *benchmark-file*
                     :name name
                     :status type
                     :condition (princ-to-string condition))
        *results*)
  (unless (stringp condition)
    (when *break-on-failure*
      (really-invoke-debugger condition))))

(defun broken-p (broken-on)
  (sb-impl::featurep broken-on))

(defun skipped-p (skipped-on)
  (sb-impl::featurep skipped-on))

;;; Benchmark execution functions and macros

#+TODO-maybe (defun get-time ()
  #+no (multiple-value-bind (sec nsec) (sb-ext:get-time-of-day)
         (+ sec (/ nsec 1000000000d0)))
  (/ (get-internal-real-time) (float internal-time-units-per-second 1.0d0)))

;;; Repeat calling THUNK until its cumulated runtime, measured using
;;; GET-INTERNAL-RUN-TIME, is larger than PRECISION. Repeat this
;;; REPETITIONS many times and return the time one call to THUNK took
;;; in seconds as a float, according to the minimum of the cumulated
;;; runtimes over the repetitions.
;;; This allows to easily measure the runtime of expressions that take
;;; much less time than one internal time unit. Also, the results are
;;; unaffected, modulo quantization effects, by changes to
;;; INTERNAL-TIME-UNITS-PER-SECOND.
;;; Taking the minimum is intended to reduce the error introduced by
;;; garbage collections occurring at unpredictable times. The inner
;;; loop doubles the number of calls to THUNK each time before again
;;; measuring the time spent, so that the time measurement overhead
;;; doesn't distort the result if calling THUNK takes very little time.
(defun runtime* (thunk repetitions precision)
  (declare (type function thunk))
  (let ((all-internal-time-units-per-call '())
        (all-bytes-consed-per-call '()))
    (loop repeat repetitions
          do (loop with start = (get-internal-run-time)
                   with start-consed = (nth-value 3 (sb-impl::time-get-sys-info)) ; TODO
                   with duration = 0
                   with bytes-consed = 0
                   for n = 1 then (* n 2)
                   for total-runs = n then (+ total-runs n)
                   do (dotimes (i n) (funcall thunk))
                      (setf duration (- (get-internal-run-time) start)
                            bytes-consed (- (nth-value 3 (sb-impl::time-get-sys-info))
                                            start-consed))
                   until (> duration precision)
                   finally (let ((internal-time-units-per-call
                                   (/ (float duration) (float total-runs)))
                                 (bytes-consed-per-call
                                   (/ (float bytes-consed) (float total-runs))))
                             (push internal-time-units-per-call
                                   all-internal-time-units-per-call)
                             (push (ceiling bytes-consed-per-call)
                                   all-bytes-consed-per-call))))
    (values
     (mapcar (lambda (x)
               (/ x (float internal-time-units-per-second)))
             all-internal-time-units-per-call)
     all-bytes-consed-per-call)))

(defmacro runtime (form &key (repetitions 5) (precision 10))
  `(runtime* (lambda () ,form) ,repetitions ,precision))

(defun call-with-measuring (thunk)
  "TODO(jmoringe): document"
  (runtime* thunk 5 10))

;; TODO do this in a less embarrassing way
(defun estimate-size (fun)
  (let* ((string (with-output-to-string (stream)
                   (sb-disassem:disassemble-fun fun :stream stream)))
         (index (search "Size: " string)))
    (parse-integer (subseq string (+ index 6)) :junk-allowed t)))

(defmacro with-measuring (() &body body)
  "TODO(jmoringe): document"
  (let ((measured? (gensym))
        (name (gensym)))
    `(let ((,measured? nil))
       (macrolet ((measuring (&body body)
                    `(flet ((,',name () ,@body))
                       (setf ,',measured?
                             (append (multiple-value-list
                                      (call-with-measuring #',',name))
                                     (list (list (estimate-size #',',name))))))))
         ,@body
         (unless ,measured?
           (error "~@<Benchmark did not measure anything~@:>"))
         (values-list ,measured?)))))

(defun call-with-parameters (parameter-values thunk)
  (declare (function thunk))
  (let ((args (make-list (length parameter-values))))
    (declare (dynamic-extent args))
    (labels ((bind-parameters (head values)
               (destructuring-bind
                   (&optional first-values &rest rest-values) values
                (cond
                  (rest-values
                   (dolist (value first-values)
                     (setf (car head) value)
                     (bind-parameters (rest head) rest-values)))
                  (first-values
                   (dolist (value first-values)
                     (setf (car head) value)
                     (apply thunk args)))
                  (t (funcall thunk))))))
      (bind-parameters args parameter-values))))

(defun parse-parameter-spec (spec)
  (labels ((parse-value-spec (spec)
             (etypecase spec
               ((cons (eql :expt) (cons integer))
                (mapcar (lambda (value)
                          (expt (second spec) value))
                        (parse-value-spec (nth 2 spec))))
               ((cons (eql :iota))
                (destructuring-bind (size &optional (start 0) (step 1))
                    (rest spec)
                  (loop :repeat size :for i :from start :by step
                        :collect i)))
               (cons
                spec))))
   (destructuring-bind (name values) spec
     (list name (parse-value-spec values)))))

(defun parameter-name (parameter)
  (first parameter))

(defun parameter-values (parameter)
  `(list ,@(second parameter)))

(defmacro with-parameters ((&rest parameter-specs)
                           &body body)
  (let ((parameters
          (mapcar #'parse-parameter-spec parameter-specs)))
    `(call-with-parameters
      (list ,@(mapcar #'parameter-values parameters))
      (lambda ,(mapcar #'parameter-name parameters) ,@body))))

(defun note-skipped-benchmark (name)
  (fail-benchmark
   :skipped-disabled name
   "Benchmark disabled for this combination of platform and features"))

(defun call-as-benchmark-body (name parameters thunk)
  "TODO(jmoringe): document"
  (declare (type function thunk))
  (handler-bind ((error (lambda (error)
                          (fail-benchmark :expected-failure name error)
                          (return-from call-as-benchmark-body))))
    (log-msg "~:[Running ~S~:;~2@TRunning ~:*~S~]"
             parameters name)
    (multiple-value-bind (runtimes bytes-consed code-sizes) (funcall thunk)
      (let ((runtimes (%make-measurement runtimes))
            (bytes-consed (%make-measurement bytes-consed))
            (code-sizes (%make-measurement code-sizes)))
        (push (make-parameter-result
               :file *benchmark-file*
               :name name
               :status :success
               :runtime runtimes
               :bytes-consed bytes-consed
               :code-size code-sizes
               :parameters parameters)
              *results*)
        (log-msg "~:[Done ~S~:;~2@TDone ~:*~S~*~] ~
                  => ~A s; ~
                  ~,3:D byte~:P consed; ~
                  ~,3:D byte~:P code ~
                  [~D measurement~:P]"
                 parameters name
                 (measurement-median runtimes)
                 (measurement-median bytes-consed)
                 (measurement-median code-sizes)
                 (length (measurement-values runtimes)))))))

(defmacro with-benchmark ((&key
                           name
                           skipped-on
                           parameters)
                          &body body)
  (let ((parameters/parsed (mapcar #'parse-parameter-spec parameters))
        (parameters-var (gensym)))
    (flet ((make-body ()
             (if parameters/parsed
                 `(progn
                    (log-msg "Running ~S" ',name)
                    (with-parameters ,parameters

                      (let ((,parameters-var (list ,@(reduce #'append parameters/parsed
                                                             :key (lambda (parameter)
                                                                    (list `(quote ,(parameter-name parameter))
                                                                          (parameter-name parameter)))))))
                        (call-as-benchmark-body
                         ',name ,parameters-var (lambda () (with-measuring () ,@body)))))

                    (log-msg "Done ~S" ',name))
                 `(call-as-benchmark-body
                   ',name '() (lambda () (with-measuring () ,@body))))))
      `(progn
         (start-benchmark)
         (cond
           ((skipped-p ,skipped-on)
            (note-skipped-benchmark ',name))
           (t
            ,(make-body)))))))
