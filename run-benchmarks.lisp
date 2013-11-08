(load "util/benchmark-util.lisp")
(load "util/plotting.lisp")

(cl:defpackage #:run-benchmarks
  (:use #:cl #:benchmark-util #:sb-ext))

(cl:in-package #:run-benchmarks)

(load "util/colorize.lisp")

(defvar *all-results* nil)
(defvar *accept-files* nil)

(defvar *break-on-error* nil)

(defvar *report-skipped-benchmarks** nil)
(defvar *report-style* :describe)
(defvar *report-target* *standard-output*)

(defun run-all ()
  (let ((compare-to nil))
    ;; Parse commandline options.
    (let ((args (rest *posix-argv*)))
      (flet ((missing-argument (option)
               (error "~@<Missing argument for ~A option.~@:>"
                      option)))
        (do ((arg (pop args) (pop args))) ((null arg))
          (cond #+later ((string= arg "--break-on-failure")
                         (setf *break-on-error* t)
                         (setf test-util:*break-on-failure* t))
                #+later ((string= arg "--report-skipped-benchmarks")
                         (setf *report-skipped-benchmarks* t))
                ((string= arg "--report-style")
                 (let* ((name (or (pop args) (missing-argument arg)))
                        (style (intern (string-upcase name) :keyword)))
                   (setf *report-style* style)))
                ((string= arg "--report-target")
                 (setf *report-target*
                       (or (pop args) (missing-argument arg))))
                ((string= arg "--compare-to")
                 (setf compare-to (or (pop args) (missing-argument arg))))
                ((string= arg "--no-color")) ; TODO
                (t
                 (push (truename (parse-namestring arg)) *accept-files*))))))

    (benchmark-runner (benchmark-files) #'load-benchmark)

    (ensure-directories-exist "results/")
    (with-open-file (stream (format nil "results/~A.lisp-expr" (lisp-implementation-version))
                            :direction         :output
                            :if-does-not-exist :create
                            :if-exists         :supersede)
      (write (list *all-results*
                   :version (lisp-implementation-version))
             :stream stream))

    (apply #'report (list *all-results*
                          :version (lisp-implementation-version)) ; TODO proper top-level structure
           (when compare-to
             (list :compare-to compare-to)))

    (sb-ext:exit :code (if (unexpected-failures *all-results*)
                           1
                           104))))

(defun run-in-child-sbcl (load-forms forms)
  ;; We used to fork() for POSIX platforms, and use this for Windows.
  ;; However, it seems better to use the same solution everywhere.
  (process-exit-code
   (#-win32 with-open-file #-win32 (devnull "/dev/null") #+win32 progn
     (sb-ext:run-program
      (first *POSIX-ARGV*)
      (append
       (list "--core" SB-INT:*CORE-STRING*
             "--noinform"
             "--no-sysinit"
             "--no-userinit"
             "--noprint"
             "--disable-debugger")
       (loop for form in (append load-forms forms)
             collect "--eval"
             collect (write-to-string form)))
      :output sb-sys:*stdout*
      :input #-win32 devnull #+win32 sb-sys:*stdin*))))

(defun run-benchmarks-in-child-sbcl (test-file test-code)
  (run-in-child-sbcl
    `((load "util/benchmark-util")
      #+TODO-probably-not-needed (load "assertoid")
      (cl:defpackage #:run-benchmarks
        (:use #:cl #:benchmark-util #:sb-ext)))

    `((cl:in-package #:cl-user)
      (cl:use-package '#:benchmark-util)
      #+TODO-probably-not-needed (use-package :assertoid)
      #+later (setf test-util:*break-on-failure* ,test-util:*break-on-failure*)
      (let ((file ,test-file)
        #+later (*break-on-error* ,run-tests::*break-on-error*))
        #+later (declare (special *break-on-error*))
        (format t "// Running ~a~%" file)
        (restart-case
            (handler-bind
                ;; TODO use make-error-handler
                ((error (lambda (condition)
                          (push (make-result :file file :status :unhandled-error)
                                benchmark-util:*results*)
                          (cond #+no (*break-on-error*
                                 (benchmark-util:really-invoke-debugger condition))
                                (t
                                 (format *error-output* "~&Unhandled ~a: ~a~%"
                                         (type-of condition) condition)
                                 (sb-debug:print-backtrace)))
                          (invoke-restart 'skip-file))))
              ,test-code)
          (skip-file ()
            (format t ">>>~a<<<~%" benchmark-util:*results*)))
        (benchmark-util:report-benchmark-status)
        (sb-ext:exit :code 104)))))

(defun setup-cl-user ()
  (cl:use-package '#:benchmark-util)
  #+TODO-probably-not-needed (use-package :assertoid))

(defun load-benchmark (file)
  `(load ,file))

(defun benchmark-runner (files test-fun)
  (format t "// Running benchmarks (~a)~%" test-fun)
  (let ((*package* (find-package '#:cl-user)))
    (setup-cl-user)
    (dolist (file files)
      (when (accept-test-file file)
        (force-output)
        (let ((exit-code (run-benchmarks-in-child-sbcl
                          file (funcall test-fun file))))
          (if (= exit-code 104)
              (with-open-file (stream "benchmark-result.lisp-expr"
                                      :direction :input
                                      :if-does-not-exist :error)
                (append-results
                 (sb-ext:without-package-locks ; test names may contain such symbols
                   (read stream))))
              (push (make-result :file file :status :invalid-exit-status)
                    *all-results*)))))))

#+TODO-not-used (defun make-error-handler (file)
  (lambda (condition)
    (push (make-result :file file :status :unhandled-error) *results*)
    (cond (*break-on-error*
           (benchmark-util:really-invoke-debugger condition))
          (t
           (format *error-output* "~&Unhandled ~a: ~a~%"
                   (type-of condition) condition)
           (sb-debug:print-backtrace)))
    (invoke-restart 'skip-file)))

(defun accept-test-file (file)
  (if *accept-files*
      (find (truename file) *accept-files* :test #'equalp)
      t))

(defun benchmark-files ()
  (directory "*.benchmark.lisp"))

;;; Result handling

(defun append-results (&optional (results *results*))
  (setf *all-results* (append results *all-results*)))

(defun failures (results)
  (remove :success results :key #'result-status))

(defun unexpected-failures (results)
  (remove-if (lambda (x)
               (member (result-status x)
                       '(:success
                         :expected-failure
                         :unexpected-success
                         :skipped-broken
                         :skipped-disabled)))
             results))

(defgeneric load-result (result-designator))

(defmethod load-result ((result-designator string))
  (load-result (make-pathname :directory '(:relative "results")
                              :name result-designator
                              :type "lisp-expr")))

(defmethod load-result ((result-designator pathname))
  (with-open-file (stream result-designator)
    (with-standard-io-syntax
      (read stream))))

;;; Result reporting

(defun report (results
               &key
               (style *report-style*)
               (target *report-target*)
               compare-to)
  (if compare-to
      (compare-using-style results (load-result compare-to) style target)
      (report-using-style results style target)))

(defgeneric report-using-style (results style target))

(defgeneric compare-using-style (new-results old-results style target))

(defmethod report-using-style ((results t) (style t) (target string))
  (report-using-style results style (parse-namestring target)))

(defmethod report-using-style ((results t) (style t) (target pathname))
  (with-open-file (stream target :if-does-not-exist :create
                                 :direction :output
                                 :if-exists :supersede)
    (report-using-style results style stream)))

;;; Describe style

(defmethod report-using-style ((results t) (style (eql :describe)) (target stream))
  (terpri target)
  (format target "Finished running tests.~%")
  (let ((skipcount 0)
        (*print-pretty* nil))
    (cond ((failures results)
           (format target "Status:~%")
           (dolist (failure (reverse (failures results)))
             (with-accessors ((status result-status)
                              (file result-file)
                              (condition result-condition)) failure
               (case status
                 (:unhandled-error
                  (output-colored-text status
                                       " Unhandled Error")
                  (format target " ~a~%"
                          (enough-namestring file)))
                 (:invalid-exit-status
                  (output-colored-text status
                                       " Invalid exit status:")
                  (format target " ~a~%"
                          (enough-namestring file)))
                 (:skipped-disabled
                  (when *report-skipped-benchmarks*
                    (format target " ~20a ~a / ~a~%"
                            "Skipped (irrelevant):"
                            (enough-namestring file)
                            condition))
                  (incf skipcount))
                 (t
                  (output-colored-text status
                                       (ecase status
                                         (:expected-failure " Expected failure:")
                                         (:unexpected-failure " Failure:")
                                         (:leftover-thread " Leftover thread (broken):")
                                         (:unexpected-success " Unexpected success:")
                                         (:skipped-broken " Skipped (broken):")
                                         (:skipped-disabled " Skipped (irrelevant):")))
                  (format target " ~a / ~a~%"
                          (enough-namestring file)
                          condition)))))
           (when (> skipcount 0)
             (format target " (~a tests skipped for this combination of platform and features)~%"
                     skipcount)))
          (t
           (format target "All tests succeeded~%")))))

(defmethod compare-using-style (new-results old-results (style (eql :describe)) (target stream))
  (labels ((relative-difference (old new)
             (if (and (zerop new) (zerop old))
                 0
                 (/ (- new old) (max new old))))
           (difference-greater/values (threshold old new)
             (let ((diffs (mapcar #'relative-difference old new)))
               (when (> (count-if (lambda (diff) (> (abs diff) threshold)) diffs)
                        (floor (length new) 2))
                 diffs)))
           (difference-greater/results (threshold old new accessor)
             #+no (difference-greater/values
              threshold
              (measurement-values/sorted (funcall accessor new))
              (measurement-values/sorted (funcall accessor old)))
             (when (> (abs (relative-difference
                            (measurement-median (funcall accessor old))
                            (measurement-median (funcall accessor new))))
                      threshold)
               (mapcar #'relative-difference
                       (measurement-values/sorted (funcall accessor old))
                       (measurement-values/sorted (funcall accessor new)))))
           (analyze-quantity (name old new threshold accessor)
             (let ((diffs (difference-greater/results threshold old new accessor)))
               (when diffs
                 (format target "~A difference ~S ~S~%~2@T~{~,,2F%~^, ~}~%"
                         name (result-name new) (parameter-result-parameters new)
                         diffs)))))
    (dolist (new-result (first new-results))

      (let ((old-result (find-if (lambda (result)
                                   (corresponding-results-p new-result result))
                                 (first old-results))))


        (when old-result
          (let* ((a (likelihood-of-common-distribution
                     (result-runtime old-result) (result-runtime new-result)))
                 (b (likelihood-of-common-distribution
                     (result-runtime new-result) (result-runtime new-result)))
                (a* (max a b))
                (b* (min a b)))
            (when (and (not (zerop a*)) (not (<= 1/2 (/ b* a*) 2)))
              (format target "~S ~A ~A ~A~%"
                      (result-name new-result)
                      b* a* (/ b* a*))
              (format target "~S ~A + ~A~%"
                      (result-name new-result)
                      (measurement-mean (result-runtime new-result))
                      (measurement-variance (result-runtime new-result)))
              (format target "~S ~A + ~A~%"
                      (result-name old-result)
                      (measurement-mean (result-runtime old-result))
                      (measurement-variance (result-runtime old-result))))))

        #+no (if (not old-result)
            (format target "No old result for ~S ~S~%"
                    (result-name new-result)
                    (parameter-result-parameters new-result))
            (progn
              (analyze-quantity :runtime      old-result new-result .2  #'result-runtime)
              (analyze-quantity :bytes-consed old-result new-result .01 #'result-bytes-consed)))))))

;;; Sexp style

(defun write-universal-time/iso (&optional (time (get-universal-time))
                                           (stream t))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time time)
    (declare (ignore day-of-week dst-p))
    (format stream "~4,'0D-~2,'0D-~2,'0DT~2,'0d:~2,'0d:~2,'0d~@D:00"
            year
            month
            date
            hour
            minute
            second
            (- tz))))

(defmethod report-using-style (results (style (eql :sexp)) (target stream))
  (flet ((one-result (result)
           (with-accessors ((file result-file) (name result-name)
                            (status result-status) (condition result-condition)) result
             `(:file ,(enough-namestring file)
               :test ,name
               :status ,status
               ,@(when (eq status :unexpected-failure)
                   `(:condition ,(prin1-to-string condition)))))))
    (let ((time (get-universal-time)))
      (write-string "; Timestamp: " target)
      (write-universal-time/iso time target)
      (print
       `(:timestamp ,time
         :implementation-version ,(lisp-implementation-version)
         :machine-type ,(machine-type)
         :machine-instance ,(machine-instance)
         :features ,*features*
         :results ,(mapcar #'one-result results))
       target))))

;;; Plot style

;; TODO do something useful when TARGET is a pathname
(defmethod report-using-style (results (style (eql :plot)) (target t))
  (benchmark-util::plot (list results)))

(defmethod compare-using-style (new-results old-results (style (eql :plot)) (target t))
  (benchmark-util::plot (list new-results old-results)))
