;;;; benchmarks of restart-related functionality

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;; Utilities

;; Call THUNK with TODO
;;
;; Note: this is only suitable for benchmarking the behavior of THUNK,
;; not for benchmarking establishing of restarts.
(defun call-with-established-restarts (num-clusters condition? thunk)
  (let ((condition (when condition?
                     (make-instance 'simple-error :format-control "foo"))))
    (labels ((call-thunk ()
               (if condition
                   (funcall thunk condition)
                   (funcall thunk)))
             (clusters (remaining)
               (flet ((next ()
                        (if (zerop remaining)
                            (call-thunk)
                            (clusters (1- remaining)))))
                 (restart-case
                     (if condition
                         (with-condition-restarts condition
                             (mapcar #'find-restart '(continue retry use-value))
                           (next))
                         (next))
                   (continue (&optional condition)
                     (declare (ignore condition)))
                   (retry ())
                   (use-value (value)
                     (declare (ignore value)))))))
      (if (zerop num-clusters)
          (call-thunk)
          (restart-case
              (clusters num-clusters)
            (outermost ()))))))

(defmacro with-established-restarts ((&key
                                      (num-clusters 1000)
                                      (condition    nil))
                                     &body body)
  `(call-with-established-restarts
    ,num-clusters ,(when condition t)
    (lambda (,@(when condition `(,condition)))
      ,@(when condition `((declare (ignorable ,condition))))
      ,@body)))

;;; Benchmark establishing of restarts

(with-benchmark (:name restart-bind
                 :parameters ((num-clusters (:expt 10 (:iota 5)))))
  (labels ((clusters (remaining)
             (unless (zerop remaining)
               (restart-bind ((continue (lambda (&optional condition)
                                          (declare (ignore condition))))
                              (retry (lambda ()))
                              (use-value (lambda (value)
                                           (declare (ignore value)))))
                 (clusters (1- remaining))))))
    (measuring (clusters num-clusters))))

(with-benchmark (:name restart-case
                 :parameters ((num-clusters (:expt 10 (:iota 5)))))
  (labels ((clusters (remaining)
             (unless (zerop remaining)
               (restart-case (clusters (1- remaining))
                 (continue (&optional condition)
                   (declare (ignore condition)))
                 (retry ())
                 (use-value (value)
                   (declare (ignore value)))))))
    (measuring (clusters num-clusters))))

;;; Benchmark associating conditions to restarts

(with-benchmark (:name with-condition-restarts
                 :parameters ((restart      ('continue 'outermost))
                              (num-clusters (:expt 10 (:iota 5)))))
  (with-established-restarts (:num-clusters num-clusters)
    (let ((condition (make-condition 'simple-error))
          (restarts (list (find-restart restart))))
      (measuring (with-condition-restarts condition restarts)))))

;;; Benchmark finding established restarts

;;; find-restart

(with-benchmark (:name (find-restart :symbol :without-condition)
                 :parameters ((restart      ('continue 'outermost))
                              (num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters)
    (case restart
      (continue  (measuring (find-restart 'continue)))
      (outermost (measuring (find-restart 'outermost))))))

(with-benchmark (:name (find-restart :symbol :with-condition)
                 :parameters ((restart      ('continue 'outermost))
                              (num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters
                              :condition condition)
    (case restart
      (continue  (measuring (find-restart 'continue condition)))
      (outermost (measuring (find-restart 'outermost condition))))))

(with-benchmark (:name (find-restart :instance :without-condition)
                 :parameters ((restart      ('continue 'outermost))
                              (num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters)
    (let ((restart-instance (find-restart restart)))
      (measuring (find-restart restart-instance)))))

(with-benchmark (:name (find-restart :instance :with-condition)
                 :parameters ((restart      ('continue 'outermost))
                              (num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters
                              :condition condition)
    (let ((restart-instance (find-restart restart)))
      (measuring (find-restart restart-instance condition)))))

;;; compute-restarts

(with-benchmark (:name (compute-restarts :without-condition)
                 :parameters ((num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters)
    (measuring (compute-restarts))))

(with-benchmark (:name (compute-restarts :with-condition)
                 :parameters ((num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters
                              :condition condition)
    (measuring (compute-restarts condition))))

;;; Benchmark invoking restarts

(with-benchmark (:name (invoke-restart :without-control-transfer)
                 :parameters ((restart ('with-args 'without-args))))
  (macrolet ((one-restart (restart)
               `(restart-bind
                    ((,restart
                       (lambda
                        ,@(ecase restart
                            (with-args `((a b c d)
                                         (declare (ignore a b c d))))
                            (without-args `(()))))))
                 (let ((restart (find-restart ',restart)))
                   (measuring (invoke-restart
                               restart
                               ,@(case restart
                                   (with-args `(1 2 3 4)))))))))
    (case restart
      (with-args    (one-restart with-args))
      (without-args (one-restart without-args)))))

#+later
(with-benchmark (:name (invoke-restart :with-control-transfer)
                 :parameters ((restart ('continue 'retry 'outermost))))
  (with-established-restarts (:num-clusters 10000)
    (let ((restart (find-restart restart)))
      (print (multiple-value-list (ignore-errors (invoke-restart restart))))
      (measuring (print (multiple-value-list (ignore-errors (invoke-restart restart))))))))
