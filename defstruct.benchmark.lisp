;;;; benchmarks of structure-related functionality

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

;;; equalp comparison

(with-benchmark (:name (defstruct equalp :raw-slots)
                 :parameters ((type      ('sb-vm:word
                                          'single-float 'double-float
                                          '(complex double-float)))
                              (num-slots (:iota 4 0 16))))
  ;; TODO :expect (zerop bytes-consed)
  (let ((name (gensym)))
    (eval `(defstruct ,name
             ,@(map-into (make-list num-slots)
                (lambda ()
                  `(,(gensym) (coerce 0 ',type) :type ,type)))))
    (let ((a (make-instance name))
          (b (make-instance name)))
      (measuring (equalp a b)))))
