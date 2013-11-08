;;;; benchmarks of list-related functionality

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

;;; [n]butlast

(macrolet ((define-*butlast-test (function)
             `(with-benchmark (:name ,function
                              :parameters ((length (:expt 10 (:iota 6)))))
                (let ((input (make-list length)))
                  (measuring (,function input))))))
  (define-*butlast-test nbutlast)
  (define-*butlast-test butlast))
