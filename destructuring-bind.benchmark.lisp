;;;; benchmarks for DESTRUCTURING-BIND-related functionality

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

(with-benchmark (:name (destructuring-bind :linear)
                 :parameters ((length (:expt 2 (:iota 10)))))
  (let* ((names (map-into (make-list length) #'gensym))
         (values (map-into (make-list length) (constantly 1)))
         (fun (compile nil `(lambda ()
                              (destructuring-bind ,names ',values
                                (declare (ignore ,@names)))))))
    (declare (type function fun))
    (measuring (funcall fun))))

(with-benchmark (:name (destructuring-bind :smoke)
                 :parameters ((input (:iota 3))))
  (case input
    (0 (measuring
        (destructuring-bind (a b c d e) '(1 2 3 4 5)
          (declare (ignore a b c d e)))))
    (1 (measuring
        (destructuring-bind (&whole (foo bar) &rest r) '(1 2)
          (declare (ignore foo bar r)))))
    (2 (measuring
        (destructuring-bind
            (&whole boo blah (bla3 &optional foo bar &key bla4)
                    bli &key foo2 &allow-other-keys)
            '(1 (2 3 4 :bla4 6) 7 :foo 8)
          (declare (ignore boo blah bla3 foo bar bla4 bli foo2)))))))
