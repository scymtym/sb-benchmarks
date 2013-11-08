;;; CLOS implementation of the Fibonnaci function, with EQL specialization

(defgeneric eql-fib (x)
  (:method ((x (eql 0))) 1)
  (:method ((x (eql 1))) 1)
  (:method (x) (+ (eql-fib (- x 1)) (eql-fib (- x 2)))))

(with-benchmark (:name (:cl-bench :clos :eql-fib)
                 :parameters ((size (:expt 2 (:iota 5)))))
  (measuring (eql-fib size)))
