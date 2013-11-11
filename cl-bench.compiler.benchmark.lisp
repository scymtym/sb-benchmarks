(with-benchmark (:name (:cl-bench :compiler)
                 :parameters ((file (#P"cl-bench.gabriel.disabled.lisp")))) ; TODO just gabriel.lisp?
  (let ((*error-output* (make-broadcast-stream)))
    (measuring
     (compile-file file :print nil :verbose nil))))
