(with-benchmark (:name (:cl-bench :arrays :1d)
                 :parameters ((size (:expt 10 (:iota 6)))))
  (let ((ones (make-array size :element-type '(integer 0 1000) :initial-element 1))
        (twos (make-array size :element-type '(integer 0 1000) :initial-element 2))
        (threes (make-array size :element-type '(integer 0 2000))))
    (measuring
     (dotimes (pos size)
       (setf (aref threes pos) (+ (aref ones pos) (aref twos pos)))))))

(with-benchmark (:name (:cl-bench :arrays :2d)
                 :parameters ((size (:expt 10 (:iota 4)))))
  (let ((ones (make-array (list size size) :element-type '(integer 0 1000) :initial-element 1))
        (twos (make-array (list size size) :element-type '(integer 0 1000) :initial-element 2))
        (threes (make-array (list size size) :element-type '(integer 0 2000))))
    (measuring
     (dotimes (i size)
       (dotimes (j size)
         (setf (aref threes i j) (+ (aref ones i j) (aref twos i j))))))))
