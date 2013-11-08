(defstruct node left right dummy1 dummy2)

;; build tree top down, assigning to older objects
(defun populate (depth thisNode)
  (when (> depth 0)
    (setf (node-left thisNode) (make-node))
    (setf (node-right thisNode) (make-node))
    (populate (1- depth) (node-left thisNode))
    (populate (1- depth) (node-right thisNode))))

;; build tree bottom-up
(defun make-tree (depth)
  (if (<= depth 0) (make-node)
      (make-node :left (make-tree (- depth 1))
                 :right (make-tree (- depth 1)))))

;; nodes used by a tree of a given size
(defmacro tree-size (i) `(- (expt 2 (1+ ,i)) 1))

;; number of iterations to use for a given tree depth
(defmacro iteration-count (i)
  `(floor (* 2 (tree-size stretch-tree-depth))
          (tree-size ,i)))

(with-benchmark (:name (:cl-bench :boehm-gc)
                 :parameters ((stretch-tree-depth (3 9 18))))
  ;;   Parameters are determined by stretch-tree-depth.
  ;;   In Boehm's version the parameters were fixed as follows:
  ;;     public static final int stretch-tree-depth    = 18;  // about 16Mb
  ;;     public static final int kLongLivedTreeDepth  = 16;  // about 4Mb
  ;;     public static final int kArraySize  = 500000;       // about 4Mb
  ;;     public static final int kMinTreeDepth = 4;
  ;;     public static final int kMaxTreeDepth = 16;
  ;;   In Larceny the storage numbers above would be 12 Mby, 3 Mby, 6 Mby.

  (let* ((kLongLivedTreeDepth (- stretch-tree-depth 2))
         (kArraySize          (* 4 (tree-size kLongLivedTreeDepth)))
         (kMinTreeDepth       4)
         (kMaxTreeDepth       kLongLivedTreeDepth))
    ;; (format t "Stretching memory with a binary tree of depth ~d~%" stretch-tree-depth)

    ;; stretch the memory space quickly
    (make-tree stretch-tree-depth)

    ;; Create a long lived object
    ;; (format t "Creating a long-lived binary tree of depth ~d~%" kLongLivedTreeDepth)
    (let ((longLivedTree (make-node)))
      (populate kLongLivedTreeDepth longLivedTree)

      ;; create long-lived array, filling half of it
      ;; (format t "Creating a long-lived array of ~d inexact reals~%" kArraySize)
      (let ((array (make-array kArraySize :element-type 'single-float)))
        (loop :for i :below (floor kArraySize 2)
              :do (setf (aref array i) (/ 1.0 (1+ i))))

        (measuring
         (do ((d kMinTreeDepth (+ d 2)))
             ((> d kMaxTreeDepth))
           (let ((iteration-count (iteration-count d)))
             ;; (format t "~&Creating ~d trees of depth ~d~%" iteration-count d)
             ;; (format t "GCBench: Top down construction~%")
             (dotimes (i iteration-count) (populate d (make-node)))
             ;; (format t "GCBench: Bottom up construction~%")
             (dotimes (i iteration-count) (make-tree d)))))

        ;; these are fake references to LongLivedTree and array to
        ;; keep them from being optimized away
        (assert (not (null longLivedTree)))
        (assert (let ((n (min 1000 (1- (floor (length array) 2)))))
                  (= (round (aref array n)) (round (/ 1.0 (1+ n))))))))))
