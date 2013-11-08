(defun read-many-lines (file)
  (with-open-file (f file :direction :input :external-format :utf-8)
    (loop :for l = (read-line f nil)
          :while l
          :count (length l))))

(with-benchmark (:name (:cl-bench :run-slurp-lines)) ; TODO skip when file not found
  (cond ((probe-file "/usr/share/dict/words")
         (measuring (read-many-lines "/usr/share/dict/words")))
        ((probe-file "/usr/dict/words")
         (measuring (read-many-lines "/usr/dict/words")))))

(defconstant +digit+ "0123456789ABCDEF")

(defconstant +digits-needed+
  #((10 100 1000 10000 100000 10000000 100000000 536870911)
    (16 256 4096 65536 1048576 16777216 268435456 4294967296 536870911)))

(defun fixnum-to-string (n base)
  (declare (fixnum n base))
  (let* ((tsize (position-if (lambda (x) (> (the fixnum x) n))
                             (aref +digits-needed+ (ash base -4))))
         (result (make-string (1+ tsize))))
    (loop for i fixnum from tsize downto 0 with q fixnum = n and r fixnum = 0
          do (multiple-value-setq (q r) (floor q base))
             (setf (schar result i) (aref +digit+ r)))
    result))

(with-benchmark (:name (:cl-bench :hash-strings)
                 :parameters ((size (:expt 10 (:iota 5)))))
  (measuring
   (let ((table (make-hash-table :test #'equal :size size)))
     (dotimes (i size)
       (setf (gethash (fixnum-to-string i 16) table) i))
     (maphash (lambda (key value) (incf (gethash key table) value))
              table))))

(with-benchmark (:name (:cl-bench :hash-integers)
                 :parameters ((size (:expt 10 (:iota 5)))))
  (measuring
   (let ((table (make-hash-table :test #'eql)))
     (dotimes (i size)
       (setf (gethash i table) (1+ i)))
     (maphash (lambda (key value) (incf (gethash key table) value))
              table))))
