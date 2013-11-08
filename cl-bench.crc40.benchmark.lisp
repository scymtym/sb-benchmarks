(defun crc-division-step (bit rmdr poly msb-mask)
  (declare (type (signed-byte 56) rmdr poly msb-mask)
           (type bit bit))
  ;; Shift in the bit into the LSB of the register (rmdr)
  (let ((new-rmdr (logior bit (* rmdr 2))))
    ;; Divide by the polynomial, and return the new remainder
    (if (zerop (logand msb-mask new-rmdr))
        new-rmdr
        (logxor new-rmdr poly))))

(defun compute-adjustment (poly n)
  (declare (type (signed-byte 56) poly)
           (fixnum n))
  ;; Precompute X^(n-1) mod poly
  (let* ((poly-len-mask (ash 1 (1- (integer-length poly))))
         (rmdr (crc-division-step 1 0 poly poly-len-mask)))
    (dotimes (k (- n 1))
      (setf rmdr (crc-division-step 0 rmdr poly poly-len-mask)))
    rmdr))

(with-benchmark (:name (:cl-bench :crc40)
                 :parameters ((len (3014633))))
  (let ((crc-poly 1099587256329))
    (measuring (compute-adjustment crc-poly len))))
