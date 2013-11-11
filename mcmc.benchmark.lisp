
;;; Misc types
(deftype uint-t (width) `(unsigned-byte ,width))
(deftype uint64-t () '(uint-t 64))
(deftype uint8-t () '(uint-t 8))

;;; LCG
(defconstant +width+ 64)
(defconstant +multiplier+ 6364136223846793005)
(defconstant +addend+ 1442695040888963407)
(defconstant +mask+ (- (expt 2 +width+) 1))
(defconstant RANDOM-MAX +mask+)
(defconstant +expt-2-52+ (expt 2 52) "Maximum exact integer with -1-valued scale parameter (i.e., exponent) in IEEE754 double-float precision format.")
(defconstant +expt-2-52d0+ (expt 2d0 52))

(declaim (inline make-generator))
(defun make-generator () (logand 8682522807148012 +mask+))

(declaim (inline random-next select))
(defun random-next (r)
  (logand (+ (* r +multiplier+)
             +addend+)
          +mask+))


;;; LCG support code
(defun select (r bits)
  "Choose BITS msb from +width+ bit value R."
  (ash r (- bits +width+)))

(declaim (inline ->double-biased ->double/52 ->double/53))
(defun ->double/52 (r)
  (/ (select r 52) +expt-2-52d0+))
(defun ->double/53 (r)
  (* (select r 53) (expt 2d0 -53)))
(defun ->double/biased (r)
  (* r (/ 1.0d0 (+ 1.0d0 +mask+))))

;;; Counts -> frequencies
(declaim (ftype (function ((simple-array uint64-t 1))
                          (values (simple-array double-float 1)
                                  &optional))
                normalize!))
(defun normalize! (vec)
  (let ((scale (/ 1d0 (reduce #'+ vec))))
    (map '(simple-array double-float 1)
         (lambda (x) (* x scale))
         vec)))

;;; Generic MCMC loops, parameterised on the routine to get a single
;;; random bit and random bits w/ P[bit = 0] = p0, the single
;;; argument.
;;;
;;; Runs for N iterations before returning the count vector.
(declaim (maybe-inline generic-mcmc))
(defun generic-mcmc (N uniform-bit biased-bit)
  (declare (type uint64-t N))
  (let ((Ncounts (make-array 2 :element-type 'uint64-t
                               :initial-contents '(0 0)))
        (burglary (funcall uniform-bit))
        (earthquake (funcall uniform-bit))
        (alarm (funcall uniform-bit))
        (p-burglary-0 0.0d0)
        (p-earthquake-0 0.0d0)
        (p-alarm-0 0.0d0)
        (cpt-alarm-*-0-* (make-array '(2 2) :element-type 'double-float
                                            :initial-contents
                                            '((0.999d0 0.001d0)
                                              (0.060d0 0.940d0))))
        (cpt-alarm-0-*-* (make-array '(2 2) :element-type 'double-float
                                            :initial-contents
                                            '((0.999d0 0.001d0)
                                              (0.710d0 0.290d0))))
        (cpt-alarm-*-*-0 (make-array '(2 2) :element-type 'double-float
                                            :initial-contents
                                            '((0.999d0 0.710d0)
                                              (0.060d0 0.050d0))))
        (cpt-johncalls-*-1 (make-array
                            2 :element-type 'double-float
                              :initial-contents '(0.050d0 0.900d0)))
        (cpt-marycalls-*-1 (make-array
                            2 :element-type 'double-float
                              :initial-contents '(0.010d0 0.700d0))))
    (flet ((count-burglary ()
             (setf (aref Ncounts burglary)
                   (ldb (byte 64 0) (1+ (aref Ncounts burglary)))))
           (biased-bit (p0)
             (funcall biased-bit p0)))
      (declare (inline count-burglary))
      (when (plusp n)
        (loop repeat (1- n) do
                 (count-burglary)
                 (setf p-burglary-0 (* 0.999d0 (aref cpt-alarm-0-*-*
                                                     earthquake alarm))
                       burglary (biased-bit p-burglary-0))
                 (count-burglary)
                 (setf p-earthquake-0 (* 0.998d0 (aref cpt-alarm-*-0-*
                                                       burglary earthquake))
                       earthquake (biased-bit p-earthquake-0))
                 (count-burglary)
                 (setf p-alarm-0 (* (aref cpt-alarm-*-*-0 burglary earthquake)
                                    (aref cpt-johncalls-*-1 alarm)
                                    (aref cpt-marycalls-*-1 alarm))
                       alarm (biased-bit p-alarm-0))))
      Ncounts)))

;;; Turning the rank-2 arrays into rank-1 vectors helps reduce the
;;; amount of indirection a tiny bit, and enables complete DX
;;; allocation.
(declaim (maybe-inline generic-mcmc-2))
(defun generic-mcmc-2 (N uniform-bit biased-bit)
  (declare (type uint64-t N))
  (let ((Ncounts (make-array 2 :element-type 'uint64-t
                               :initial-contents '(0 0)))
        (burglary (funcall uniform-bit))
        (earthquake (funcall uniform-bit))
        (alarm (funcall uniform-bit))
        (p-burglary-0 0.0d0)
        (p-earthquake-0 0.0d0)
        (p-alarm-0 0.0d0)
        (cpt-alarm-*-0-* (make-array 4 :element-type 'double-float
                                       :initial-contents
                                       '(0.999d0 0.001d0
                                         0.060d0 0.940d0)))
        (cpt-alarm-0-*-* (make-array 4 :element-type 'double-float
                                       :initial-contents
                                       '(0.999d0 0.001d0
                                         0.710d0 0.290d0)))
        (cpt-alarm-*-*-0 (make-array 4 :element-type 'double-float
                                       :initial-contents
                                       '(0.999d0 0.710d0
                                         0.060d0 0.050d0)))
        (cpt-johncalls-*-1 (make-array
                            2 :element-type 'double-float
                              :initial-contents '(0.050d0 0.900d0)))
        (cpt-marycalls-*-1 (make-array
                            2 :element-type 'double-float
                              :initial-contents '(0.010d0 0.700d0))))
    (declare (dynamic-extent cpt-alarm-*-0-*
                             cpt-alarm-0-*-*
                             cpt-alarm-*-*-0
                             cpt-johncalls-*-1
                             cpt-marycalls-*-1))
    (flet ((count-burglary ()
             (setf (aref Ncounts burglary)
                   (ldb (byte 64 0) (1+ (aref Ncounts burglary)))))
           (biased-bit (p0)
             (funcall biased-bit p0)))
      (declare (inline count-burglary))
      (when (plusp n)
        (loop repeat (1- n) do
                 (count-burglary)
                 (setf p-burglary-0 (* 0.999d0 (aref cpt-alarm-0-*-*
                                                     (+ (* 2 earthquake)
                                                        alarm)))
                       burglary (biased-bit p-burglary-0))
                 (count-burglary)
                 (setf p-earthquake-0 (* 0.998d0 (aref cpt-alarm-*-0-*
                                                       (+ (* 2 burglary)
                                                          earthquake)))
                       earthquake (biased-bit p-earthquake-0))
                 (count-burglary)
                 (setf p-alarm-0 (* (aref cpt-alarm-*-*-0
                                          (+ (* 2 burglary) earthquake))
                                    (aref cpt-johncalls-*-1 alarm)
                                    (aref cpt-marycalls-*-1 alarm))
                       alarm (biased-bit p-alarm-0))))
      Ncounts)))

(with-benchmark (:name mcmc/pkhuong
                 :parameters ((variant (:mt :mt-2 :lcg :lcg-2))
                              (N (:expt 10 (:iota 5)))))
  (case variant
    (:mt
     (measuring
      (locally
          (declare (optimize speed) (inline generic-mcmc))
        (let ((state *random-state*))
          (generic-mcmc N
                        (lambda () (random 2 state))
                        (lambda (p0)
                          (if (< (random 1d0 state) p0) 1 0)))))))
    (:mt-2
     (measuring
      (locally
          (declare (optimize speed) (inline generic-mcmc-2))
        (let ((state *random-state*))
          (generic-mcmc-2 N
                          (lambda () (random 2 state))
                          (lambda (p0)
                            (if (< (random 1d0 state) p0) 1 0)))))))

    (:lcg
     (measuring
      (locally
          (declare (optimize speed) (inline generic-mcmc))
        (let ((state (make-generator)))
          (generic-mcmc N
                        (lambda ()
                          (select (setf state (random-next state)) 1))
                        (lambda (p0)
                          (let ((r (->double/52
                                    (setf state (random-next state)))))
                            (if (> r p0) 1 0))))))))

    (:lcg-2
     (measuring
      (locally
          (declare (optimize speed) (inline generic-mcmc-2))
        (let ((state (make-generator)))
          (generic-mcmc-2 N
                          (lambda ()
                            (select (setf state (random-next state)) 1))
                          (lambda (p0)
                            (let ((r (->double/52
                                      (setf state (random-next state)))))
                              (if (> r p0) 1 0))))))))))
