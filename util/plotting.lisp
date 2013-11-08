(cl:in-package #:benchmark-util)

(defun test-name->filename (name)
  (substitute-if
   #\- (lambda (c) (member c '(#\( #\) #\Space)))
   (string-trim
    '(#\( #\)) (string-downcase (princ-to-string name)))))

(defun write-gnuplot-script (scriptfile output-filename files-and-columns
                             &key
                             logy2?)
  (with-open-file (stream scriptfile
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "
set terminal png font \",8\" size 1400,800
set output \"~A.png\"

set datafile separator ','

set bmargin at screen 0.15

set key left

set xtic rotate by -20

set ylabel \"Runtime [seconds]\"
set log y

set y2label \"Memory allocated [bytes]\"
~:[
unset y2tics # TODO does not work
set y2range [0:1]
~:;
set y2tics
set log y2
~]

set grid

set style fill solid border -1

rgb(r,g,b) = 65536 * int(255 * r) + 256 * int(255 * g) + int(255 * b)
lerp(index) = (index+1)/4.0
color(r, g, b, index) = rgb(lerp(index)*r + (1.0-lerp(index))*.3, \\
                            lerp(index)*g + (1.0-lerp(index))*.3, \\
                            lerp(index)*b + (1.0-lerp(index))*.3)

plot "
            output-filename logy2?)
    (let ((width  (/ .8 (length files-and-columns) 4))
          (colors '((1 0 0) (0 1 0) (0 0 1)
                    (1 1 0) (1 0 1) (0 1 1))))
      (labels ((a-color ()
                 (pop colors))
               (add-column (filename value min max
                            &key
                            tics
                            (offset 0)
                            (width  width)
                            (color  (a-color))
                            (axes   "x1y1")
                            title
                            last?)
                 (format stream "~S using ($0+~F):~2*($~D):(~F):(color(~{~D~^, ~}, 0.0))~@[:xtic(~D)~] ~
                                 with boxes linecolor rgb variable axes ~A notitle~*, \\~%~10:*~
                                 ~S using ($0+~F):($~D)~2*:(~F):(color(~{~D~^, ~}, 1.0))~* ~
                                 with boxes linecolor rgb variable axes ~A notitle~*, \\~%~10:*~
                                 ~S using ($0+~F):~*($~D)~*:(~F):(color(~{~D~^, ~}, 2.0))~* ~
                                 with boxes linecolor rgb variable axes ~A ~:[notitle~:;title ~:*~S~]~@[, \\~]~%"
                         filename offset value min max width color tics axes title (not last?))))
        (loop :for (title file &rest columns) :in files-and-columns
              :for i :from 1 ; TODO
              :for offset :from 0 :by (* 4 width)
              :do (add-column file 2 3 4
                              :tics   (when (= i 1) 1)
                              :offset offset
                              :title  (format nil "~A | Runtime" title))
                  (add-column file 5 6 7
                              :offset (+ offset width)
                              :title  (format nil "~A | Memory" title)
                              :axes   "x1y2")
                  (add-column file 8 9 10
                              :offset (+ offset (* 2 width))
                              :title  (format nil "~A | Code size" title)
                              :axes   "x1y2"
                              :last?  (= i (length files-and-columns))))))))

(defun run-gnuplot (script)
  (run-program "gnuplot" `(,script)
               :output *standard-output*
               :search t
               :wait   t))

(defun plot (results)
  (let ((benchmarks (make-hash-table :test #'equal)))
    (loop :for (results1 &key version) :in results
          :do (dolist (result results1)
                (let ((versions (or (gethash (result-name result) benchmarks)
                                    (setf (gethash (result-name result) benchmarks)
                                          (make-hash-table :test #'equal)))))
                  (push result (gethash version versions '())))))

    (maphash
     (lambda (name versions)
       (format t ";;; Plotting ~A~%" name)

       (let* ((base-name (test-name->filename name))
              (data-files '())
              (non-zero-bytes-consed? nil)
              (script-file (format nil "~A.plt" base-name)))

         (maphash
          (lambda (version results)
            (let ((data-file (format nil "~A-~A.txt" base-name version)))
              (push (list version data-file 1 2 3 4 5 6) data-files)
              (with-open-file (stream data-file
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (flet ((write-measurement (measurement)
                         (format stream ", ~16F, ~16F, ~16F"
                                 (measurement-median measurement)
                                 (measurement-quantile measurement 4 1)
                                 (measurement-quantile measurement 4 3))))
                  (dolist (result results)
                    (format stream "~A"
                            (parameter-result-parameters result))
                    (when (result-runtime result)
                      (write-measurement (result-runtime result)))
                    (when (result-bytes-consed result)
                      (when (plusp (measurement-quantile (result-bytes-consed result) 4 3))
                        (setf non-zero-bytes-consed? t))
                      (write-measurement (result-bytes-consed result)))
                    (when (result-code-size result)
                      (write-measurement (result-code-size result)))
                    (terpri stream))))))
          versions)

         (write-gnuplot-script script-file base-name data-files
                               :logy2? t #+no non-zero-bytes-consed?)
         (run-gnuplot script-file)))
     benchmarks)))
