(defvar *no-color*
  (member "--no-color" *posix-argv* :test #'equal))

(defvar *color-error* nil)

(unless *no-color*
  (let ((file #-win32 "util/colorize-control-codes.lisp"
              #+win32 "util/colorize-windows-console.lisp"))
    (handler-case (load file)
      (error (c)
        (setf *color-error*
              (format nil "Error while loading ~a:~% ~a"
                      (enough-namestring file)
                      c))))))

(defun is-tty (&optional (stream *standard-output*))
  (let* ((stream (sb-impl::stream-output-stream stream))
         (fd (and (sb-sys:fd-stream-p stream)
                  (sb-sys:fd-stream-fd stream))))
    (when (integerp fd)
      (plusp (sb-unix:unix-isatty fd)))))

(defun present-coloring-error (error &optional (stream *standard-output*))
  (format stream "~a~%" error)
  (format stream "Switching off colored output,~%~
                    it can be turned off by passing --no-color~%~%")
  (setf *no-color* t))

(defun output-colored-text (kind text
                            &key (align 20) (stream *standard-output*))
  (cond ((or (not (is-tty))
             *no-color*)
         (write-string text))
        (*color-error*
         (present-coloring-error *color-error*)
         (write-string text stream))
        (t
         (handler-case
             (case kind
               ((:unexpected-failure
                 :leftover-thread
                 :unhandled-error
                 :invalid-exit-status)
                (%output-colored-text text :red :bold t))
               ((:unexpected-success)
                (%output-colored-text text :green))
               (t
                (write-string text stream)))
           (error (c)
             (present-coloring-error
              (format nil "Error while printing colored text:~% ~a"
                      c)
              stream)
             (write-string text stream)))))
  (write-string (make-string (max 0 (- align (length text)))
                             :initial-element #\Space)
                stream))
