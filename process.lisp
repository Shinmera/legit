#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(defvar *cwd* (uiop:getcwd))
(defmacro with-exchdir ((&optional new-path) &body body)
  (let ((old (gensym "OLD"))
        (new (gensym "NEW")))
    `(let* ((,old (or (ignore-errors (uiop:getcwd))
                      (user-homedir-pathname)))
            (,new (location ,(or new-path '*cwd*))))
       (unwind-protect
            (progn
              (ensure-directories-exist ,new)
              (uiop:chdir ,new)
              ,@body)
         (uiop:chdir ,old)))))

(defmacro with-chdir ((new-path) &body body)
  `(let ((*cwd* ,new-path))
     ,@body))

(defmacro with-resolved-stream ((stream &key args) &body body)
  `(call-with-resolved-stream (lambda (,stream) ,@body) ,stream ,@args))

(defun call-with-resolved-stream (func stream &key args)
  (etypecase stream
    (null
     (funcall func (make-broadcast-stream)))
    (stream
     (funcall func stream))
    (pathname
     (let ((stream (apply #'open stream args))
           (abort T))
       (unwind-protect
            (prog1 (funcall func stream)
              (setf abort NIL))
         (close stream :abort abort))))
    ((eql :string)
     (with-output-to-string (stream)
       (funcall func stream)))
    ((eql T)
     (funcall func *standard-output*))))

(defun copy-stream (input output &key consume-all)
  ;; We copy char by char which is /pretty shit/ performance wise
  ;; but otherwise we would have to either thread or block,
  ;; both of which we /definitely/ do want to avoid.
  (when (open-stream-p input)
    (loop for char = (read-char-no-hang input NIL :eof)
          do (case char
               ((NIL) (unless consume-all (return)))
               (:eof (return))
               (T (write-char char output))))))

(defun stop-process (process &key (attempts 10) (sleep 0.1))
  (external-program:signal-process process :interrupt)
  (loop repeat attempts
        do (sleep sleep)
           (case (external-program:process-status process)
             ((:stopped :exited) (return)))
        finally (external-program:signal-process process :killed)))

(defun ensure-process-stopped (process)
  (when (eq (external-program:process-status process) :running)
    (stop-process process)))

(defun %start-process (program args &rest kargs)
  #+sbcl (apply #'external-program:start program args :directory *cwd* kargs)
  #-sbcl (with-exchdir () (apply #'external-program:start program args kargs)))

(defun run (program args &key input output error (on-non-zero-exit :return))
  (ecase on-non-zero-exit ((NIL :return :error :warn)))
  #+verbose (v:trace :legit "~a~{~^ ~a~}" program args)
  (with-resolved-stream (output)
    (with-resolved-stream (error)
      (let* ((process (%start-process program args :output :stream :error :stream :input input))
             (process-output (external-program:process-output-stream process))
             (process-error (external-program:process-error-stream process)))
        (unwind-protect
             (loop do (copy-stream process-output output)
                      (copy-stream process-error error)
                      ;; Some breathing space
                      #+sbcl (sb-thread:thread-yield)
                   while (eq (external-program:process-status process) :running))
          (ensure-process-stopped process)
          (copy-stream process-output output :consume-all T)
          (copy-stream process-error error :consume-all T)
          (close process-output)
          (close process-error))
        (let ((exit (nth-value 1 (external-program:process-status process))))
          (if (= 0 exit)
              exit
              (case on-non-zero-exit
                ((NIL) NIL)
                (:return exit)
                (:error (error "RUN of ~a ~a exited with ~a." program args exit))
                (:warn (warn "RUN of ~a ~a exited with ~a." program args exit)))))))))

(defvar *git-output* T)
(defvar *git-errors* T)
(defvar *git-input* NIL)
(defun run-git (&rest cmdargs)
  (run
   "git" (loop with list = ()
               for arg in cmdargs
               do (typecase arg
                    (list (dolist (a arg) (push a list)))
                    (T (push arg list)))
               finally (return (nreverse list)))
   :output *git-output*
   :error *git-errors*
   :input *git-input*))
