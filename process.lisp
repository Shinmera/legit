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
  `(let ((*cwd* (location ,new-path)))
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

(defun copy-stream (input output &key consume-all (buffer 64))
  ;; Ok, this is kludgy. Let's see.
  ;; In order to avoid having to spawn threads to read the
  ;; two streams simultaneously, we have to somehow read only
  ;; as much as is available and then return, to let the
  ;; other stream be read. As such, READ-SEQUENCE by itself
  ;; is not a possible choice as it might well block until the
  ;; end of the program. The only other choice is to use
  ;; READ-CHAR-NO-HANG, which is impossibly inefficient and
  ;; will eat all of the resources. So we opt for a compromise
  ;; in which we check if new input is there by
  ;; READ-CHAR-NO-HANG and then use one of several methods to
  ;; read more or less blocking and more or less efficiently
  ;; from the stream.
  (when (open-stream-p input)
    (let ((char (read-char-no-hang input NIL)))
      (when char
        (write-char char output)
        (etypecase buffer
          ((eql :line)
           (if consume-all
               (loop for line = (read-line input NIL)
                     while line
                     do (write-line line output))
               (write-line (read-line input) output)))
          ((eql :character)
           (if consume-all
               (loop for char = (read-char input NIL)
                     while char
                     do (write-char char output))
               (loop for char = (read-char-no-hang input NIL)
                     while char
                     do (write-char char output))))
          (integer
           (let ((buf (make-array buffer :element-type 'character)))
             (if consume-all
                 (loop for size = (read-sequence buf input)
                       while (< 0 size) do (write-sequence buf output :end size))
                 (write-sequence buf output :end (read-sequence buf input))))))
        (finish-output output)))))

(defun handle-process-sequential (copier process out-in out-out err-in err-out &key (cooldown 0.05))
  (unwind-protect
       (loop do (funcall copier out-in out-out)
                (funcall copier err-in err-out)
                (sleep cooldown)
             while (eq (external-program:process-status process) :running))
    (ensure-process-stopped process)
    (funcall copier out-in out-out :consume-all T)
    (funcall copier err-in err-out :consume-all T)))

(defun handle-process-parallel (copier process out-in out-out err-in err-out)
  (let ((err-thread (bt:make-thread (lambda () (funcall copier err-in err-out :consume-all T))))
        (out-thread (bt:make-thread (lambda () (funcall copier out-in out-out :consume-all T)))))
    (unwind-protect
         (loop while (eq (external-program:process-status process) :running)
               do (sleep 0.1))
      (ensure-process-stopped process)
      (bt:join-thread err-thread)
      (bt:join-thread out-thread))))

(defun make-copier (buffer)
  (lambda (in out &rest args)
    (apply #'copy-stream in out :buffer buffer args)))

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

(defun run (program args &key input output error (on-non-zero-exit :return) (handler #'handle-process-sequential) (copier (make-copier :line)))
  (ecase on-non-zero-exit ((NIL :return :error :warn)))
  #+verbose (v:trace :legit "~a~{~^ ~a~}" program args)
  (with-resolved-stream (output)
    (with-resolved-stream (error)
      (let* ((process (%start-process program args :output :stream :error :stream :input input))
             (out-in (external-program:process-output-stream process))
             (err-in (external-program:process-error-stream process)))
        (unwind-protect
             (funcall handler copier process out-in output err-in error)
          (close out-in)
          (close err-in))
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
