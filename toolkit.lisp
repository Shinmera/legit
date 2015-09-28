#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(defun ensure-list (a)
  (if (listp a) a (list a)))

(defun shellify (arg)
  (when arg
    (etypecase arg
      (symbol (string-downcase arg))
      (string arg)
      (pathname (uiop:native-namestring arg))
      (real (prin1-to-string arg))
      (list (format NIL "~{~@[~a ~]~}" (mapcar #'shellify arg))))))

(defvar *git-output* T)
(defvar *git-errors* T)
(defvar *git-input* NIL)

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

(defun run (program args &key input output error)
  (format T "~&> ~a ~{~a~^ ~}~&" program args)
  (with-resolved-stream (output)
    (with-resolved-stream (error)
      (let* ((process (external-program:start program args :output :stream :error :stream :input input))
             (process-output (external-program:process-output-stream process))
             (process-error (external-program:process-error-stream process)))
        (unwind-protect
             (loop do (copy-stream process-output output)
                      (copy-stream process-error error)
                   while (eq (external-program:process-status process) :running))
          (ensure-process-stopped process)
          (copy-stream process-output output :consume-all T)
          (copy-stream process-error error :consume-all T)
          (close process-output)
          (close process-error))
        (nth-value 1 (external-program:process-status process))))))

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

(defun purify-args (args)
  (mapcar (lambda (a) (if (listp a) (first a) a)) args))

(defun p-symb (symbol)
  (intern (format NIL "~a-P" symbol)))

(defun front-arg-p (arg)
  (and (listp arg) (find :front arg)))

(defmacro define-git-wrapper (name &rest argdefs)
  (lambda-fiddle:with-destructured-lambda-list (:required req :optional opt :key key) argdefs
    (let* ((purereq (purify-args req))
           (purekey (purify-args key))
           (pureopt (purify-args opt))
           (augkeys (mapcar (lambda (a) `(,a NIL ,(p-symb a))) (append pureopt purekey))))
      `(defun ,name (,@purereq &key ,@augkeys)
         (declare (ignorable ,@(mapcar #'third augkeys)))
         (run-git
          ,(subseq (string-downcase name) 4)
          ,@(loop for arg in req when (front-arg-p arg) collect (parse-rargdef arg))
          ,@(loop for arg in opt when (front-arg-p arg) collect (parse-oargdef arg))
          ,@(mapcar #'parse-kargdef key)
          ,@(loop for arg in req unless (front-arg-p arg) collect (parse-rargdef arg))
          ,@(loop for arg in opt unless (front-arg-p arg) collect (parse-oargdef arg)))))))

(defmacro %opt (option &rest forms)
  `(let ((args (assoc ,option options)))
     (when args
       ,@forms)))

(defmacro argetypecase (symb &body options)
  `(append (list 'etypecase ,symb)
           ,@(loop for (name . forms) in options
                   collect `(%opt ,name ,@forms))))

(defmacro define-argparser (funcname (default symbol prefix name options) &body body)
  (let ((argdef (gensym "ARGDEF")))
    `(defun ,funcname (,argdef)
       (destructuring-bind (,symbol . ,options) (ensure-list ,argdef)
         (declare (ignorable ,symbol))
         (let* ((,options (mapcar #'ensure-list (or ,options '(,default))))
                (,name (if (assoc :name ,options) (second (assoc :name ,options)) ,symbol))
                (,name (if (assoc :upcase ,options) (string-upcase ,name) (string-downcase ,name)))
                (,prefix (if (= (length ,name) 1) "-" "--")))
           (declare (ignorable ,prefix ,name ,options))
           ,@body)))))

(define-argparser parse-rargdef (:req symbol prefix name options)
  (argetypecase symbol
    (:--
     `((T (list "--" ,symbol))))
    (:member
     (loop for thing in (cdr args)
           collect `((eql ,thing) ,(format NIL "~(~a~)" thing))))
    (:req
     `((T ,symbol)))))


(define-argparser parse-oargdef (:opt symbol prefix name options)
  `(when ,(p-symb symbol)
     ,(argetypecase symbol
        (:--
         `((T (list "--" ,symbol))))
        (:member
         (loop for thing in (cdr args)
               collect `((eql ,thing) ,thing)))
        (:opt
         `((T ,symbol))))))

(define-argparser parse-kargdef (:flag symbol prefix name options)
  `(when ,(p-symb symbol)
     ,(argetypecase symbol
        (:flag
         `(((eql T) ,(format NIL "~a~a" prefix name))))
        (:bool
         `((null ,(format NIL "--no-~a" name))))
        (:member
         (loop for thing in (cdr args)
               collect `((eql ,thing) ,(format NIL "~a~a=~(~a~)" prefix name thing))))
        (:arg
         `(((not null) (list ,(format NIL "~a~a" prefix name) (shellify ,symbol)))))
        (:arg=
         `(((not null) (format NIL ,(format NIL "~a~a=~~a" prefix name) (shellify ,symbol)))))
        (:arg.
         `(((not null) (format NIL ,(format NIL "~a~a~~a" prefix name) (shellify ,symbol)))))
        (:map
         `((list (loop for (key val) in ,symbol
                       collect (format NIL ,(format NIL "~a~a ~~a~a~~a" prefix name (or (first options) "=")) (shellify key) (shellify val))))))
        (:flag
         `((T)))
        (:bool
         `((T ,(format NIL "~a~a" prefix name)))))))

(defgeneric location (thing)
  (:method ((pathname pathname))
    pathname)
  (:method ((string string))
    (uiop:parse-native-namestring string)))

(defmacro with-chdir ((new-path) &body body)
  (let ((old (gensym "OLD"))
        (new (gensym "NEW")))
    `(let ((,old (uiop:getcwd))
           (,new (location ,new-path)))
       (unwind-protect
            (progn
              (ensure-directories-exist ,new)
              (uiop:chdir ,new)
              ,@body)
         (uiop:chdir ,old)))))

(defun minimal-shell-namestring (pathname)
  (uiop:native-namestring
   (uiop:enough-pathname
    pathname (uiop:getcwd))))

(defun relative-dir (relative &rest subdirs)
  (loop for sub in subdirs
        for dir = (merge-pathnames (uiop:ensure-directory-pathname sub)
                                   (uiop:ensure-directory-pathname relative))
        then (merge-pathnames (uiop:ensure-directory-pathname sub) dir)
        finally (return dir)))
