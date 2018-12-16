#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(defun shellify (arg)
  (when arg
    (etypecase arg
      (symbol (string-downcase arg))
      (string arg)
      (pathname (uiop:native-namestring arg))
      (real (prin1-to-string arg))
      (list (format NIL "~{~@[~a ~]~}" (mapcar #'shellify arg))))))

(defun ensure-list (a)
  (if (listp a) a (list a)))

(defun purify-args (args)
  (mapcar (lambda (a) (if (listp a) (first a) a)) args))

(defun p-symb (symbol)
  (let ((*print-case* #.(readtable-case *readtable*)))
    (intern (format NIL "~a-~a" symbol 'p))))

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

(defmacro %opt (options &rest forms)
  `(let ((args (or ,@(loop for option in (if (listp options) options (list options))
                           collect `(assoc ,option options)))))
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
           collect `((eql ,thing) ,(when thing (format NIL "~(~a~)" thing)))))
    (:req
     `((T ,symbol)))))


(define-argparser parse-oargdef (:opt symbol prefix name options)
  `(when ,(p-symb symbol)
     ,(argetypecase symbol
        (:--
         `((T (when ,symbol (list "--" ,symbol)))))
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
         `((T ,(format NIL "~a~a" prefix name))))
        ((:arg :arg= :arg.)
         (unless (or (assoc :flag options) (assoc :bool options))
           `((null)))))))

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

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))
