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
      (pathname (prin1-to-string (uiop:native-namestring arg)))
      (real (prin1-to-string arg))
      (list (format NIL "~{~a ~}" (mapcar #'shellify arg))))))

(defun run (&rest cmdargs)
  (uiop:run-program (shellify cmdargs) :output T :error-output T))

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
         (run
          "git" ,(subseq (string-downcase name) 4)
          ,@(loop for arg in req when (front-arg-p arg) collect (parse-rargdef arg))
          ,@(mapcar #'parse-kargdef key)
          ,@(loop for arg in req unless (front-arg-p arg) collect (parse-rargdef arg))
          ,@(mapcar #'parse-oargdef opt))))))

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
     `((T (list "--" ,name))))
    (:member
     (loop for thing in (cdr args)
           collect `((eql ,thing) ,(format NIL "~(~a~)" thing))))
    (:req
     `((T ,name)))))


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
         `((T (format NIL ,(format NIL "~a~a ~~s" prefix name) (shellify ,symbol)))))
        (:arg=
         `((T (format NIL ,(format NIL "~a~a=~~s" prefix name) (shellify ,symbol)))))
        (:arg.
         `((T (format NIL ,(format NIL "~a~a~~a" prefix name) (shellify ,symbol)))))
        (:map
         `((list (loop for (key val) in ,symbol
                       collect (format NIL ,(format NIL "~a~a ~~s~a~~s" prefix name (or (first options) "=")) (shellify key) (shellify val))))))
        (:flag
         (unless (or (assoc :arg options) (assoc :arg= options) (assoc :arg. options))
           `((T))))
        (:bool
         (unless (or (assoc :arg options) (assoc :arg= options) (assoc :arg. options))
           `((T ,(format NIL "~a~a" prefix name))))))))
