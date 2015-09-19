#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(defun cmdify (args)
  (with-output-to-string (out)
    (dolist (arg args)
      (when arg
        (etypecase arg
          (symbol (format out "~(~a~)" arg))
          (string (write-string arg out))
          (pathname (format out "~s" (uiop:native-namestring arg)))
          (real (prin1 arg out))
          (list (format out "~{~a~^ ~}" arg)))
        (write-string " " out)))))

(defun run (&rest cmdargs)
  (uiop:run-program (cmdify cmdargs) :output T :error-output T))

(defun purify-args (args)
  (mapcar (lambda (a) (if (listp a) (first a) a)) args))

(defun p-symb (symbol)
  (intern (format NIL "~a-P" symbol)))

(defmacro define-git-wrapper (name &rest argdefs)
  (lambda-fiddle:with-destructured-lambda-list (:required req :key key) argdefs
    (let* ((purereq (purify-args req))
           (purekeys (purify-args key))
           (augkeys (mapcar (lambda (a) `(,a NIL ,(p-symb a))) purekeys))
           (keys ())
           (opts ()))
      (loop for def in key
            do (multiple-value-bind (form opt-p) (parse-argdef def)
                 (if opt-p (push form opts) (push form keys))))
      `(defgeneric ,name (,@purereq &key ,@purekeys)
         (:method (,@req &key ,@augkeys)
           (run
            "git" ,(subseq (string-downcase name) 4)
            ,@(reverse keys)
            ,@purereq
            ,@(reverse opts)))))))

(defun parse-argdef (argdef)
  (destructuring-bind (symbol . options) (if (listp argdef) argdef (list argdef))
    (let ((options (mapcar (lambda (a) (if (listp a) a (list a))) (or options '(:flag)))))
      (cond ((assoc :optional options)
             (values `(format NIL "~{~a~^ ~}" ,symbol) T))
            (T
             (macrolet ((opt (option &rest forms)
                          `(let ((args (assoc ,option options)))
                             (when args
                               ,@forms))))
               `(when ,(p-symb symbol)
                  (etypecase ,symbol
                    ,@(opt :flag
                       `(((not null) ,(format NIL "--~(~a~)" symbol))
                         (T)))
                    ,@(opt :bool
                       `((null ,(format NIL "--no-~(~a~)" symbol))))
                    ,@(opt :member
                       (loop for thing in (cdr args)
                             collect `((eql ,thing) ,(format NIL "--~(~a~)=~(~a~)" symbol thing))))
                    ,@(opt :arg=
                       `((T (format NIL ,(format NIL "--~(~a~)=~~s" symbol) (shellify ,symbol)))))
                    ,@(opt :arg
                       `((T (format NIL ,(format NIL "--~(~a~) ~~s" symbol) (shellify ,symbol)))))
                    ,@(opt :map
                       `((list (loop for (key val) in ,symbol
                                     collect (format NIL ,(format NIL "--~(~a~) ~~s=~~s" symbol) (shellify key) (shellify val))))))
                    ,@(opt :bool
                       (unless (or (assoc :arg options) (assoc :arg= options))
                         `((T ,(format NIL "--~(~a~)" symbol)))))))))))))
