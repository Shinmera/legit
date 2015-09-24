#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(defclass repository ()
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location NIL))

(defmethod print-object ((repository repository) stream)
  (print-unreadable-object (repository stream :type T)
    (format stream "~s" (uiop:native-namestring (location repository)))))

(defun init (repository &key (if-does-not-exist :error))
  (let ((repository (location repository)))
    (unless (uiop:directory-exists-p
             (relative-dir repository ".git"))
      (ecase if-does-not-exist
        (:error (error "~a is not a GIT repository." repository))
        (:create (git-init :directory repository))
        (:ignore NIL)
        ((NIL) (return-from init NIL))))
    (make-instance 'repository :location repository)))

(defgeneric pull (repository)
  (:method ((repository repository))
    (with-chdir (repository)
      (git-pull))))

(defgeneric current-commit (repository)
  (:method ((repository repository))
    (with-chdir (repository)
      (let ((*git-output* :string))
        (string-trim '(#\Newline) (git-rev-parse "HEAD"))))))
