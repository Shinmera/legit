#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(defvar *git-output* T)
(defvar *git-errors* T)
(defvar *git-input* NIL)
(defun run-git (&rest cmdargs)
  (run
   "git" (loop with list = ()
               for arg in cmdargs
               do (typecase arg
                    (list (dolist (a arg) (cl:push a list)))
                    (T (cl:push arg list)))
               finally (return (nreverse list)))
   :output *git-output*
   :error *git-errors*
   :input *git-input*))
