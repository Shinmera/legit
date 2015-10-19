#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem legit
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "CL interface to the GIT binary."
  :homepage "https://github.com/Shinmera/legit"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "process")
               (:file "low-level")
               (:file "repository"))
  :depends-on (:uiop
               :external-program
               :lambda-fiddle
               :bordeaux-threads))
