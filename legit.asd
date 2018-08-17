#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem legit
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "CL interface to the GIT binary."
  :homepage "https://Shinmera.github.io/legit/"
  :bug-tracker "https://github.com/Shinmera/legit/issues"
  :source-control (:git "https://github.com/Shinmera/legit.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "process")
               (:file "low-level")
               (:file "repository")
               (:file "documentation"))
  :depends-on (:uiop
               :simple-inferiors
               :lambda-fiddle
               :cl-ppcre
               :documentation-utils))
