(asdf:defsystem legit
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
