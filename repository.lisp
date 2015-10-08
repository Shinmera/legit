#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(defclass repository ()
  ((location :initarg :location :accessor location)
   (cache :initform (make-hash-table :test 'equal) :accessor cache))
  (:default-initargs
   :location NIL))

(defmethod print-object ((repository repository) stream)
  (print-unreadable-object (repository stream :type T)
    (format stream "~s" (uiop:native-namestring (location repository)))))

(defun handle-init (action location remote branch)
  (ecase action
    (:error
     (error "~a is not a GIT repository." location))
    (:ignore
     NIL)
    ((:create :init)
     (git-init :directory location)
     (when (string/= branch "master")
       (git-checkout :branch (or branch "master") :orphan T)))
    ((:clone)
     (git-clone (or remote (error "REMOTE required for :CLONE."))
                :directory location
                :branch (or branch "master")))))

(defgeneric clear-cache (repository)
  (:method ((repository repository))
    (clrhash (cache repository))))

(defgeneric init (repository &key &allow-other-keys)
  (:method ((repository pathname) &key (if-does-not-exist :error) remote branch)
    (unless (uiop:directory-exists-p
             (relative-dir repository ".git"))
      (if if-does-not-exist
          (handle-init if-does-not-exist repository remote branch)
          (return-from init NIL)))
    (make-instance 'repository :location repository))
  (:method ((repository repository) &key (if-does-not-exist :error) remote branch)
    (unless (uiop:directory-exists-p
             (relative-dir (location repository) ".git"))
      (if if-does-not-exist
          (handle-init if-does-not-exist (location repository) remote branch)
          (return-from init NIL)))
    repository))

(defgeneric clone (from to &key &allow-other-keys)
  (:method ((from repository) to &key branch)
    (clone (location from) to :branch branch))
  (:method ((from pathname) to &key branch)
    (clone (uiop:native-namestring from) to :branch branch))
  (:method ((from string) (to repository) &key branch)
    (clone from (location to) :branch branch))
  (:method ((from string) (to pathname) &key branch)
    (clone from (uiop:native-namestring to) :branch branch))
  (:method ((from string) (to string) &key branch)
    (git-clone from :directory to :branch branch)))

(defgeneric pull (repository &key &allow-other-keys)
  (:method ((repository repository) &key)
    (with-chdir (repository)
      (git-pull))
    (clear-cache repository)))

(defgeneric checkout (repository thing &key &allow-other-keys)
  (:method ((repository repository) thing &key)
    (with-chdir (repository)
      (git-checkout :tree-ish thing))
    (clear-cache repository)))

(defgeneric reset (repository &key &allow-other-keys)
  (:method ((repository repository) &key to hard mixed soft)
    (with-chdir (repository)
      (git-reset :paths to :hard hard :mixed mixed :soft soft))
    (clear-cache repository)))

(defgeneric clean (repository &key &allow-other-keys)
  (:method ((repository repository) &key directories force ignored)
    (with-chdir (repository)
      (git-clean :directories directories :force force :remove-ignored ignored))))

(defmacro git-value (repository name form)
  `(or (gethash ,name (cache ,repository))
       (setf (gethash ,name (cache ,repository))
             (with-chdir (,repository)
               (let ((*git-output* :string))
                 (string-right-trim '(#\Newline) ,form))))))

(defgeneric commits (repository &key &allow-other-keys)
  (:method ((repository repository) &key)
    (loop with text = (git-value repository `(commits) (git-rev-list :all T))
          with stream = (make-string-input-stream text)
          for line = (read-line stream NIL NIL)
          while line
          when (string/= line "")
          collect line)))

(defgeneric current-commit (repository &key &allow-other-keys)
  (:method ((repository repository) &key short)
    (git-value repository `(commit ,short) (git-rev-parse "HEAD" :short short))))

(defgeneric current-branch (repository &key &allow-other-keys)
  (:method ((repository repository) &key)
    (git-value repository `(branch) (git-rev-parse "HEAD" :abbrev-ref T))))

(defgeneric current-message (repository &key &allow-other-keys)
  (:method ((repository repository) &key)
    (git-value repository `(message) (git-log :pretty "%B" :max-count 1))))

(defgeneric current-age (repository &key &allow-other-keys)
  (:method ((repository repository) &key)
    (unix-to-universal-time
     (parse-integer (git-value repository `(age) (git-log :pretty "%ct" :max-count 1))))))

(defgeneric remote-url (repository &key &allow-other-keys)
  (:method ((repository repository) &key (remote "origin"))
    (git-value repository `(url ,remote) (git-config :name (format NIL "remote.~a.url" remote)))))
