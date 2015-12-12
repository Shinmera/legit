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

(defun handle-init (action location remote branch bare)
  (ecase action
    (:error
     (error "~a is not a GIT repository." location))
    (:ignore
     NIL)
    ((:create :init)
     (ensure-directories-exist location)
     (git-init :directory location :bare bare)
     (when (and branch (string/= branch "master"))
       (git-checkout :branch branch :orphan T)))
    ((:clone)
     (ensure-directories-exist location)
     (git-clone (or remote (error "REMOTE required for :CLONE."))
                :directory location
                :branch branch
                :bare bare))))

(defgeneric clear-cache (repository &optional key)
  (:method ((repository repository) &optional (key NIL k-p))
    (if k-p
        (remhash key (cache repository))
        (clrhash (cache repository)))))

(defun git-location-p (location)
  (= 0 (with-chdir (location)
         (git-rev-parse NIL :git-dir T))))

(defgeneric init (repository &key &allow-other-keys)
  (:method ((repository pathname) &key (if-does-not-exist :error) remote branch bare)
    (unless (git-location-p repository)
      (if if-does-not-exist
          (handle-init if-does-not-exist repository remote branch bare)
          (return-from init NIL)))
    (make-instance 'repository :location repository))
  (:method ((repository repository) &key (if-does-not-exist :error) remote branch bare)
    (unless (git-location-p repository)
      (if if-does-not-exist
          (handle-init if-does-not-exist (location repository) remote branch bare)
          (return-from init NIL)))
    repository))

(defgeneric clone (from to &key &allow-other-keys)
  (:method ((from repository) to &rest args &key)
    (apply #'clone (location from) to args))
  (:method ((from pathname) to &rest args &key)
    (apply #'clone (uiop:native-namestring from) to args))
  (:method ((from string) (to repository) &rest args &key)
    (apply #'clone from (location to) args))
  (:method ((from string) (to pathname) &rest args &key)
    (apply #'clone from (uiop:native-namestring to) args))
  (:method ((from string) (to string) &key branch bare)
    (git-clone from :directory to :branch branch :bare bare)))

(defgeneric fetch (repository &key &allow-other-keys)
  (:method ((repository repository) &key)
    (with-chdir (repository)
      (git-fetch))
    (clear-cache repository)))

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

(defgeneric submodules (repository &key &allow-other-keys)
  (:method ((repository repository) &key recursive only-existing)
    (loop with text = (git-value repository `(submodules ,recursive)
                                 (git-submodule :status :recursive recursive))
          with stream = (make-string-input-stream text)
          for line = (read-line stream NIL NIL)
          for path = (when (and line (string/= line ""))
                       (merge-pathnames (subseq line (1+ (position #\  line :start 1)))
                                        (location repository)))
          while line
          when (and path (or (not only-existing) (valid-location-p path)))
          collect (make-instance 'repository :location path))))

(defgeneric map-submodules (repository function &key &allow-other-keys)
  (:method ((repository repository) function &rest args &key)
    (dolist (submodule (apply #'submodules repository args))
      (funcall function submodule))))

(defmacro do-submodules ((submodule repository &rest args) &body body)
  `(map-submodules ,repository (lambda (,submodule) ,@body) ,@args))

(defgeneric remotes (repository &key &allow-other-keys)
  (:method ((repository repository) &key)
    (remove-duplicates
     (loop with text = (git-value repository `remotes (git-remote NIL :verbose T))
           with stream = (make-string-input-stream text)
           for line = (read-line stream NIL NIL)
           while line
           collect (cl-ppcre:register-groups-bind (name remote) ("^(.*?)\\t(.*?) " line)
                     (cons name remote)))
     :key #'car :test #'string=)))

(defgeneric (setf remotes) (remotes repository &key &allow-other-keys)
  (:method ((new-remotes list) (repository repository) &key)
    (with-chdir (repository)
      (let ((old-remotes (remotes repository)))
        ;; Rename and remove old.
        (loop for (name . url) in old-remotes
              for new = (find name new-remotes :key #'car :test #'string=)
              do (cond (new
                        (when (string/= url (cdr new))
                          (git-remote :set-url :name name :newurl (cdr new) :oldurl url)))
                       (T
                        (git-remote :remove :name name))))
        ;; Add new.
        (loop for (name . url) in new-remotes
              do (unless (find name old-remotes :key #'car :test #'string=)
                   (git-remote :add :name name :url url)))))
    ;; Invalidate cache.
    (clear-cache repository `remotes)
    new-remotes))

(defgeneric commit-age (repository commit &key &allow-other-keys)
  (:method ((repository repository) commit &key)
    (unix-to-universal-time
     (parse-integer (git-value repository `(age ,commit) (git-log :pretty "%ct" :max-count 1 :paths commit))))))

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

(defgeneric bare-p (repository &key &allow-other-keys)
  (:method ((repository repository) &key)
    (string-equal "true" (git-value repository `bare-p (git-rev-parse NIL :is-bare-repository T)))))
