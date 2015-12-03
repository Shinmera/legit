#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:legit
  (:nicknames #:org.shirakumo.legit)
  (:use #:cl #:simple-inferiors)
  ;; low-level.lisp
  (:export
   #:git-add
   #:git-am
   #:git-apply
   #:git-archive
   #:git-bisect
   #:git-blame
   #:git-branch
   #:git-bundle
   #:git-cat-file
   #:git-checkout
   #:git-cherry-pick
   #:git-clean
   #:git-clone
   #:git-commit
   #:git-commit-tree
   #:git-config
   #:git-count-objects
   #:git-daemon
   #:git-describe
   #:git-diff
   #:git-diff-index
   #:git-fast-import
   #:git-fetch
   #:git-filter-branch
   #:git-for-each-ref
   #:git-format-patch
   #:git-fsck
   #:git-gc
   #:git-grep
   #:git-hash-object
   #:git-help
   #:git-init
   #:git-instaweb
   #:git-log
   #:git-ls-files
   #:git-merge
   #:git-merge-base
   #:git-mergetool
   #:git-mv
   #:git-pull
   #:git-push
   #:git-read-tree
   #:git-rebase
   #:git-reflog
   #:git-remote
   #:git-request-pull
   #:git-reset
   #:git-rev-list
   #:git-rev-parse
   #:git-revert
   #:git-rm
   #:git-send-mail
   #:git-shortlog
   #:git-show
   #:git-show-ref
   #:git-stash
   #:git-status
   #:git-submodule
   #:git-svn
   #:git-symbolic-ref
   #:git-tag
   #:git-update-index
   #:git-update-ref
   #:git-update-server-info
   #:git-verify-pack
   #:git-write-tree)
  ;; process.lisp
  (:export
   #:*git-output*
   #:*git-errors*
   #:*git-input*
   #:run-git)
  ;; repository.lisp
  (:export
   #:repository
   #:init
   #:clone
   #:fetch
   #:pull
   #:checkout
   #:reset
   #:clean
   #:git-value
   #:commits
   #:submodules
   #:map-submodules
   #:do-submodules
   #:remotes
   #:commit-age
   #:current-commit
   #:current-branch
   #:current-message
   #:current-age
   #:remote-url)
  ;; toolkit.lisp
  (:export
   #:location
   #:valid-location-p
   #:relative-dir))
