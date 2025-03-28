(in-package #:cl-user)
(defpackage #:legit
  (:nicknames #:org.shirakumo.legit)
  (:use #:cl #:simple-inferiors)
  (:shadow #:push)
  ;; re-export
  (:export
   #:with-chdir
   #:location
   #:valid-location-p)
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
   #:git-ls-tree
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
   #:git-send-email
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
   #:git-error
   #:run-git)
  ;; repository.lisp
  (:export
   #:repository
   #:clear-cache
   #:git-location-p
   #:init
   #:clone
   #:fetch
   #:pull
   #:checkout
   #:reset
   #:clean
   #:add
   #:commit
   #:push
   #:git-value
   #:commits
   #:submodules
   #:map-submodules
   #:do-submodules
   #:remotes
   #:commit-age
   #:commit-message
   #:commit-author
   #:current-commit
   #:current-branch
   #:current-message
   #:current-age
   #:remote-url
   #:default-remote
   #:branch-remote
   #:branch-upstream
   #:remote-url
   #:default-remote
   #:bare-p
   #:branches
   #:tags)
  ;; toolkit.lisp
  (:export
   #:relative-dir))
