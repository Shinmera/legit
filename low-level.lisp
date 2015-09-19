#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(define-git-wrapper git-init
  &optional directory
  &key quiet bare (template :arg=) (separate-git-dir :arg=)
  (shared :arg= (:member :false :true :umask :group :all :world :everybody)))

(define-git-wrapper git-clone
  repository
  &optional directory
  &key local no-hardlinks shared (reference :arg) dissociate quiet verbose progress
  no-checkout bare mirror (origin :arg) (branch :arg) (upload-pack :arg)
  (template :arg=) (config :map) (depth :arg) (single-branch :bool) recursive
  (separate-git-dir :arg=))

(define-git-wrapper git-push
  &optional repository refs
  &key all prune mirror dry-run porcelain delete tags follow-tags
  signed (atomic :bool) (receive-pack :arg=) (force-with-lease :bool :arg=) force
  repo set-upstream (thin :bool) quiet verbose progress
  (recurse-submodules (:member :check :on-demand)) (verify :bool))

(define-git-wrapper git-pull
  &optional repository refspec
  &key quiet verbose (recurse-submodules :bool (:member :yes :on-demand :no))
  (commit :bool) (edit :bool) (ff :bool) ff-only (log :bool :arg=) (stat :bool)
  (squash :bool) (strategy :arg=) (strategy-option :arg=) (verify-signatures :bool)
  (summary :bool) (rebase :bool (:member :false :true :perserve)) all append
  (depth :arg=) unshallow update-shallow force keep no-tags update-head-ok
  (upload-pack :arg) progress)

(define-git-wrapper git-rev-parse
  &key parseopt sq-quote keep-dashdash stop-at-non-option stuck-long revs flags
  (default :arg) (prefix :arg) verify quiet (abbrev-ref (:member :strict :loose))
  (short :arg) symbolic symbolic-full-name all (branches :arg=) (tags :arg=)
  (remotes :arg=) (glob :arg=) (exclude :arg=) (disambiguate :arg=) local-env-vars
  git-dir git-common-dir is-inside-git-dir is-inside-work-tree is-bare-repository
  (resolve-git-dir :arg) (git-path :arg) show-cdup show-prefix show-toplevel
  shared-index-path (since :arg=) (until :arg=))
