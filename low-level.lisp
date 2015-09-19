#|
 This file is a part of legit
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.legit)

(define-git-wrapper git-add
  &optional (paths :--)
  &key dry-run verbose force interactive patch edit update (all :bool)
  (ignore-removal :bool) intent-to-add refresh ignore-errors ignore-missing)

(define-git-wrapper git-am
  &optional maildirs
  &key signoff keep keep-non-patch (keep-cr :bool) (scissors :bool)
  (message-id :bool) quiet (utf8 :bool) 3way ignore-space-change ignore-whitespace
  (whitespace :arg=) (directory :arg=) (exclude :arg=) (include :arg=) reject
  patch-format interactive commiter-date-is-author-date ignore-date skip
  (gpg-sign :arg= :flag) continue resolved (resolvemsg :arg=) abort
  (remove-leading-slashes (:name p) :arg.) (surrounding (:name C) :upcase :arg.))

(define-git-wrapper git-apply
  &optional patch
  &key stat numstat summary check index cached 3way (build-fake-ancestor :arg=)
  recursive reject (machine-readable (:name z) :flag)
  (remove-leading-slashes (:name p) :arg.) (surrounding (:name C) :upcase :arg.)
  unidiff-zero apply no-add binary (exclude :arg=) (include :arg=) ignore-whitespace 
  (whitespace (:member :nowarn :warn :fix :error :error-all)) inaccurate-eof
  verbose recount (directory :arg=) unsafe-paths)

(define-git-wrapper git-archive
  tree-ish
  &optional path
  &key (format :arg=) list verbose (prefix :arg=) (output :arg=) worktree-attributes
  (remote :arg=) (exec :arg=))

(define-git-wrapper git-bisect
  (action (:member :help :start :bad :good :skip :reset :visualize :replay :log :run))
  &optional options (paths :--)
  &key no-checkout)

(define-git-wrapper git-blame
  (file :--)
  &key (blank (:name b) :flag) root show-stats (range (:name L) :upcase (:map ","))
  (long (:name l) :flag) (timestamp (:name t) :flag) (revisions (:name S) :upcase :arg)
  reverse porcelain line-porcelain incremental (encoding :arg=) (contents :arg)
  (date :arg) (moved (:name M) :upcase :arg. :flag) (within-commit (:name C) :upcase :arg. :flag)
  (help (:name h) :flag) (annotate (:name c) :flag) score-debug show-name show-number
  (suppress (:name s) :flag) show-email (ignore-whitespace (:name w) :flag) (abbrev :arg=))

(define-git-wrapper git-branch
  &optional branch start-point old-branch new-branch
  &key delete create-reflog force move (color :arg= :bool) (column :arg :bool)
  remotes all list verbose quiet (abbrev :arg= :bool) (track :bool) set-upstream
  (set-upstream-to :arg=) unset-upstream edit-description (contains :arg)
  (merged :arg) (no-merged :arg))

(define-git-wrapper git-bundle
  (action (:member :create :verify :list-heads :unbundle)) file
  &optional git-rev-list-args refname)

(define-git-wrapper git-cat-file
  object
  &optional type
  &key (show-type (:name t) :flag) (show-size (:name s) :flag) (suppress (:name e) :flag)
  (pretty (:name p) :flag) textconv (batch :flag :arg=) (batch-check :flag :arg=)
  allow-unknown-type follow-symlinks)

(define-git-wrapper git-checkout
  &optional branch new-branch start-point tree-ish commit (paths :--)
  &key quiet force ours theirs (track :bool) detach (orphan :arg)
  ignore-skip-worktree-bits merge (cnoflict :arg=) path
  ignore-other-worktrees)

(define-git-wrapper git-cherry-pick
  &optional commit
  &key edit continue quit abort (append-notice (:name x) :flag) (mainline :arg)
  no-commit signoff (gpg-sign :flag :arg=) ff allow-empty allow-empty-message
  keep-redundant-commits (strategy :arg=) (strategy-option :arg=))

(define-git-wrapper git-clean
  &optional (paths :--)
  &key (directories (:name d) :flag) force interactive dry-run quiet (exclude :arg=)
  (no-ignore (:name x) :flag) (remove-ignored (:name X) :upcase :flag))

(define-git-wrapper git-clone
  repository
  &optional directory
  &key local no-hardlinks shared (reference :arg) dissociate quiet verbose progress
  no-checkout bare mirror (origin :arg) (branch :arg) (upload-pack :arg)
  (template :arg=) (config :map) (depth :arg) (single-branch :bool) recursive
  (separate-git-dir :arg=))

(define-git-wrapper git-commit
  &optional (files :--)
  &key all patch (reuse-message :arg=) (reedit-message :arg=) (fixup :arg=)
  (squash :arg=) reset-author short branch porcelain long null (file :arg=)
  (author :arg=) (date :arg=) (message :arg=) (template :arg=) signoff no-verify
  allow-empty allow-empty-message
  (cleanup (:member :strip :whitespace :verbatim :scissors :default)) (edit :bool)
  amend no-post-rewrite include only (untracked-files :flag :arg=) verbose quiet
  dry-run (status :bool) (gpg-sign :bool :arg=))

(define-git-wrapper git-commit-tree)
(define-git-wrapper git-config)
(define-git-wrapper git-count-objects)
(define-git-wrapper git-daemon)
(define-git-wrapper git-describe)
(define-git-wrapper git-diff)
(define-git-wrapper git-diff-index)
(define-git-wrapper git-fast-import)
(define-git-wrapper git-fetch)
(define-git-wrapper git-filter-branch)
(define-git-wrapper git-for-each-ref)
(define-git-wrapper git-format-patch)
(define-git-wrapper git-fsck)
(define-git-wrapper git-gc)
(define-git-wrapper git-grep)
(define-git-wrapper git-hash-object)
(define-git-wrapper git-help)

(define-git-wrapper git-init
  &optional directory
  &key quiet bare (template :arg=) (separate-git-dir :arg=)
  (shared :arg= (:member :false :true :umask :group :all :world :everybody)))

(define-git-wrapper git-instaweb)

(define-git-wrapper git-log
  &optional revision-range (paths :--)
  &key follow (decorate (:member :short :full :no) :bool) source use-mailmap
  full-diff log-size (range (:name L) :upcase (:map ",")) (max-count :arg=)
  (skip :arg=) (since :arg=) (until :arg=) (author :arg=) (grep-reflog :arg=)
  all-match invert-grep regexp-ignore-case basic-regexp extended-regexp
  fixed-strings perl-regexp remove-empty (merges :bool) (min-parents :arg= :bool)
  (max-parents :arg= :bool) first-parent not all (branches :arg=) (tags :arg=)
  (remotes :arg=) (glob :arg=) (exclude :arg=) reflog ignore-missing bisect
  stdin cherry-mark cherry-pick left-only right-only cherry walk-reflogs merge
  boundary simplify-by-decoration full-history dense sparse simplify-merges
  ancestry-path date-order author-date-order topo-order reverse
  (no-walk :flag (:member :sorted :unsorted)) do-walk relative-date
  (date (:member :relative :local :default :iso :iso-strict :rfc :short :raw))
  parents children left-right graph (show-linear-break :flag :arg=)
  (simultaneous-diff (:name c) :flag) (compressed-simultaneous-diff (:name c) :flag)
  (full-merge-diff (:name m) :flag) (show-recursive-diff (:name r) :flag)
  (show-tree-diff (:name t) :flag))

(define-git-wrapper git-ls-files)
(define-git-wrapper git-merge)
(define-git-wrapper git-merge-base)
(define-git-wrapper git-mergetool)
(define-git-wrapper git-mv)

(define-git-wrapper git-pull
  &optional repository refspec
  &key quiet verbose (recurse-submodules :bool (:member :yes :on-demand :no))
  (commit :bool) (edit :bool) (ff :bool) ff-only (log :bool :arg=) (stat :bool)
  (squash :bool) (strategy :arg=) (strategy-option :arg=) (verify-signatures :bool)
  (summary :bool) (rebase :bool (:member :false :true :perserve)) all append
  (depth :arg=) unshallow update-shallow force keep no-tags update-head-ok
  (upload-pack :arg) progress)

(define-git-wrapper git-push
  &optional repository refs
  &key all prune mirror dry-run porcelain delete tags follow-tags
  signed (atomic :bool) (receive-pack :arg=) (force-with-lease :bool :arg=) force
  repo set-upstream (thin :bool) quiet verbose progress
  (recurse-submodules (:member :check :on-demand)) (verify :bool))

(define-git-wrapper git-read-tree
  &optional tree-ish1 tree-ish2 tree-ish3
  &key (merge (:name m) :flag) reset (update (:name u) :flag)
  (temporary-index (:name i) :flag) dry-run (verbose (:name v) :flag) trivial
  aggressive (prefix :arg=) (exclude-per-directory :arg=) (index-output :arg=)
  no-sparse-checkout empty)

(define-git-wrapper git-rebase
  &optional upstream branch
  &key (onto :arg) continue abort keep-empty skip edito-todo merge (strategy :arg=)
  (strategy-option :arg=) (gpg-sign :arg= :flag) quiet verbose (stat :bool)
  (verify :bool) (context (:name C) :upcase :arg.) force-rebase (fork-point :bool)
  ignore-whitespace (whitespace (:member :nowarn :warn :fix :error :error-all))
  committer-date-is-author-date ignore-date interactive preserve-merges (exec :arg)
  root (autosquash :bool) (autostash :bool) no-ff)

(define-git-wrapper git-reflog
  (action (:member :show :expire :delete))
  &key all (expire :arg=) (expire-unreachable :arg=) updateref rewrite stale-fix
  dry-run verbose follow (decorate (:member :short :full :no) :bool) source use-mailmap
  full-diff log-size (range (:name L) :upcase (:map ",")) (max-count :arg=)
  (skip :arg=) (since :arg=) (until :arg=) (author :arg=) (grep-reflog :arg=)
  all-match invert-grep regexp-ignore-case basic-regexp extended-regexp
  fixed-strings perl-regexp remove-empty (merges :bool) (min-parents :arg= :bool)
  (max-parents :arg= :bool) first-parent not (branches :arg=) (tags :arg=)
  (remotes :arg=) (glob :arg=) (exclude :arg=) reflog ignore-missing bisect
  stdin cherry-mark cherry-pick left-only right-only cherry walk-reflogs merge
  boundary simplify-by-decoration full-history dense sparse simplify-merges
  ancestry-path date-order author-date-order topo-order reverse
  (no-walk :flag (:member :sorted :unsorted)) do-walk relative-date
  (date (:member :relative :local :default :iso :iso-strict :rfc :short :raw))
  parents children left-right graph (show-linear-break :flag :arg=)
  (simultaneous-diff (:name c) :flag) (compressed-simultaneous-diff (:name c) :flag)
  (full-merge-diff (:name m) :flag) (show-recursive-diff (:name r) :flag)
  (show-tree-diff (:name t) :flag))

(define-git-wrapper git-remote
  (action (:member :add :rename :remove :set-head :set-branches :set-url :show :prune :update))
  &optional name url old new branch newurl oldurl group remote
  &key verbose (tags :bool) (mirror (:member :fetch :push)) auto add
  push delete dry-run prune (immediate (:name f) :flag) (track (:name t) :arg)
  (symlink (:name m) :arg) (no-query (:name n) :flag))

(define-git-wrapper git-request-pull
  start url
  &optional end
  &key (patch (:name p) :flag))

(define-git-wrapper git-reset
  &optional tree-ish (paths :--)
  &key soft mixed hard merge keep quiet patch)

(define-git-wrapper git-rev-list
  &key (max-count :arg=) (skip :arg=) (since :arg=) (until :arg=) (max-age :arg=)
  (author :arg=) (grep-reflog :arg=) (grep :arg=) all-match invert-grep
  regexp-ignore-case basic-regexp extended-regexp fixed-strings perl-regexp
  remove-empty (merges :bool) (min-parents :arg= :bool) (max-parents :arg= :bool)
  first-parent all (branches :flag :arg=) (tags :flag :arg=) (remotes :flag :arg=)
  (glob :arg=) (exclude :arg=) reflog ignore-missing stdin quiet cherry-mark
  cherry-pick left-only right-only cherry walk-reflogs merge boundary use-bitmap-index
  simplify-by-decoration full-history dense sparse simplify-merges ancestry-path
  bisect bisect-vars bisect-all date-order author-date-order topo-order reverse
  objects objects-edge objects-edge-aggressive indexed-objects unpacked
  (no-walk (:member :sorted :unsorted) :flag) do-walk relative-date
  (date (:member :relative :local :default :iso :iso-strict :rfc :short :raw))
  header parents children timestamp left-right graph (show-linear-break :arg= :flag)
  count (full-merge-diff (:name m) :flag) (show-recursive-diff (:name r) :flag)
  (show-tree-diff (:name t) :flag))

(define-git-wrapper git-rev-parse
  &key parseopt sq-quote keep-dashdash stop-at-non-option stuck-long revs flags
  (default :arg) (prefix :arg) verify quiet (abbrev-ref (:member :strict :loose))
  (short :arg) symbolic symbolic-full-name all (branches :arg=) (tags :arg=)
  (remotes :arg=) (glob :arg=) (exclude :arg=) (disambiguate :arg=) local-env-vars
  git-dir git-common-dir is-inside-git-dir is-inside-work-tree is-bare-repository
  (resolve-git-dir :arg) (git-path :arg) show-cdup show-prefix show-toplevel
  shared-index-path (since :arg=) (until :arg=))

(define-git-wrapper git-revert)
(define-git-wrapper git-rm)
(define-git-wrapper git-send-email)
(define-git-wrapper git-shortlog)
(define-git-wrapper git-show)
(define-git-wrapper git-show-ref)
(define-git-wrapper git-stash)
(define-git-wrapper git-status)
(define-git-wrapper git-submodule)
(define-git-wrapper git-svn)
(define-git-wrapper git-symbolic-ref)
(define-git-wrapper git-tag)
(define-git-wrapper git-update-index)
(define-git-wrapper git-update-ref)
(define-git-wrapper git-update-server-info)
(define-git-wrapper git-verify-pack)
(define-git-wrapper git-write-tree)
