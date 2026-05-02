;;; tmp-diff-hl.el --- tmp-diff-hl -*- lexical-binding: t -*-

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; URL: https://github.com/jamescherti/lightemacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configures the `diff-hl' package, which highlights uncommitted changes in the
;; window margin, enabling navigation between them. Also known as source control
;; gutter indicators, it displays added, modified, and deleted lines in real
;; time. In Git-controlled buffers, changes can be staged and unstaged directly,
;; providing a clear view of version-control changes without running 'git diff'.
;; By default, the module does not start `diff-hl-mode' automatically.
;;
;; URL: https://github.com/dgutov/diff-hl

;;; Code:

(eval-and-compile
  (require 'lightemacs-use-package))

(lightemacs-use-package diff-hl
  ;; TODO This adds support for indirect buffers:
  ;;      https://github.com/dgutov/diff-hl/pull/276
  :vc (:url "https://github.com/jamescherti/diff-hl"
         :rev :newest)
  :commands (diff-hl-mode
             global-diff-hl-mode)
  :init
  (setq diff-hl-flydiff-delay 0.4)  ; Faster
  (setq diff-hl-show-staged-changes nil)  ; Realtime feedback
  (setq diff-hl-update-async t)  ; Do not block Emacs
  (setq diff-hl-global-modes '(not pdf-view-mode image-mode)))

(provide 'tmp-diff-hl)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; tmp-diff-hl.el ends here
