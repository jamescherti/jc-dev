;;; mod-kirigami.el --- Better Folds -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides enhanced folding and unfolding capabilities in Emacs.
;; It offers a more intuitive commands to manipulate code and text folds.

;;; Code:

(require 'lightemacs-use-package)

;; Important for kirigami jump to load
(require 'kirigami)

;; (defun my-setup-outline-minor-mode-kirigami ()
;;   "Close all folds."
;;   (let ((buffer-name (buffer-name)))
;;     ;; Buffers such as *vc-diff*
;;     (unless (string-prefix-p "*vc-" buffer-name)
;;       (ignore-errors
;;         (unwind-protect
;;             (kirigami-close-folds)
;;           (kirigami-open-fold))))))
;;
;; (add-hook 'outline-minor-mode-hook #'my-setup-outline-minor-mode-kirigami)

(defun mod-kirigami--spc ()
  (interactive)
  (condition-case nil
      (kirigami-open-fold)
    (error
     nil)))

(with-eval-after-load 'evil
  (when (bound-and-true-p evil-normal-state-map)
    (define-key evil-normal-state-map (kbd "SPC") 'mod-kirigami--spc)))

;; TODO Find a way to add this fix to kirigami??
;; Adjust the window to ensure the current heading remains visible. This fixes
;; a bug where not the whole heading is displayed, with makes outline only
;; display an allipsis at window-start
;; (defun kirigami--outline-run-on-window-change (&optional object)
;;   "Ensure outline headings remain visible when windows display new buffers.
;; OBJECT can be a window or a frame."
;;   (when (fboundp 'kirigami--outline-ensure-window-start-heading-visible)
;;     (dolist (window (cond
;;                      ((windowp object)
;;                       (list object))
;;
;;                      ((framep object)
;;                       (window-list object))
;;
;;                      ((not object)
;;                       (list (selected-window)))))
;;       (kirigami--outline-ensure-window-start-heading-visible window))))

;; Good implementation but risky according to gemini when immediately is t
(defvar kirigami--outline-fix-window-start-immediately nil)

;; TODO store windows is a list and run-with-timer should act on them
(defun mod-kirigami--outline-ensure-visible (object)
  "Ensure outline headings remain visible when windows display new buffers.
OBJECT can be nil (current window), a window, or a frame."
  (when (fboundp 'kirigami--outline-ensure-window-start-heading-visible)
    (dolist (window (cond
                     ((windowp object)
                      (list object))

                     ((framep object)
                      (window-list object))

                     ((not object)
                      (list (selected-window)))

                     ((listp object)
                      object)

                     (t
                      (error
                       "OBJECT needs to be a window, a frame, nil, or a list of windows"))))
      (when (window-live-p window)
        (with-selected-window window
          (when (or (derived-mode-p 'outline-mode)
                    (bound-and-true-p outline-minor-mode))
            (if kirigami--outline-fix-window-start-immediately
                (kirigami--outline-ensure-window-start-heading-visible)
              ;; SAFETY: Defer the adjustment to the next event loop cycle.
              ;; This prevents 'set-window-start' from clashing with the
              ;; active redisplay.
              (run-with-timer
               0 nil
               #'kirigami--outline-ensure-window-start-heading-visible))))))))

(add-hook 'window-buffer-change-functions
          #'mod-kirigami--outline-ensure-visible)

;; Useless when loading because window-start does not change
;; (defun kirigami--outline-enable-immediate-fix ()
;;   "Enable the immediate window start fix for the current buffer."
;;   ;; Add locally so it only runs on relevant buffers
;;   (when (or (derived-mode-p 'outline-mode)
;;             (bound-and-true-p outline-minor-mode))
;;     (add-hook 'window-scroll-functions
;;               #'(lambda(window _display-start &rest _args)
;;                   (let ((kirigami--outline-fix-window-start-immediately t))
;;                     (mod-kirigami--outline-ensure-visible window)))
;;               nil  ; depth
;;               t))) ; local
;; (add-hook 'outline-minor-mode-hook #'kirigami--outline-enable-immediate-fix)
;; (add-hook 'outline-mode-hook #'kirigami--outline-enable-immediate-fix)

(with-eval-after-load 'kirigami
  (require 'kirigami-jump)
  (when (fboundp 'kirigami-jump-mode)
    (kirigami-jump-mode))


  (with-eval-after-load 'evil
    (require 'kirigami-evil)
    (when (fboundp 'kirigami-evil-mode)
      (kirigami-evil-mode))))

;; (with-eval-after-load 'evil
;;   (require 'kirigami-evil)
;;   (kirigami-evil-mode))

;;   (defcustom kirigami-unfold-on-jump nil
;;     "Reveal folded content whenever point moves into a hidden section.
;; Integrates Kirigami with native and third-party jump mechanisms so navigation
;; does not land inside concealed text. Adds hooks and advices for components such
;; as `xref', `imenu', `consult', `save-place', `flymake', `evil' jumps,
;; `bookmarks', `grep', and `org-agenda'."
;;     :type 'boolean
;;     :group 'kirigami)


;;; kirigami settings

;; (setq pulse-delay 0.04)

;; (defun test/kirigami-pre-hook (_action)
;;   "Test pre-hook that logs the ACTION.
;; Must return non-nil so `run-hook-with-args-until-failure` does not stop."
;;   ;; (message "[2] PRE-HOOK: Validating state for %S" action)
;;   t
;;   )
;;
;; (defun test/kirigami-post-hook (_action)
;;   "Test post-hook that logs the completion of ACTION."
;;   ;; (message "[3] POST-HOOK: Finalized operation %S" action)
;;   t
;;   )
;;
;; ;; Registration
;; (add-hook 'kirigami-pre-action-predicates #'test/kirigami-pre-hook)
;; (add-hook 'kirigami-post-action-functions #'test/kirigami-post-hook)

;; -----------------------------------------------
;; PULSE
;; -----------------------------------------------
(defun mod-kirigami-pulse (_action)
  "Visually emphasize the current line after a Kirigami folding operation.
ACTION identifies the completed folding operation and is ignored by
this function. The function produces a brief, transient highlight on
the line at point, providing visual feedback that the folding change
has finished."
  (pulse-momentary-highlight-one-line))

(add-hook 'kirigami-post-action-functions #'mod-kirigami-pulse)


(provide 'mod-kirigami)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-kirigami.el ends here
