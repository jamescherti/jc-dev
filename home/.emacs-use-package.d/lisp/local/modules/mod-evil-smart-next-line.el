;;; mod-evil-smart-next-line.el --- Evil Smart Next Line -*- lexical-binding: t -*-

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


;;; Code:

;;; Require

(require 'evil)

;;; Main code

;;; Smart previous/next line

(defun evilcursor--get-category-at-point ()
  "Get the category at point."
  (get-text-property (pos-bol) 'category))

(defun evilcursor--outline-invisible-p (pos)
  "Return non-nil when POS is invisible.
POS is the buffer position to check."
  (when (>= pos 1)
    (cond
     ((derived-mode-p 'org-mode)
      (if (fboundp 'org-fold-folded-p)
          (org-fold-folded-p pos)
        (when (fboundp 'org-invisible-p)
          (org-invisible-p pos))))

     ((and (or (bound-and-true-p outline-minor-mode)  ; folded?
               (derived-mode-p 'outline-mode))
           (fboundp 'outline-invisible-p))
      (outline-invisible-p pos))

     (t
      (invisible-p pos)))))

;; TODO should this be part of Emacs ? PATCH
;; Doesn't work when an org mode line contain: *line content*
(defun evilcursor--after-vertical-movement ()
  "Run this after a vertical movement."
  ;; Prevent the command loop from moving the cursor after we place it
  ;; Useless
  ;; (setq disable-point-adjustment t)

  (let ((p (point)))
    (when (and
           ;; Landed on an invisible line
           (evilcursor--outline-invisible-p
            (if (and (eolp) (not (bobp)))
                ;; Without this, invisible-p is nil when eolp
                (1- p)
              p)))
      (vertical-motion 0)
      (goto-char (pos-eol)))))

(defun evilcursor-next-visual-line (count)
  "Move the cursor COUNT screen lines down.
COUNT is the number of lines to move."
  (evil-next-visual-line (or count 1))
  (evilcursor--after-vertical-movement))

(defvar my-cpu-architecture nil
  "The native CPU architecture determined by GCC.")

(defun evilcursor-previous-visual-line (count)
  "Move the cursor COUNT screen lines up.
COUNT is the number of lines to move."
  (let ((line-move-visual t))
    (when (and (numberp temporary-goal-column)
               (< temporary-goal-column 0))
      (setq temporary-goal-column 0))
    (evil-line-move (- (or count 1))))
  (evilcursor--after-vertical-movement))

(defun evilcursor-forward-line (n)
  "Move N lines forward (backward if N is negative).
N is the number of lines to move.
More accurate than `evil-next-line' and `evil-previous-line' when lines are not
truncated."
  (interactive)
  (setq n (or n 1))
  (cond
   ;; ((minibufferp)
   ;;  ;; ignore-errors fixes issues with icomplete
   ;;  (ignore-errors
   ;;    (if (> n 0)
   ;;        (next-line-or-history-element)
   ;;      (previous-line-or-history-element))))

   ;; Not Minibuffer
   (t
    (let* ((count (abs n))
           (forwardp (> n 0))
           (line-number-type (bound-and-true-p display-line-numbers-type))

           ;; (evil-respect-visual-line-mode nil)

           ;; TODO enable?
           ;; (evil-track-eol nil)
           ;; (track-eol nil)
           ;; (line-move-ignore-invisible t)

           (func-change-line (if forwardp
                                 #'evil-next-line
                               #'evil-previous-line))
           (func-change-line-visual (if forwardp
                                        #'evilcursor-next-visual-line
                                      #'evilcursor-previous-visual-line)))
      (cond
       ;; TODO patch embark
       ((eq major-mode 'embark-collect-mode)
        (let ((previous-cat (evilcursor--get-category-at-point))
              (start-point (point)))
          (funcall func-change-line count)
          (let ((current-cat (evilcursor--get-category-at-point)))
            (when (and (not (= start-point (point)))
                       previous-cat
                       current-cat
                       (string= current-cat "embark-collect-group-button")
                       (not (string= previous-cat "embark-collect-group-button")))
              (let ((next-cat (save-excursion
                                (funcall func-change-line count)
                                (evilcursor--get-category-at-point))))
                (when (and next-cat
                           (not (string= next-cat "embark-collect-group-button")))
                  (funcall func-change-line count)))))))

       ((eq line-number-type 'visual)
        (if (and truncate-lines
                 (= count 1))
            ;; This speeds-up scrolling because it does not take into
            ;; consideration visual things
            (progn
              (funcall func-change-line count))
          (funcall func-change-line-visual count))
        )

       ((eq line-number-type 'relative)
        (funcall func-change-line count))

       ((eq line-number-type t)
        ;; TODO doesn't work when count > 1
        (funcall func-change-line count))

       (t
        (message
         "Unsupported evilcursor-smart-next-line/evilcursor-smart-previous-line.")))))))

(evil-define-motion evilcursor-smart-next-line (count)
  "Move smart next line down by COUNT."
  :type line
  (unless count
    (setq count 1))
  (evilcursor-forward-line count))

(evil-define-motion evilcursor-smart-previous-line (count)
  "Move smart previous line up by COUNT."
  :type line
  (unless count
    (setq count 1))
  (evilcursor-forward-line (* count -1)))

(defun my-setup-local-evilcursor-smart-next-prev-line ()
  "Setup smart next/previous line."
  (evil-define-key 'normal 'local
    (kbd "k") #'evilcursor-smart-previous-line
    (kbd "j") #'evilcursor-smart-next-line)
  (evil-define-key 'insert 'local
    (kbd "M-k") #'evilcursor-smart-previous-line
    (kbd "M-j") #'evilcursor-smart-next-line))

(add-hook 'embark-collect-mode-hook #'my-setup-local-evilcursor-smart-next-prev-line)

;; Alternative: SMART
(evil-define-key '(insert visual) 'global
  (kbd "M-k") #'evilcursor-smart-previous-line
  (kbd "M-j") #'evilcursor-smart-next-line)
(evil-define-key 'normal 'global
  (kbd "k") #'evilcursor-smart-previous-line
  (kbd "j") #'evilcursor-smart-next-line)
(evil-define-key 'motion 'global
  (kbd "k") nil
  (kbd "j") nil)

;; (evil-define-key 'insert minibuffer-local-map (kbd "M-k") #'previous-history-element)
;; (evil-define-key 'insert minibuffer-local-map (kbd "M-j") #'next-history-element)
(evil-define-key 'insert evil-eval-map
  (kbd "M-k") #'previous-history-element
  (kbd "M-j") #'next-history-element)
(evil-define-key 'insert evil-ex-completion-map
  (kbd "M-k") #'previous-history-element
  (kbd "M-j") #'next-history-element)
(evil-define-key 'insert evil-ex-search-keymap
  (kbd "M-k") #'previous-history-element
  (kbd "M-j") #'next-history-element)

;;; Provide

(provide 'mod-evil-smart-next-line)

;;; mod-evil-smart-next-line.el ends here
