;;; kirigami-evil.el --- Kirigami Evil keybindings -*- lexical-binding: t; -*-

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
;; A unified method to fold and unfold text.

;;; Code:

;; Kirigami Evil keybindings.

;; (Kirigami offers a unified interface for text folding across a diverse set of
;; major and minor modes in Emacs, including outline-mode, outline-minor-mode,
;; outline-indent-mode, org-mode, markdown-mode, vdiff-mode, vdiff-3way-mode,
;; hs-minor-mode, hide-ifdef-mode, and origami-mode.)
;;

(require 'evil)
(require 'kirigami)

;;;###autoload
(define-minor-mode kirigami-evil-mode
  "Minor mode to override `evil-mode' folding keybindings with Kirigami functions.

When enabled, the standard Evil folding commands (zo, zO, zc, za, zr, zm) are
replaced with Kirigami's versions, providing consistent folding behavior across
all supported modes.

Disabling this mode restores the original Evil keybindings."
  :global t
  :lighter " KirigamiEvil"
  :group 'kirigami
  (if kirigami-evil-mode
      (progn
        (define-key evil-normal-state-map "zo" 'kirigami-open-fold)
        (define-key evil-normal-state-map "zO" 'kirigami-open-fold-rec)
        (define-key evil-normal-state-map "zc" 'kirigami-close-fold)
        (define-key evil-normal-state-map "za" 'kirigami-toggle-fold)
        (define-key evil-normal-state-map "zr" 'kirigami-open-folds)
        (define-key evil-normal-state-map "zm" 'kirigami-close-folds))
    (define-key evil-normal-state-map "zo" 'evil-open-fold)
    (define-key evil-normal-state-map "zO" 'evil-open-fold-rec)
    (define-key evil-normal-state-map "zc" 'evil-close-fold)
    (define-key evil-normal-state-map "za" 'evil-toggle-fold)
    (define-key evil-normal-state-map "zr" 'evil-open-folds)
    (define-key evil-normal-state-map "zm" 'evil-close-folds)))

(provide 'kirigami-evil)
;;; kirigami-evil.el ends here
