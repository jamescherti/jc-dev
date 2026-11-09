;;; my-evil-intercept.el --- my config evil -*- lexical-binding: t -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/jc-dev
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.0.9
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Config evil intercept.

;;; Code:

(require 'evil)

(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t
  :group 'my-intercept-mode)

(dolist (state '(normal visual insert))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
   state))

(add-hook 'lightemacs-after-init-hook #'my-intercept-mode)

;;; Provide

(provide 'my-evil-intercept)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; env-allow-syntax-checker-package-lint: nil
;; End:

;;; my-evil-intercept.el ends here
