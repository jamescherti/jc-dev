;;; evil-quick-fasd.el --- using fasd right from evil-ex  -*- lexical-binding: t; -*-

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

;;  Invoke fasd functionality right from `evil-ex'

;;; Code:

(require 'evil)
(require 'quick-fasd)

(defvar evil-quick-fasd-prefix ":"
  "Prefix for `evil-ex' command that will invoke fasd.")

(defun evil-quick-fasd-eval (orig-fun str)
  "Advice for `evil-ex-execute'.
ORIG-FUN is `evil-ex-execute', STR is the command input."
  (if (not (cond
            ((string-prefix-p evil-quick-fasd-prefix str)
             (funcall #'quick-fasd-find-path
                      (string-remove-prefix evil-quick-fasd-prefix str)))))
      (funcall orig-fun str)))

(advice-add #'evil-ex-execute :around 'evil-quick-fasd-eval)

;;;###autoload
(defun evil-quick-fasd ()
  "Invoke `evil-ex' with `evil-quick-fasd-prefix'."
  (interactive)
  (evil-ex evil-quick-fasd-prefix))

(provide 'evil-quick-fasd)
;;; evil-quick-fasd.el ends here
