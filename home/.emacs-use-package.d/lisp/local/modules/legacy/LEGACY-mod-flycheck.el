;;; pkg-flycheck.el --- Flycheck -*- lexical-binding: t -*-

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

;;; DEPRECATED: Legacy unmaintained code. Safe to remove if it causes
;;; regressions.

;;; Code:

(lightemacs-use-package flycheck
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)

  :init
  (setq flycheck-display-errors-delay 0.9)
  (setq flycheck-idle-change-delay 2.0)
  (setq flycheck-yamllintrc "~/.yamllint_global.yml")

  (setq flycheck-check-syntax-automatically
        (if (> (num-processors) 6)
            '(save idle-change new-line mode-enabled)
          '(save mode-enabled)))

  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global (kbd "<leader>a") #'flycheck-next-error)
    (evil-define-key 'normal 'global (kbd "<leader>A") #'flycheck-previous-error))

  :config
  ;; Nice
  (defun my-flycheck-command-wrapper (command)
    (append '("nice" "-n" "19") command))
  (setq flycheck-command-wrapper-function #'my-flycheck-command-wrapper)

  ;; Specify checkers
  (setq flycheck-checkers '(sh-bash
                            sh-posix-bash
                            sh-shellcheck
                            python-pylint
                            python-mypy
                            python-flake8
                            yaml-yamllint
                            json-python-json
                            json-jq
                            lua-luacheck
                            lua))

  ;; ;; Icon
  ;; (define-fringe-bitmap 'flycheck-fringe-bitmap-one-excl
  ;;   (vector #b00000000
  ;;           #b00000000
  ;;           #b00000000
  ;;           #b00110000
  ;;           #b00110000
  ;;           #b00110000
  ;;           #b00110000
  ;;           #b00110000
  ;;           #b00110000
  ;;           #b00110000
  ;;           #b00110000
  ;;           #b00000000
  ;;           #b00110000
  ;;           #b00110000
  ;;           #b00000000
  ;;           #b00000000
  ;;           #b00000000))
  ;; (define-fringe-bitmap 'flycheck-fringe-bitmap-two-excl
  ;;   (vector #b00000000
  ;;           #b00000000
  ;;           #b00000000
  ;;           #b01100110
  ;;           #b01100110
  ;;           #b01100110
  ;;           #b01100110
  ;;           #b01100110
  ;;           #b01100110
  ;;           #b01100110
  ;;           #b01100110
  ;;           #b00000000
  ;;           #b01100110
  ;;           #b01100110
  ;;           #b00000000
  ;;           #b00000000
  ;;           #b00000000))
  ;;
  ;; (flycheck-define-error-level 'error
  ;;   :severity 100
  ;;   :compilation-level 2
  ;;   :overlay-category 'flycheck-error-overlay
  ;;   :fringe-bitmap 'flycheck-fringe-bitmap-two-excl
  ;;   :fringe-face 'flycheck-fringe-error
  ;;   :error-list-face 'flycheck-error-list-error)
  ;; (flycheck-define-error-level 'warning
  ;;   :severity 10
  ;;   :compilation-level 1
  ;;   :overlay-category 'flycheck-warning-overlay
  ;;   :fringe-bitmap 'flycheck-fringe-bitmap-one-excl
  ;;   :fringe-face 'flycheck-fringe-warning
  ;;   :error-list-face 'flycheck-error-list-warning)
  ;; (flycheck-define-error-level 'info
  ;;   :severity -10
  ;;   :compilation-level 0
  ;;   :overlay-category 'flycheck-info-overlay
  ;;   :fringe-bitmap 'flycheck-fringe-bitmap-one-excl
  ;;   :fringe-face 'flycheck-fringe-info
  ;;   :error-list-face 'flycheck-error-list-info)

  ;; BASHATE CHECKER
  ;; https://github.com/alexmurray/flycheck-bashate
  (flycheck-define-checker
   bashate
   "A checker using bashate."
   :command ("bashate" "-i" "E003,E006" source)
   :error-patterns ((error line-start "[E] "(message (minimal-match (one-or-more not-newline))) ": '" (one-or-more not-newline) "'\n"
                           " - " (file-name) " : L" line line-end)
                    (warning line-start "[W] "(message (minimal-match (one-or-more not-newline))) ": '" (one-or-more not-newline) "'\n"
                             " - " (file-name) " : L" line line-end)
                    (error line-start (file-name) ":" line ":" column ":" " E040"
                           (message (minimal-match (one-or-more not-newline)) line-end))
                    (warning line-start (file-name) ":" line ":" column ":" " E"
                             (message (minimal-match (one-or-more not-newline)) line-end)))
   :modes (bash-ts-mode sh-mode))
  (add-to-list 'flycheck-checkers 'bashate)

  ;; https://github.com/flycheck/flycheck/issues/1101
  (add-hook 'flycheck-error-list-mode-hook
            (lambda ()
              (setq tabulated-list-format '[("Line" 15 flycheck-error-list-entry-< :right-align t)
                                            ("Col" 3 nil :right-align t)
                                            ("Level" 8 flycheck-error-list-entry-level-<)
                                            ("ID" 20 t)
                                            (#("Message (Checker)" 0 9
                                               (face default)
                                               9 16
                                               (face flycheck-error-list-checker-name)
                                               16 17
                                               (face default))
                                             0 t)]))))

(provide 'pkg-flycheck)
;;; pkg-flycheck.el ends here
