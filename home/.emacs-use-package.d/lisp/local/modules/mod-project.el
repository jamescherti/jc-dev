;;; mod-project.el --- project -*- lexical-binding: t -*-

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

;;; use-package project

(require 'le-core-paths)
(require 'lightemacs-use-package)
(require 'cl-lib)

(lightemacs-use-package project
  ;; :ensure nil
  :commands (;; project-try-vc
             ;; project-remember-project
             ;; project-current
             ;; project-root
             project--read-project-list
             project-find-file
             project-display-buffer-other-frame
             project-list-buffers
             project-switch-to-buffer
             project-forget-project
             project-recompile
             project-shell-command
             project-kill-buffers
             project-any-command
             project-or-external-find-file
             project-forget-zombie-projects
             project-switch-project
             project-async-shell-command
             project-compile
             project-other-window-command
             project-shell
             project-other-frame-command
             project-query-replace-regexp
             project-find-regexp
             project-other-tab-command
             project-dired
             project-search
             project-eshell
             project-or-external-find-regexp
             project-remember-projects-under
             project-execute-extended-command
             project-display-buffer
             project-forget-projects-under
             project-find-dir
             project-prefix-or-any-command
             project-vc-dir)
  :functions (project--ensure-read-project-list
              project--file-completion-table))

;;; Better project selector

(defun my-project-prompt-project-dir ()
  "Prompt the user to select a directory from the known project roots.
This functions like the built-in `project-prompt-project-dir', but with the
following differences:
- Does not include the `... (choose a dir)` option.
- Automatically reloads the project list.
The selection is limited to projects already listed in the project database; see
`project-list-file'."
  (when (and (fboundp 'project--ensure-read-project-list)
             (fboundp 'project--file-completion-table))
    (project--ensure-read-project-list)
    (let* ((choices
            ;; Just using this for the category (substring completion style).
            (project--file-completion-table project--list))
           (project--dir-history (project-known-project-roots))
           (project-dir ""))
      (while (equal project-dir "")
        ;; If the user simply pressed RET, do this again until they don't.
        (setq project-dir
              (let (history-add-new-input)
                (completing-read "Select project: " choices nil t nil
                                 'project--dir-history))))
      project-dir)))

(setq project-prompter #'my-project-prompt-project-dir)

;;; Project cleanup forward slash and duplicates

(defun my-project--cleanup ()
  "Normalize and deduplicate the global project list."
  ;; Remove trailing '/' from each project path
  (when (and project--list (not (eq project--list 'unset)))
    (setq project--list
          (mapcar (lambda (project)
                    (cond
                     ((listp project)
                      (let ((project (car project)))
                        (if (and project (string-suffix-p "/" project))
                            (list (substring project 0 -1))
                          (list project))))))
                  project--list))

    ;; Remove duplicate projects
    (setq project--list (cl-remove-duplicates project--list :test 'equal))))

;;; Load project list

(defvar my-project-list-file-auto (when (boundp 'lightemacs-var-directory)
                                    (expand-file-name "projects-auto"
                                                      lightemacs-var-directory))
  "Automatically generated project list.")

;; The project list
(setq project-list-file (when (boundp 'lightemacs-var-directory)
                          (expand-file-name "projects" lightemacs-var-directory)))

(defvar my-project-list-file-auto-mtime nil
  "Stores the last known modification time of the file.")

(defvar project-list-file-mtime nil
  "Stores the last known modification time of the file.")

(defun my-project-append-to-project-list (file)
  "Append project list from FILE."
  (when (eq project--list 'unset)
    (setq project--list nil))

  (let ((loaded-project-list nil))
    ;; Load project list
    (let ((project-list-file file)
          (project--list nil))
      (when (fboundp 'project--read-project-list)
        (project--read-project-list))
      (setq loaded-project-list (copy-sequence project--list)))

    (when loaded-project-list
      (setq project--list (append project--list
                                  loaded-project-list))
      project--list)))

(defun my-project--maybe-append (file mtime-var)
  "Append projects from FILE if it exists and has changed.
MTIME-VAR is a symbol storing the last known modification time."
  (when (file-exists-p file)
    (let ((attrs (file-attributes file))
          (mtime (symbol-value mtime-var)))
      (unless (equal (file-attribute-modification-time attrs) mtime)
        (my-project-append-to-project-list file)
        (set mtime-var (file-attribute-modification-time attrs))))))

(defun my-project--project--ensure-read-project-list ()
  "Initialize `project--list' by loading projects."
  (my-project--maybe-append project-list-file 'project-list-file-mtime)
  (my-project--maybe-append my-project-list-file-auto 'my-project-list-file-auto-mtime)
  (my-project--cleanup))

;; (defun my-project--project--ensure-read-project-list ()
;;   "Initialize `project--list' by loading projects."
;;   ;; Project list
;;   (when (and (file-exists-p project-list-file)
;;              (or (not project-list-file-mtime)  ; First time
;;                  (not (equal (file-attribute-modification-time
;;                               (file-attributes project-list-file))
;;                              project-list-file-mtime))))
;;     ;; File: `project-list-file'
;;     (my-project-append-to-project-list project-list-file)
;;     ;; Update modtime of the main file that is not automatically generated
;;     (setq project-list-file-mtime
;;           (file-attribute-modification-time
;;            (file-attributes project-list-file))))
;;
;;   ;; Project list auto
;;   (when (and (file-exists-p my-project-list-file-auto)
;;              (or (not my-project-list-file-auto-mtime)  ; First time
;;                  (and (not (equal (file-attribute-modification-time
;;                                    (file-attributes my-project-list-file-auto))
;;                                   my-project-list-file-auto-mtime)))))
;;     ;; File: `my-project-list-file-auto'
;;     (my-project-append-to-project-list my-project-list-file-auto)
;;     (setq my-project-list-file-auto-mtime
;;           (file-attribute-modification-time
;;            (file-attributes my-project-list-file-auto))))
;;
;;   ;; Cleanup
;;   (my-project--cleanup))

(with-eval-after-load 'project
  (advice-add 'project--ensure-read-project-list
              :override #'my-project--project--ensure-read-project-list))

;;; Project root dir

;;; Project dir / name

(defvar my-project-name-enable-cache t
  "Cache the project name.")

(defun my-project-name ()
  "Return the project name or nil if the project name cannot be found."
  (if (and my-project-name-enable-cache (boundp 'my-project-name-cache))
      my-project-name-cache
    (let ((project-name (let ((project-root (my-project-root-dir)))
                          (when project-root
                            (file-name-nondirectory project-root)))))
      (if project-name
          (progn
            (setq-local my-project-name-cache project-name)
            project-name)
        (setq-local my-project-name-cache nil)))))

;;------------------------------------------
;; Switch to projects
;;------------------------------------------
;;
;; (progn
;;   (defun my-project-switch-project-find-file (dir)
;;     "Switch to another project using (find-file)."
;;     (interactive (list (my-project-prompt-project-dir)))
;;     (find-file dir))
;;
;;   )

;;------------------------------------------
;; Project add (learn)
;;------------------------------------------
;; (defun my-project-display-root-dir ()
;;   "Display the root directory of a project."
;;   (interactive)
;;   (message "%s" (my-project-root-dir)))

;; (defun my-project-add ()
;;   "Find the project root add it to known projects."
;;   (interactive)
;;   (when-let* ((root (my-project-root-dir)))
;;     (project-remember-project (cons 'transient root))
;;     ;; (when root
;;     ;;   ;; Load the list (not the auto one)
;;     ;;   (let ((project--list nil))
;;     ;;     ;; (my-project-append-to-project-list project-list-file)
;;     ;;     ;; TODO use the backup thing
;;     ;;     (message "Add project: %s" root)
;;     ;;     (project-remember-project (cons 'transient root))))
;;     ))
;;
;; (defun my-project-forget ()
;;   "Forget the project at the current buffer's directory."
;;   (interactive)
;;   (let* ((root (my-project-root-dir)))
;;     (when root
;;       (project-forget-project root)
;;       (message "Forgot project: %s" root))))

;;; Project tab group

(defvar-local my-project-change-tab-group-name nil)

(defun my-project-change-tab-group (&rest _args)
  "Change the project in the tab group."
  ;; Tab bar project
  (when my-project-change-tab-group-name
    (let ((buffer-name (buffer-name))
          (my-project-name-enable-cache nil))
      (when (and (not (minibufferp))
                 (fboundp 'tab-bar--current-tab)
                 (fboundp 'tab-bar-change-tab-group))
        (let ((project-name (my-project-name)))
          (when (or (not my-project-change-tab-group-name)
                    (string= my-project-change-tab-group-name ""))
            (when (and (not project-name)
                       (or (or (or (string-prefix-p "*" buffer-name)
                                   (string-prefix-p " *" buffer-name))
                               (string-prefix-p "*" buffer-name))
                           (derived-mode-p 'special-mode)))
              (setq project-name "Special"))

            (unless project-name
              (setq project-name "Misc"))

            (let ((current-group (alist-get 'group (tab-bar--current-tab))))
              (unless (string= current-group project-name)
                (tab-bar-change-tab-group project-name)
                (tab-bar-move-tab-to-group)
                (setq my-project-change-tab-group-name project-name)
                (force-mode-line-update)))))))))

;; void variable project TODO
;;(when my-project-change-tab-group-name
;;  (add-hook 'window-selection-change-functions #'my-project-change-tab-group)
;;  (add-hook 'dired-mode-hook #'my-project-change-tab-group)
;;  (add-hook 'find-file-hook #'my-project-change-tab-group)
;;  (add-hook 'window-buffer-change-functions #'my-project-change-tab-group))

;;; CANCELED: Project dabbrev

;; (defun my-project-dabbrev-friend-buffer (other-buffer)
;;   "Check if OTHER-BUFFER has the same project as the current project."
;;   (and
;;    ;; Same major mode
;;    (eq major-mode
;;        (with-current-buffer other-buffer
;;          major-mode))
;;    ;; Same project name
;;    (let ((project1 (with-current-buffer other-buffer
;;                      (my-project-name)))
;;          (project2 (my-project-name)))
;;      (and project1
;;           project2
;;           (string= project1 project2)))))
;;
;; (setq dabbrev-friend-buffer-function 'my-project-dabbrev-friend-buffer)

;;; Provide

(provide 'mod-project)

;; Local variables:
;; byte-compile-warnings: (not lexical free-vars)
;; End:

;;; mod-project.el ends here
