;;; lightemacs-compile.el --- Description -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; Version: 0.0.9
;; URL: https://github.com/jamescherti/jc-dev
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
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


;;; Code:

;;; Compilation

(require 'bytecomp)

(defun lightemacs-find-el-files (directory)
  "Return a list of all source files in DIRECTORY."
  (let ((regex (concat "\\.el" (regexp-opt load-file-rep-suffixes) "\\'")))
    (directory-files-recursively directory regex)))

(defun lightemacs-get-elc-files (list-el-files)
  "Return a list of existing .elc files for the source files in LIST-EL-FILES."
  (let ((elc-files nil))
    (dolist (source-file list-el-files)
      (let ((elc-file (funcall (if (bound-and-true-p
                                    byte-compile-dest-file-function)
                                   byte-compile-dest-file-function
                                 #'byte-compile-dest-file)
                               source-file)))
        (when (and elc-file (file-regular-p elc-file))
          (push elc-file elc-files))))
    elc-files))

(defun lightemacs-get-eln-files (list-el-files)
  "Return a list of existing .eln files for the source files in LIST-EL-FILES."
  (let ((eln-files nil))
    (dolist (source-file list-el-files)
      (let ((eln-file (when (fboundp 'comp-el-to-eln-filename)
                        (comp-el-to-eln-filename source-file))))
        (when (and eln-file (file-regular-p eln-file))
          (push eln-file eln-files))))
    eln-files))

(defun lightemacs-delete-elc-files (list-el-files)
  "Delete all .elc files associated with the source files in LIST-EL-FILES.
Returns a list of the files that were successfully deleted."
  (let ((elc-files (lightemacs-get-elc-files list-el-files))
        (deleted-files nil))
    (dolist (file elc-files)
      (condition-case nil
          (progn
            ;; (delete-file file)
            (message "DEL: %s" file)
            (push file deleted-files))
        (error nil)))
    deleted-files))

(defun lightemacs-delete-eln-files (list-el-files)
  "Delete all .eln files associated with the source files in LIST-EL-FILES.
Returns a list of the files that were successfully deleted."
  (let ((eln-files (lightemacs-get-eln-files list-el-files))
        (deleted-files nil))
    (dolist (file eln-files)
      (condition-case nil
          (progn
            ;; (delete-file file)
            (message "DEL: %s" file)
            (push file deleted-files))
        (error nil)))
    deleted-files))

(defun lightemacs-get-all-el-files (&optional loaded-only)
  "Get a list of all Lightemacs *.el files, including init and directory files.
If LOADED-ONLY is non-nil, return only the files that are currently loaded."
  (let ((all-el-files nil))
    ;; Collect standard init files
    (when (and user-init-file (file-regular-p user-init-file))
      (push user-init-file all-el-files))

    (let ((early-init (if (boundp 'early-init-file)
                          early-init-file
                        (expand-file-name "early-init.el"
                                          user-emacs-directory))))
      (when (file-regular-p early-init)
        (push early-init all-el-files)))

    ;; Collect files from modules directory
    (when (and (boundp 'lightemacs-builtin-modules-directory)
               (stringp lightemacs-builtin-modules-directory)
               (file-directory-p lightemacs-builtin-modules-directory))
      (setq all-el-files
            (append all-el-files
                    (lightemacs-find-el-files lightemacs-builtin-modules-directory))))

    ;; Collect files from local directory
    (when (and (boundp 'lightemacs-local-lisp-directory)
               (stringp lightemacs-local-lisp-directory)
               (file-directory-p lightemacs-local-lisp-directory))
      (setq all-el-files
            (append all-el-files
                    (lightemacs-find-el-files lightemacs-local-lisp-directory))))

    (if loaded-only
        (let ((loaded-hash (make-hash-table :test 'equal))
              (filtered-files nil))
          (dolist (lh load-history)
            (let ((lh-file (car lh)))
              (when (stringp lh-file)
                (let ((base lh-file))
                  (dolist (suffix load-file-rep-suffixes)
                    (when (and (> (length suffix) 0)
                               (string-suffix-p suffix base))
                      (setq base (substring base 0 (- (length suffix))))))
                  (when (string-match-p "\\.elc?\\'" base)
                    (setq base (file-name-sans-extension base)))
                  (puthash (file-truename base) t loaded-hash)))))
          (dolist (file all-el-files)
            (let ((base file))
              (when (string-match-p "\\.el\\'" base)
                (setq base (file-name-sans-extension base)))
              (when (gethash (file-truename base) loaded-hash)
                (push file filtered-files))))
          (nreverse filtered-files))
      all-el-files)))

(defun lightemacs-compile-all-files ()
  "Byte-compile and native compile Lightemacs core, modules, and init files.
Returns a list of the files that were compiled."
  (interactive)
  (let ((all-el-files (lightemacs-get-all-el-files :loaded-only))
        (compiled-files nil))
    (when all-el-files
      (dolist (file all-el-files)
        (let ((compiled nil))
          ;; Byte-compile
          (condition-case err
              (progn
                (byte-compile-file file)
                (setq compiled t))
            (error
             (message "Byte-compile failed for %s: %s"
                      file (error-message-string err))))

          ;; Native compile
          (when (and (native-comp-available-p)
                     (fboundp 'native-compile))
            (let ((eln-file (when (fboundp 'comp-el-to-eln-filename)
                              (comp-el-to-eln-filename file))))
              (when eln-file
                (condition-case err
                    (progn
                      (native-compile-async file)
                      (setq compiled t))
                  (error
                   (message "Native-compile failed for %s: %s"
                            file (error-message-string err)))))))

          (when compiled
            (push file compiled-files))))
      (message "Finished compiling %d files." (length compiled-files)))
    compiled-files))

;; Byte compilation check
(defun lightemacs--compile-module-maybe (base-path)
  "Byte and natively compile the module at BASE-PATH when needed.
This function verifies whether the source file (.el) is newer than its
corresponding byte-compiled (.elc) or natively compiled (.eln) artifacts.
If the source file is newer, byte compilation is performed synchronously,
and native compilation is dispatched asynchronously.
BASE-PATH is the base path of the module without its file extension."
  (when lightemacs-module-auto-compile
    (let ((el-file (concat base-path ".el")))
      (when (file-exists-p el-file)
        (let* ((elc-file (funcall
                          (if (bound-and-true-p byte-compile-dest-file-function)
                              byte-compile-dest-file-function
                            #'byte-compile-dest-file)
                          el-file))
               (elc-file-exists (file-exists-p elc-file)))
          (when (or (not elc-file-exists)
                    (file-newer-than-file-p el-file elc-file))
            ;; Clean up stale .elc files before recompiling
            (when (and elc-file-exists
                       (file-writable-p elc-file))
              (lightemacs-verbose-message "[DELETE] %s" elc-file)
              (delete-file elc-file))
            (lightemacs-verbose-message "[BYTE COMPILE] %s" el-file)
            (byte-compile-file el-file)))

        ;; Native compilation check (Async)
        (when (and
               ;; Prevent this function from from attempting to spawn async
               ;; compilation processes when it is being executed by a
               ;; background worker or any non-interactive script.
               ;; TODO
               (not noninteractive)
               (not (bound-and-true-p comp-no-spawn))

               ;; Is native compilation available?
               (featurep 'native-compile)
               (fboundp 'native-comp-available-p)
               (native-comp-available-p)
               (fboundp 'comp-el-to-eln-filename)
               (fboundp 'native-compile-async))
          (let* ((eln-file (comp-el-to-eln-filename el-file)))
            (when (or (not (file-exists-p eln-file))
                      (file-newer-than-file-p el-file eln-file))
              ;; Suppress background compiler warnings/errors to avoid popups
              (lightemacs-verbose-message "[ASYNC NATIVE COMPILE] %s" el-file)
              (native-compile-async el-file))))))))

;; (defun lightemacs-delete-all-compiled-files ()
;;   "Delete all Lightemacs core and modules .elc and .eln files."
;;   (interactive)
;;   (let* ((all-el-files (lightemacs-get-all-el-files))
;;          (deleted nil))
;;     (when all-el-files
;;       (setq deleted (append (lightemacs-delete-elc-files all-el-files)
;;                             (lightemacs-delete-eln-files all-el-files)))
;;       (if deleted
;;           (message "Successfully cleaned up %d compiled files." (length deleted))
;;         (message "No compiled files were found to delete.")))
;;     deleted))

(require 'lightemacs-module) ; lightemacs--compile-module-maybe

(cond
 ((eq lightemacs-package-manager 'straight)
  (lightemacs--compile-module-maybe
   (expand-file-name "le-core-pm-straight"
                     lightemacs-builtin-lisp-directory)))
 ((eq lightemacs-package-manager 'elpaca)
  (lightemacs--compile-module-maybe
   (expand-file-name "le-core-pm-elpaca"
                     lightemacs-builtin-lisp-directory)))
 ((eq lightemacs-package-manager 'builtin-package)
  (lightemacs--compile-module-maybe
   (expand-file-name "le-core-pm-use-package"
                     lightemacs-builtin-lisp-directory))))

(let ((init-files '("early-init"
                    "init")))
  (dolist (name init-files)
    ;; Compile Minimal-emacs init.el
    (let ((base-path (expand-file-name name minimal-emacs-user-directory)))
      (lightemacs--compile-module-maybe base-path))

    ;; Compile Lightemacs early-init.el
    (let ((base-path (expand-file-name name lightemacs-user-directory)))
      (lightemacs--compile-module-maybe base-path))))

(let ((core-modules '("le-core-cli-tools"
                      "le-core-defaults"
                      "lightemacs"
                      "lightemacs-module")))
  (dolist (module core-modules)
    (let ((base-path (expand-file-name module lightemacs-builtin-lisp-directory)))
      (lightemacs--compile-module-maybe base-path))))

(defun lightemacs-legacy-module-load (modules)
  "Load all modules listed in MODULES.
If a module fails to load, an error warning is displayed and the module
is not added to the loaded list."
  (when lightemacs-module-auto-compile
    (require 'bytecomp)
    (when (and (featurep 'native-compile)
               (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (require 'comp nil t)))

  (let ((priority-path (cons lightemacs-builtin-modules-directory
                             (cons lightemacs-local-modules-directory
                                   load-path))))
    (dolist (feature-symbol modules)
      (lightemacs-verbose-message "Load module: %s" feature-symbol)

      (unless (featurep feature-symbol)
        (let ((exact-path (locate-library (symbol-name feature-symbol)
                                          nil
                                          priority-path)))
          (if exact-path
              (progn
                ;; Pass base path (e.g. "/path/to/le-modname") so `require'
                ;; searches for .elc first (which also triggers the .eln
                ;; native-compilation swap if available), gracefully falling
                ;; back to .el if uncompiled.
                (let ((base-path (lightemacs--remove-el-file-suffix exact-path)))
                  (lightemacs-debug-message "Load module path: %s (%s)"
                                            feature-symbol
                                            base-path)

                  ;; Load before compiling to prevent race conditions such
                  ;; as: Debugger entered--Lisp error: (file-missing "Cannot
                  ;; open load file" "No such file or directory"
                  ;; "orderless") load("orderless" nil t)
                  (require feature-symbol base-path)))
            (error "Cannot find module '%s'" feature-symbol)))))))

;;; Provide

(provide 'lightemacs-compile)

;;; lightemacs-compile.el ends here
