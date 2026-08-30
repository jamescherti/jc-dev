;;; mod-dired.el --- mod-dired -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'seq)

;;; Dired defaults

;; Allow drag and drop out of dired into other apps (e.g. browser)
(setq dired-mouse-drag-files t)

(setq dired-movement-style 'bounded-files)

;; Reuses a single buffer for Dired navigation instead of opening a new buffer
;; for every directory. This keeps your buffer list clean and prevents Dired
;; buffer proliferation.
;;
;; NOTE: Disabled because too buffers in other tabs are killed.
(setq dired-kill-when-opening-new-dired-buffer t)

(setq dired-hide-details-hide-symlink-targets nil)

;; setq native-comp-async-on-battery-power nil) is an excellent default, for
;; users running Emacs on laptops. Background native compilation (via gccemacs)
;; is a highly CPU-intensive task. When packages are installed or updated,
;; spawning multiple asynchronous compiler processes on battery power can cause
;; rapid battery drain and thermal throttling. Suspending this behavior until
;; the machine is connected to AC power is a sensible optimization.
;;
;; NOTE: Issue. This stops native compilation, even when the laptop is charging.
;; (setq native-comp-async-on-battery-power nil)

;; Asks for confirmation before creating missing parent directories during file
;; copy or rename operations. This protects against creating unintended
;; directories due to typos while remaining convenient.
;; (setq dired-create-destination-dirs 'ask)

;; Automatically creates destination directories without asking if the
;; destination path ends with a trailing slash.
;; (setq dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29

;; Allows wdired to automatically create missing parent directories when you
;; rename files to paths that do not exist yet. This makes bulk project
;; restructuring incredibly fast.
;; (setq wdired-create-parent-directories t)

;; Automatically kills the buffers of files that you delete or rename within
;; Dired. This prevents you from accidentally interacting with stale buffers
;; that no longer correspond to the filesystem.
;; (setq dired-clean-up-buffers-too t)

;; TODO: add to minimal-emacs.d?
;; Doesn't work
;; (setq dired-create-destination-dirs-on-trailing-dirsep t)

;; Tell dired-x to not bind "I" key to `dired-info' or "N" to `dired-man'
;; (setq dired-bind-info nil)
;; (setq dired-bind-man nil)

;;; Using xdg-open/open/start for certain filetypes

;; No confirmation

;; (defun my-dired-do-shell-command-around (orig-fun &rest args)
;;   "Bypass the minibuffer prompt for `dired-do-shell-command'.
;; Uses `my-dired-xdg-open-cmd' automatically unless a prefix argument is provided."
;;   (interactive
;;    (when (and (fboundp 'dired-get-marked-files)
;;               (fboundp 'dired-read-shell-command))
;;      (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
;;        (if current-prefix-arg
;;            ;; Restore original behavior if prefix arg is passed (e.g., C-u !)
;;            (list (dired-read-shell-command "! on %s: " current-prefix-arg files)
;;                  current-prefix-arg
;;                  files)
;;          ;; Bypass prompt and supply the predefined command
;;          (list my-dired-xdg-open-cmd
;;                current-prefix-arg
;;                files)))))
;;   (apply orig-fun args))
;;
;; (advice-add 'dired-do-shell-command :around #'my-dired-do-shell-command-around)

(setq dired-confirm-shell-command t)

(defvar my-dired-xdg-open-cmd nil)

(with-eval-after-load 'dired
  (when-let* ((cmd (cond
                    ((eq system-type 'darwin)
                     "open")
                    ((memq system-type '(gnu gnu/linux gnu/kfreebsd
                                             berkeley-unix))
                     "xdg-open")
                    ((memq system-type '(cygwin windows-nt ms-dos))
                     "start"))))
    (setq dired-guess-shell-alist-user
          `((".*" ,cmd)))
    (when cmd
      (setq my-dired-xdg-open-cmd cmd))))

(with-eval-after-load 'dired
  (when-let* ((cmd (cond
                    ((eq system-type 'darwin)
                     "open")
                    ((memq system-type '(gnu gnu/linux gnu/kfreebsd
                                             berkeley-unix))
                     "xdg-open")
                    ((memq system-type '(cygwin windows-nt ms-dos))
                     "start"))))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|odt\\|odg\\|ods\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|webp\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|m4a\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)))
    (when cmd
      (setq my-dired-xdg-open-cmd cmd))))

;;; Functions

(defun my-dired-home ()
  "Dired home."
  (interactive)
  (dired "~/"))

;;; DISABLED: Open dired with external command

;; (defun my-dired-get-file-open-command (file-path)
;;   "Return FILE-PATH corresponding command from `dired-guess-shell-alist-user'."
;;   (let* ((case-fold-search nil)
;;          (result (seq-find (lambda (pattern)
;;                              (string-match-p (car pattern) file-path))
;;                            dired-guess-shell-alist-user)))
;;     (when result
;;       (car (cdr result)))))
;;
;; (defun my-dired-open-with-external-command ()
;;   "Open the current file in `dired' using an external command based on file type."
;;   (interactive nil dired-mode)
;;   (if (and (fboundp 'dired-get-file-for-visit)
;;            (fboundp 'dired--find-possibly-alternative-file))
;;       (let* ((file (dired-get-file-for-visit)))
;;         (let ((shell-cmd (my-dired-get-file-open-command file)))
;;           (if shell-cmd
;;               (progn
;;                 ;; (message "[RUN] %s %s" shell-cmd file)
;;                 (if (fboundp 'quick-fasd-add-path)
;;                     (quick-fasd-add-path file)
;;                   (message "Warning: Undefined: `quick-fasd-add-path'"))
;;                 (call-process shell-cmd nil nil nil file))
;;             (condition-case err
;;                 (dired--find-possibly-alternative-file file)
;;               (error
;;                (message "Container parsing failed (%s). Opening literally."
;;                         (error-message-string err))
;;                (find-file-literally file))))))
;;     (error
;;      "Undefined: dired-get-file-for-visit or dired--find-possibly-alternative-file")))
;;
;; (defun my-dired-xdg-open ()
;;   "Open the file under the cursor using xdg-open."
;;   (interactive)
;;   (if (fboundp 'dired-get-file-for-visit)
;;       (let* ((file (dired-get-file-for-visit)))
;;         (when my-dired-xdg-open-cmd
;;           (call-process my-dired-xdg-open-cmd nil nil nil file)))
;;     (error "Undefined: dired-get-file-for-visit")))
;;

;; (with-eval-after-load 'dired
;;   ;; Advise `dired-find-file' to use `my-dired-open-with-external-command'
;;   ;; instead
;;   (advice-add 'dired-find-file :override #'my-dired-open-with-external-command))

;;; DISABLED: Icons dired

;; (lightemacs-use-package nerd-icons-dired
;;   :if (display-graphic-p)
;;   ;;:diminish nerd-icons-dired-mode
;;   :commands nerd-icons-dired-mode
;;   ;; Cause bugs sometimes (e.g., when a file is deleted, the icons are not
;;   ;; aligned properly)
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; DISABLED: Abbreviate dired headers

;; --------------------------------------------------------------------------
;; Abbreviate dired header
;; https://emacs.stackexchange.com/questions/33799/is-there-any-way-to-abbreviate-dired-header
;;
;; I modified it to make it only modify the first line
;;
;; NOTE: does not work. it sometimes changes where the directory is
;; --------------------------------------------------------------------------
;; TODO: Contribution to Emacs?
;; (defvar my-dired-abbreviate-header t)
;;
;; (defun my-dired-readin-abbreviate-header (&rest _)
;;   "Abbreviate home directory path to '~' in the first line of the buffer."
;;   (when my-dired-abbreviate-header
;;     (save-excursion
;;       (goto-char (point-min))
;;       (let ((inhibit-read-only t)
;;             (case-fold-search nil)
;;             (home (expand-file-name "~"))
;;             (line-end (line-end-position)))
;;         (while (search-forward home line-end t)
;;           (replace-match "~" t t))))))
;;
;; (advice-add 'dired-readin :after 'my-dired-readin-abbreviate-header)

;;; Provide

(provide 'mod-dired)

;;; mod-dired.el ends here
