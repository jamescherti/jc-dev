;;; mod-dired.el --- mod-dired -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;;; Require

(eval-and-compile
  (require 'lightemacs-use-package))
(require 'seq)

;;; Dired defaults

;; Asks for confirmation before creating missing parent directories during file
;; copy or rename operations. This protects against creating unintended
;; directories due to typos while remaining convenient.
;; (setq dired-create-destination-dirs 'ask)

;; Automatically creates destination directories without asking if the
;; destination path ends with a trailing slash. This is highly efficient because
;; the trailing slash indicates clear intent to create a directory.
;; (setq dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29

;; Reuses a single buffer for Dired navigation instead of opening a new buffer
;; for every directory. This keeps your buffer list clean and prevents Dired
;; buffer proliferation.
(setq dired-kill-when-opening-new-dired-buffer t)

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

(setq dired-hide-details-hide-symlink-targets nil)

(setq dired-create-destination-dirs 'ask)

;; TODO minimal-emacs.d
;; setq native-comp-async-on-battery-power nil) is an excellent default, for
;; users running Emacs on laptops. Background native compilation (via gccemacs)
;; is a highly CPU-intensive task. When packages are installed or updated,
;; spawning multiple asynchronous compiler processes on battery power can cause
;; rapid battery drain and thermal throttling. Suspending this behavior until
;; the machine is connected to AC power is a sensible optimization.
;;
;; NOTE: Issue. This stops native compilation, even when the laptop is charging.
;; (setq native-comp-async-on-battery-power nil)

;; Disable the optimization locally for dired to guarantee directory
;; fontification
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (setq-local redisplay-skip-fontification-on-input nil)))

(setq lightemacs-dired-filter-global-enabled t)
(setq lightemacs-dired-filter-setup-hook '(dired-filter-by-omit
                                           dired-filter-by-git-ignored
                                           dired-filter-by-dot-files))

;;; Other dired settings

(defun my-dired-get-file-open-command (file-path)
  "Return FILE-PATH corresponding command from `dired-guess-shell-alist-user'."
  (let* ((case-fold-search nil)
         (result (seq-find (lambda (pattern)
                             (string-match-p (car pattern) file-path))
                           dired-guess-shell-alist-user)))
    (when result
      (car (cdr result)))))

(defun my-dired-open-with-external-command ()
  "Open the current file in `dired' using an external command based on file type."
  (interactive nil dired-mode)
  (if (and (fboundp 'dired-get-file-for-visit)
           (fboundp 'dired--find-possibly-alternative-file))
      (let* ((file (dired-get-file-for-visit)))
        (let ((shell-cmd (my-dired-get-file-open-command file)))
          (if shell-cmd
              (progn
                ;; (message "[RUN] %s %s" shell-cmd file)
                (if (fboundp 'quick-fasd-add-path)
                    (quick-fasd-add-path file)
                  (message "Warning: Undefined: `quick-fasd-add-path'"))
                (call-process shell-cmd nil nil nil file))
            (condition-case err
                (dired--find-possibly-alternative-file file)
              (error
               (message "Container parsing failed (%s). Opening literally."
                        (error-message-string err))
               (find-file-literally file))))))
    (error
     "Undefined: dired-get-file-for-visit or dired--find-possibly-alternative-file")))

(with-eval-after-load 'dired

  ;; --------------------------------------------------------------------------
  ;; Functions
  ;; --------------------------------------------------------------------------
  (defun my-dired-home ()
    "Dired home."
    (interactive)
    (dired "~/"))

  ;; --------------------------------------------------------------------------
  ;; Abbreviate dired header
  ;; https://emacs.stackexchange.com/questions/33799/is-there-any-way-to-abbreviate-dired-header
  ;;
  ;; I modified it to make it only modify the first line
  ;;
  ;; NOTE: does not work. it sometimes changes where the directory is
  ;; --------------------------------------------------------------------------
  ;; TODO: Contribution to Emacs?
  ;; (defvar dired-abbreviate-header t)
  ;;
  ;; (defun my-dired-readin-abbreviate-header (&rest _)
  ;;   "Abbreviate home directory path to '~' in the first line of the buffer."
  ;;   (when dired-abbreviate-header
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

  ;; --------------------------------------------------------------------------
  ;; Using xdg-open/open/start for certain filetypes
  ;; --------------------------------------------------------------------------
  (defvar my-dired-xdg-open-cmd nil)

  (defun my-dired-xdg-open ()
    "Make Dired open the file under the cursor."
    (interactive)
    ;; Removed: (dired-get-filename nil t)
    (if (fboundp 'dired-get-file-for-visit)
        (let* ((file (dired-get-file-for-visit)))
          (when my-dired-xdg-open-cmd
            (call-process my-dired-xdg-open-cmd nil nil nil file)))
      (error "Undefined: dired-get-file-for-visit")))

  (when-let* ((cmd (cond (IS-MAC "open")
                         (IS-LINUX "xdg-open")
                         (IS-WINDOWS "start"))))
    (when cmd
      (setq my-dired-xdg-open-cmd cmd)
      (setq dired-guess-shell-alist-user
            `(("\\.\\(?:docx\\|pdf\\|odt\\|odg\\|ods\\|djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpe?g\\|webp\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|m4a\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ;; ("\\.csv\\'" ,cmd)
              ;; ("\\.html?\\'" ,cmd)
              ;; ("\\.md\\'" ,cmd)
              ))))

  ;; Advise `dired-find-file' to use `my-dired-open-with-external-command'
  ;; instead
  (advice-add 'dired-find-file :override #'my-dired-open-with-external-command))

;;; Icons dired

;; (lightemacs-use-package nerd-icons-dired
;;   :if (display-graphic-p)
;;   ;;:diminish nerd-icons-dired-mode
;;   :commands nerd-icons-dired-mode
;;   ;; Cause bugs sometimes (e.g., when a file is deleted, the icons are not
;;   ;; aligned properly)
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))

(provide 'mod-dired)

;; Local variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; mod-dired.el ends here
