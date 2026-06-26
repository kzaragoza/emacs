;;; This is my .emacs file
(message "Loading .emacs...")

(add-to-list 'load-path (locate-user-emacs-file "lisp") 't)

;; Move the customizations to a separate file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;; Add some other archives to the package manager.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; When things slow down on init, running (use-package-report) can show which
;; use-package directives are using up a lot of time.
(setq use-package-compute-statistics t)

;; Set up packages and ensure that certain things are installed.
(package-initialize)

;; On MacOS, copy in and set up the path when run as an application rather than
;; being launched from the shell.
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom code and utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "kjz-misc.el")
(load "kjz-python.el")
(load "kjz-text.el")
(load "kjz-lisp.el")
(load "kjz-ruby.el")
(load "kjz-go.el")
(load "kjz-org.el")

;; Load any machine-specific definitions.
(let* ((fqdn (downcase system-name))
      (localname (car (split-string fqdn "\\.")))
      (fname (concat "~/.emacs.d/lisp/" localname ".el")))
  (when (file-exists-p fname)
    (load fname)))

;; (message "Loading .emacs done.")
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (kjz-restore-gc-values)))
