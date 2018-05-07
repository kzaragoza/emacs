;;; This is my .emacs file
(message "Loading .emacs...")

(add-to-list 'load-path "~/.emacs.d/lisp" 't)

;; Move the customizations to a separate file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add some other archives to the package manager.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Set up packages and ensure that certain things are installed.
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Install previously selected packages per what's in the custom file.
(package-install-selected-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom code and utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "kjz-misc.el")
(load "kjz-python.el")
(load "kjz-text.el")
(load "kjz-lisp.el")
(load "kjz-javascript.el")
(load "kjz-ruby.el")
(load "kjz-go.el")
(load "kjz-org.el")
(load "kjz-psql.el")

;; Load any machine-specific definitions.
(when (string-prefix-p "bosmac" (downcase system-name))
  (load "kjz-work.el"))

;; (message "Loading .emacs done.")
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
