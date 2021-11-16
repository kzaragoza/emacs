;;; This is my .emacs file
(message "Loading .emacs...")

(add-to-list 'load-path "~/.emacs.d/lisp" 't)

;; Move the customizations to a separate file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add some other archives to the package manager.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Patch things up to avoid issues on MacOS. See
;; https://emacs.stackexchange.com/questions/68288/error-retrieving-https-elpa-gnu-org-packages-archive-contents
(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Set up packages and ensure that certain things are installed.
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Install previously selected packages per what's in the custom file.
;; (package-install-selected-packages)

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
(load "kjz-psql.el")

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
                     gcs-done)))
