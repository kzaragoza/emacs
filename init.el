;;; This is my .emacs file
(message "Loading .emacs...")

(add-to-list 'load-path "~/.emacs.d/" 't)

;; Move the customizations to a separate file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add some other archives to the package manager.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom code and utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "kjz-misc.el")
(load "kjz-flex.el")
(load "kjz-python.el")
(load "kjz-text.el")
(load "kjz-mmm.el")
(load "kjz-lisp.el")
(load "kjz-javascript.el")
(load "kjz-ruby.el")
;; Load any machine-specific definitions.
(if (file-exists-p (concat user-emacs-directory "kjz-local.el"))
    (load "kjz-local.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the Emacs server so I can use emacsclient from the command line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

(message "Loading .emacs done.")
