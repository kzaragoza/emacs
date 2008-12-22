;;; This is the .emacs file for NT Emacs
(message "Loading .emacs...")

(add-to-list 'load-path "~/.emacs.d/" 't)

;; Move the customizations to a separate file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom code and utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "kjz-misc.el")
(load "kjz-python-utils.el")
(load "kjz-text-utils.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the Emacs server so I can use emacsclient from the command line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

(message "Loading .emacs done.")


(put 'narrow-to-region 'disabled nil)
