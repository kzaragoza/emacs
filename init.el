;;; This is my .emacs file
(message "Loading .emacs...")

(add-to-list 'load-path "~/.emacs.d/" 't)

;; Move the customizations to a separate file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom code and utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "kjz-misc.el")
(load "kjz-mmm.el")
(load "kjz-python-utils.el")
(load "kjz-text-utils.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the Emacs server so I can use emacsclient from the command line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

; Load my TODO list.
(find-file "~/TODO.org")

(message "Loading .emacs done.")

