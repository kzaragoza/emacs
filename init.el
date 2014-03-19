;;; This is my .emacs file
(message "Loading .emacs...")

(add-to-list 'load-path "~/.emacs.d/lisp" 't)

;; Move the customizations to a separate file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add some other archives to the package manager.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Set up packages and ensure that certain things are installed.
(package-initialize)

(setq *required-packages*
      (list
       'csharp-mode
       'exec-path-from-shell
       'expand-region
       'find-file-in-project
       'findr
       'helm
       'helm-ls-git
       'inf-ruby
       'inflections
       'ipython
       'jump
       'lorem-ipsum
       'magit
       'markdown-mode
       'mmm-mode
       'multiple-cursors
       'rinari
       'rspec-mode
       'ruby-compilation
       'rvm
       'undo-tree
       'yasnippet
       ))

(defun kjz-ensure-packages-installed (packages)
  (mapc (lambda (name)
          (when (not (package-installed-p name))
            (package-install name)))
        packages))

(condition-case nil
    (kjz-ensure-packages-installed *required-packages*)
    (error
     (package-refresh-contents)
     (kjz-ensure-packages-installed *required-packages*)))

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
(when (string-prefix-p "BOSMAC" system-name)
  (load "kjz-work.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the Emacs server so I can use emacsclient from the command line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

(message "Loading .emacs done.")
