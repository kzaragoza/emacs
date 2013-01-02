;; Set up Clojure support.
(add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode")
(setq inferior-lisp-program "~/bin/clj")
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)))
