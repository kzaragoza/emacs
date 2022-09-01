;; Load SLIME and set up my local SBCL as the default Lisp.
(use-package slime
  :ensure t
  :config (progn
            (setq inferior-lisp-program "sbcl --no-linedit")
            (setq slime-contribs '(slime-fancy))))
