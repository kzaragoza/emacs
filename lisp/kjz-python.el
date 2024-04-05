;; Set up support for Python development.
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . eglot-ensure))

(use-package py-autopep8
  :ensure t
  :after python
  :hook ((python-mode . py-autopep8-mode)))

(use-package pyenv-mode
  :ensure t)

;; Set up the Speedbar to recognize whether there is an up-to-date object file
;; for the given python script.
(add-hook 'speedbar-load-hook
          (lambda ()
            (setq speedbar-obj-alist (append '(("\\.py$" . ".pyc") ("\\.py$" . ".pyo")) speedbar-obj-alist))))

