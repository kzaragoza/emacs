;; Set up support for Python development.
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Set up the Speedbar to recognize whether there is an up-to-date object file
;; for the given python script.
(add-hook 'speedbar-load-hook
          (lambda ()
            (setq speedbar-obj-alist (append '(("\\.py$" . ".pyc") ("\\.py$" . ".pyo")) speedbar-obj-alist))))

