(use-package go-mode
  :ensure t
  :hook ((go-mode . (lambda ()
                      (local-set-key (kbd "C-c C-k") 'godoc)
                      (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                      (local-set-key (kbd "C-c C-g") 'go-goto-imports)
                      (local-set-key (kbd "C-c C-f") 'gofmt)
                      (local-set-key (kbd "C-c C-k") 'godoc)))
         (go-mode . eglot-ensure)
         (before-save . gofmt-before-save)))

