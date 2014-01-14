;; Make sure Rake files open in ruby-mode.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

;; Load rspec-mode for simple running of specs from a buffer.
(require 'rspec-mode)

;; Load rinari for Rails dev.
(require 'rinari)
(global-rinari-mode)

;; Make the return key do a newline and indent.
(add-hook 'ruby-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))


