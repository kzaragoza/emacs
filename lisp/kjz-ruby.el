(use-package ruby-mode
  :mode (("\\.rake$" . ruby-mode)
         ("Rakefile$" . ruby-mode))
  :interpreter "ruby")

(use-package rspec-mode
  :ensure t
  :after ruby-mode)

(use-package rinari
  :after ruby-mode
  :ensure t)
