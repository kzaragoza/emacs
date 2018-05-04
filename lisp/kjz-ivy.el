(ivy-mode)

;; Handy counsel replacements.
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f1> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c g") 'counsel-ag)
(global-set-key (kbd "M-p") 'counsel-git)
(global-set-key (kbd "C-x j") 'counsel-imenu)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

;; Wrap around when walking through completions.
(setq ivy-wrap t)

;; Adjust the minibuffer height. 10 by default.
;; (setq ivy-height 10)

;; Set up Magit completion
(require 'magit)
(setq magit-completing-read-function 'ivy-completing-read)
