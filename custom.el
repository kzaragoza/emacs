(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-syntactic-indentation nil)
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes
   (quote
    ("2b56bd7a702f4dd5f50db417ec2bcdd06a878a0e2834e9ab37ad3c5c93aaab74" default)))
 '(default-frame-alist
    (quote
     ((foreground-color . "white")
      (background-color . "black")
      (menu-bar-lines . 1)
      (width . 132)
      (height . 44))))
 '(dired-dwim-target t)
 '(ecb-layout-name "left13")
 '(ecb-options-version "2.40")
 '(espresso-indent-level 2)
 '(fill-column 80)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(mac-emulate-three-button-mouse t)
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(ns-pop-up-frames nil)
 '(ns-use-srgb-colorspace nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/OrgFiles/sermo.org" "~/Dropbox/OrgFiles/inbox.org")))
 '(package-selected-packages
   (quote
    (projectile web-mode yasnippet which-key use-package undo-tree try sql-indent rspec-mode rinari paredit-menu org mmm-mode magit lorem-ipsum jinja2-mode ipython graphviz-dot-mode go-mode find-file-in-project expand-region exec-path-from-shell etags-table csharp-mode counsel cider ag ace-window ace-jump-mode ac-slime)))
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(save-abbrevs nil)
 '(save-interprogram-paste-before-kill t)
 '(sgml-basic-offset 4)
 '(show-paren-style (quote mixed))
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 30)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-show-unknown-files t)
 '(split-width-threshold 200)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tool-bar-mode nil)
 '(transient-mark-mode t))

;; for compatibility with older Aquamacs versions
 (defvar aquamacs-140-custom-file-upgraded t)
 (unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "Yellow" :height 2.0)))))
