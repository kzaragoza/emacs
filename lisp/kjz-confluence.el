;; (add-to-list 'load-path "~/.emacs.d/vendor/confluence-el")
(require 'confluence)
(setq confluence-url "https://sermowiki.atlassian.net/rpc/xmlrpc")

(autoload 'confluence-get-page "confluence" nil t)

;; open confluence page
(global-set-key "\C-xwf" 'confluence-get-page)

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda ()
             (progn
               (visual-line-mode)
               (local-set-key "\C-xw" confluence-prefix-map))))
