;; MMM mode configuration, especially for AOMS fulfiller config files.
(require 'mmm-vars)
(require 'mmm-mode)
(require 'mmm-sample)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(set-face-background 'mmm-code-submode-face "#333333")

;; (mmm-add-classes
;;  '((aoms-config
;;     :submode python-mode
;;     :front "<param name=\"script\"><!\\[CDATA\\["
;;     :back "\\]\\]></param>"
;;     :insert (?p python nil @ "<param name=\"script\"><![CDATA[\n" @ _ @ "\n]]></param>\n" @)
;;     )))

;; Set up the mode classes.
(mmm-add-classes
 '((django-tag
    :submode python
    :front "{%"
    :back "%}"
    :include-front t
    :include-back t)))

(mmm-add-classes
 '((django-var
    :submode python
    :front "{{"
    :back "}}"
    :include-front t
    :include-back t)))

(mmm-add-mode-ext-class 'html-mode "\\.html" 'embedded-css)
(mmm-add-mode-ext-class 'html-mode "\\.html" 'django-tag)
(mmm-add-mode-ext-class 'html-mode "\\.html" 'django-var)
(mmm-add-mode-ext-class 'html-mode "\\.html" 'html-js)

;; Support for NSIS script development
(autoload 'nsi-mode "nsi-mode" "nsi editing mode." t)
(add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode))

;; Set up yasnippet for snippets support.
(require 'yasnippet-bundle)

;; Set the initial frame size/position for Carbon Emacs.
(if (boundp 'mac-carbon-version-string)
    (setq initial-frame-alist '( (top . 1) (left . 1) )))

;; Load ido-mode
(ido-mode t)

;; Use spaces instead of tabs pretty much everywhere. I tend to hate tabs and will
;; happily use C-q to get them.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Highlight parens/brackets/etc.
(show-paren-mode)

;; In the general case, use newline and indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; Load up Git custom support.
;(require 'git)
(autoload 'magit-status "magit.el" "Start Magit Git integration" 't)

;; Easy toggle to full-screen mode.
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [(meta return)] 'fullscreen)

;; Set up access to the MySQL command line interface.
(setq sql-mysql-program "/usr/local/mysql/bin/mysql")
