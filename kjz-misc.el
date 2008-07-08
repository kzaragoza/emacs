;; Set the return key to call newline-and-indent so that code can be
;; automatically properly indented.  This change due to the fact that
;; PC keyboards don't have a linefeed key
(global-set-key "\r" 'newline-and-indent)

;; In XEmacs, make the Del key delete the next character.
(setq delete-key-deletes-forward t)

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
