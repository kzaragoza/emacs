;; MMM mode configuration, especially for AOMS fulfiller config files.
(add-to-list 'load-path "~/.emacs.d/vendor/mmm")
(require 'mmm-vars)
(require 'mmm-mode)
(require 'mmm-sample)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(set-face-background 'mmm-code-submode-face "#333333")

;; This application is now defunct. However, it's a nice example of how to configure
;; MMM mode for something totally custom.
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

