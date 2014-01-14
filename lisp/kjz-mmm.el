;; MMM mode configuration, especially for AOMS fulfiller config files.
(add-to-list 'load-path "~/.emacs.d/vendor/mmm")
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 1)
(set-face-background 'mmm-code-submode-face "#333333")
(set-face-background 'mmm-default-submode-face "#333333")

;; Set up the preferred major modes to use for various languages.
(setq mmm-major-mode-preferences
      '((perl cperl-mode perl-mode)
        (javascript espresso-mode c++-mode)
        (java jde-mode java-mode c++-mode)
        (css css-mode c++-mode)))

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
    :submode python-mode
    :front "{%"
    :back "%}"
    :include-front t
    :include-back t)
  (django-var
    :submode python-mode
    :front "{{"
    :back "}}"
    :include-front t
    :include-back t)
  (mxml-as
    :submode actionscript-mode
    :front "<mx:Script>[\t\n ]*<!\\[CDATA\\["
    :back "]]>[\t\n ]*</mx:Script>"
    :face 'mmm-default-submode-face)))

(mmm-add-mode-ext-class 'html-mode "\\.html" 'embedded-css)
(mmm-add-mode-ext-class 'html-mode "\\.html" 'django-tag)
(mmm-add-mode-ext-class 'html-mode "\\.html" 'django-var)
(mmm-add-mode-ext-class 'html-mode "\\.html" 'html-js)
(mmm-add-mode-ext-class 'sgml-mode "\\.mxml" 'mxml-as) ; This should catch XML files too.
(mmm-add-mode-ext-class 'xml-mode "\\.mxml" 'mxml-as) ; Just in case.
(mmm-add-mode-ext-class 'html-mode "\\.jsp" 'jsp)

