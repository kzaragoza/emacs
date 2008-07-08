;; Set the return key to call newline-and-indent so that code can be
;; automatically properly indented.  This change due to the fact that
;; PC keyboards don't have a linefeed key
(global-set-key "\r" 'newline-and-indent)

;; In XEmacs, make the Del key delete the next character.
(setq delete-key-deletes-forward t)

;; Set the indent to be four characters in CSS mode. I really don't like the
;; default 2.
(setq cssm-indent-level 4)
(setq cssm-indent-function #'cssm-c-style-indenter)

;; MMM mode configuration, especially for AOMS fulfiller config files.
;; (require 'mmm-mode)
;; (setq mmm-global-mode 'maybe)
;; (setq mmm-submode-decoration-level 1)
;; (mmm-add-classes
;;  '((aoms-config
;;     :submode python-mode
;;     :front "<param name=\"script\"><!\\[CDATA\\["
;;     :back "\\]\\]></param>"
;;     :insert (?p python nil @ "<param name=\"script\"><![CDATA[\n" @ _ @ "\n]]></param>\n" @)
;;     )))

;; Set up yasnippet for snippet support.
(require 'yasnippet-bundle)

