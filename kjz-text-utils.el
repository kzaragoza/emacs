;; Utilities and other stuff I like when in text mdoes.

; Wire up longlines-mode so we get some nice word wrapping in text modes.
(autoload 'longlines-mode "longlines.el" "Minor mode for editing long lines." t)
(add-hook 'text-mode-hook 'longlines-mode)
(add-hook 'sgml-mode-hook 'longlines-mode-off) ; Don't want this in XML/HTML docs.

;; When in Text mode, turn on auto-fill-mode
;(add-hook 'text-mode-hook (lambda () (auto-fill-mode t)))

; Load the lorem ipsum generator. Very useful for placeholder text.  We put this
; hear rather than in a mode hook because we can use this in all kinds of
; documents, including code.
(require 'lorem-ipsum)

; Set up reStructuredText support for text documents.
(autoload 'rst-mode "rst.el" "Major mode for reStructuredText documents." t)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode))
	      auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)

; Method to call tidy on an XML document.
(defun xml-tidy-buffer ()
  (interactive)
  (mark-whole-buffer)
  (shell-command-on-region (point-min) (point-max) "tidy -q -i -xml" nil t))

; Load nxhtml mode and related support.
(load "nxml/autostart.el")
