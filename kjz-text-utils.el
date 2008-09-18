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

; Set the HTML indent value to 4 from the default of 2.
; (setq html-helper-basic-offset 4)

; Method to call tidy on an XML document.
(defun xml-tidy-buffer ()
  (interactive)
  (mark-whole-buffer)
  (shell-command-on-region (point-min) (point-max) "tidy -q -i -xml" nil t))

; Load nxhtml mode stuff
;(load "nxml/autostart.el")

; Get rid of html-helper-mode from the auto-mode-alist
(setq auto-mode-alist
      (remove-if '(lambda (x) (eq (cdr x) 'html-helper-mode)) auto-mode-alist))

(setq magic-mode-alist
      (remove-if '(lambda (x) (eq (cdr x) 'html-helper-mode)) auto-mode-alist))

; load Django mode for HTML templating stuff.
;(load "django-html-mode.el")
;(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))

; Add a text mode hook to not indent lines, but rather just tab to the tab stop.
(add-hook 'text-mode-hook
          (lambda ()
            (setq indent-line-function 'tab-to-tab-stop)
            (local-set-key (kbd "RET") 'newline)))

