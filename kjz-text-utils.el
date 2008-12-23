;; Utilities and other stuff I like when in text mdoes.

; Wire up longlines-mode so we get some nice word wrapping in text modes. This isn't
; set up in any hooks, so call it manually when wanted.
(autoload 'longlines-mode "longlines.el" "Minor mode for editing long lines." t)

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

; Function to call tidy on an XML document.
(setq xml-tidy-command "tidy -q -i -xml")
(defun xml-tidy-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (shell-command-on-region (point-min) (point-max) xml-tidy-command nil t)))

; Get rid of html-helper-mode from the auto-mode-alist
(setq auto-mode-alist
      (remove-if '(lambda (x) (eq (cdr x) 'html-helper-mode)) auto-mode-alist))

(setq magic-mode-alist
      (remove-if '(lambda (x) (eq (cdr x) 'html-helper-mode)) auto-mode-alist))


