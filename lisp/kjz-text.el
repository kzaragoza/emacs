;; Utilities and other stuff I like when in text mdoes.

;; Load the lorem ipsum generator. Very useful for placeholder text.  We put this
;; hear rather than in a mode hook because we can use this in all kinds of
;; documents, including code.
(require 'lorem-ipsum)

;; Set up reStructuredText support for text documents.
(autoload 'rst-mode "rst.el" "Major mode for reStructuredText documents." t)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode))
	      auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)

;; Function to call tidy on an XML document.
(setq xml-tidy-command "tidy -q -i -xml -wrap 75")
(defun xml-tidy-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (shell-command-on-region (point-min) (point-max) xml-tidy-command nil t)))

;; Get rid of html-helper-mode from the auto-mode-alist
(setq auto-mode-alist
      (cl-remove-if '(lambda (x) (eq (cdr x) 'html-helper-mode)) auto-mode-alist))

(setq magic-mode-alist
      (cl-remove-if '(lambda (x) (eq (cdr x) 'html-helper-mode)) auto-mode-alist))

;; Have Emacs recognize a single space as a sentence end.
(setq sentence-end-double-space nil)

;; Load up markdown-mode when editing API Blueprint files.
(push '("\\.apib$" . markdown-mode) auto-mode-alist)
