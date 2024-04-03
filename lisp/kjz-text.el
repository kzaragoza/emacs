;; Utilities and other stuff I like when in text modes.

;; Load the lorem ipsum generator. Very useful for placeholder text.  We put this
;; hear rather than in a mode hook because we can use this in all kinds of
;; documents, including code.
(use-package lorem-ipsum
  :ensure t)

;; Set up reStructuredText support for text documents.
;; (autoload 'rst-mode "rst.el" "Major mode for reStructuredText documents." t)
;; (setq auto-mode-alist
;;       (append '(("\\.rst$" . rst-mode)
;; 		("\\.rest$" . rst-mode))
;; 	      auto-mode-alist))
;; (add-hook 'rst-adjust-hook 'rst-toc-update)

;; Load up Markdown mode.
(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

;; Function to call tidy on an XML document.
(setq xml-tidy-command "tidy -q -i -xml -wrap 75")
(defun xml-tidy-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (shell-command-on-region (point-min) (point-max) xml-tidy-command nil t)))

;; Have Emacs recognize a single space as a sentence end.
(setq sentence-end-double-space nil)

;; Load up markdown-mode when editing API Blueprint files.
(push '("\\.apib$" . markdown-mode) auto-mode-alist)
