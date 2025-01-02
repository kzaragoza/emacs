;;; Set up org mode for tracking TODOs and such.
(use-package org
  :ensure t
  :mode ("\\.org$" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook
  visual-line-mode
  :config
  (progn
    ;; Org-tempo lets us expand the structured templates in org like <s for source
    ;; blocks.
    (require 'org-tempo)
    (setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org")
    (add-to-list 'org-agenda-files org-directory)
    (setq org-log-done t)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (js . t)
       (python . t)
       (ruby . t)
       (shell . t)
       (sql . t)))
    ;; Use org-capture for quick entry of items.
    (setq org-default-notes-file (concat org-directory "/inbox.org"))
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-default-notes-file "Todo")
             "* TODO %?\nCAPTURED: %u\n%i"
             :empty-lines 1)
            ("i" "General Note" entry (file+headline org-default-notes-file "Notes")
             "* %?\nCAPTURED: %u\n%i")
            ("m" "Meeting" entry (file+datetree org-default-notes-file)
             "* %? %(org-set-tags \"meeting\")\n%U\n%i")))

    ;; Hide the leading asterisks and some markup to reduce the visual noise.
    ;; (setq org-hide-leading-stars t)
    (setq org-hide-emphasis-markers t)

    ;; Set up a shortcut to quickly go to the inbox for processing and refiling.
    (global-set-key "\C-ci"
                    (lambda ()
                      (interactive)
                      (find-file org-default-notes-file)))

    ;; Setup LaTeX export to use xelatex.
    (setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))))

;; Set up ox-clip to copy formatted content from org files. The ox-clip
;; package should be cross platform.
(use-package ox-clip
  :ensure t)

(defun kjz-org-export-rich-text ()
  "Quick utility script to export Org data to HTML on the
clipboard to paste into other applications. Note this only works
on MacOS as it leverages the textutil tool that comes bundled
with it."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil nil t)))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))

