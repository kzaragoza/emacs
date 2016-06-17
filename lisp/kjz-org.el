;;; Set up org mode for tracking TODOs and such.
(require 'org)
(setq org-directory "~/Dropbox/OrgFiles")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "\C-ca") 'org-agenda)
(setq org-log-done t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . nil)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (js . t)
   (python . t)
   (ruby . t)
   (sh . t)
   (sql . t)))

;; Use org-capture for quick entry of items.
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-work-notes-file (concat org-directory "/sermo.org"))
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+datetree org-work-notes-file)
         "* TODO %?\n  %i"
         :empty-lines 1)
        ("i" "General Item" entry (file+headline org-default-notes-file "Item")
         "* %?\n  %i")
        ("w" "Work Item" entry (file+datetree org-work-notes-file)
         "* %?\n %i"
         :empty-lines 1)))

;; Set up a shortcut to quickly go to the inbox for processing and refiling.
(global-set-key "\C-ci"
                (lambda ()
                  (interactive)
                  (find-file org-default-notes-file)))
(global-set-key "\C-cw"
                (lambda ()
                  (interactive)
                  (find-file org-work-notes-file)))

(defun kjz-org-export-rich-text ()
  "Quick utility script to export Org data to HTML on the
clipboard to paste into other applications."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil nil t)))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))
