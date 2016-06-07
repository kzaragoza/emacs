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
         "* TODO %?\n  %a"
         :empty-lines 1)
        ("i" "General Item" entry (file+headline org-default-notes-file "Item")
         "* %?\n  %i\n  %a")
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
  (let ((org-export-with-author nil)
        (org-export-with-date nil)
        (org-export-with-email nil)
        (org-export-with-creator nil)
        (org-export-with-date nil)
        (org-html-validation-link nil)
        (org-export-with-statistics-cookie nil)
        (org-export-time-stamp-file nil)
        (org-export-with-title nil))
    (org-html-export-as-html))
  ;; Note we have to tweak the process coding system. It's set to Latin-1 by
  ;; default, which completely trashes the chevrons in the string, making the
  ;; AppleScript syntax invalid.
  ;;
  ;; The process here is simple.
  ;;
  ;;   1. Let org-mode do its export thing to an HTML buffer.
  ;;
  ;;   2. Convert the buffer text into a string of hex digits, 2 characters for
  ;;   each buffer char.
  ;;
  ;;   3. Use AppleScript to set the HTML format version of the clipboard
  ;;   contents. Unfortunately, pbpaste won't do this.
  (let* ((default-process-coding-system '(undecided-unix . utf-8-unix))
         (buf (apply #'concat (map 'list (lambda (c) (format "%0.2x" c)) (buffer-string))))
         (cmd (concat "osascript -e 'set the clipboard to «data HTML" buf "»'")))
    (shell-command cmd)
    (kill-buffer-and-window)))
