;; Set up org mode for tracking TODOs and such.
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "\C-ca") 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/Dropbox/OrgFiles/todo.org" "~/Dropbox/OrgFiles/sermo.org"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

(defun kjz-org-export-rich-text ()
  "Quick utility script to export Org data to HTML on the
clipboard to paste into other applications."
  (interactive)
  (org-html-export-as-html)
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
    (kill-buffer)))
