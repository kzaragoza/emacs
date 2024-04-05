;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up load path for the one-off files.
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Use the modus-vivendi theme.
(load-theme 'modus-vivendi)

;; Use spaces instead of tabs pretty much everywhere. I tend to hate tabs and will
;; happily use C-q to get them.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Highlight parens/brackets/etc.
(show-paren-mode)

;; In the general case, use newline and indent
;; (global-set-key (kbd "RET") 'newline-and-indent)

;; Tell Emacs not to disable narrow functionality.
(put 'narrow-to-region 'disabled nil)

;; Set the frame title to show the full file path name or buffer name.
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Turn on column numbers in all buffers.
(column-number-mode 't)

;; Set up mouse wheel to scroll only one line at a time rather than the
;; default 5.
(setq mouse-wheel-scroll-amount '(1 ((shift . 1)) ((control) . nil)))

;; Bind F5 to revert-buffer for convenience.
(global-set-key [f5] 'revert-buffer-without-confirm)

;; Automatically revert files changed on disk.
(global-auto-revert-mode t)

;; Store backup files in a separate directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Load the misc library and rewire M-z to zap up to a char rather than
;; blow the char away.
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Bind C-x C-d to dired as well. I never use list-directory, but I sometimes
;; hit that keybinding by accident.
(global-set-key (kbd "C-x C-d") 'dired)

;; Enable the ability to edit file permissions in wdired.
(setq wdired-allow-to-change-permissions t)

;; Copy whatever is on the clipboard to the kill ring before whacking it with a
;; fresh kill. This will avoid a lot of pain when copying and pasting across
;; applications.
(setq save-interprogram-paste-before-kill t)

;; Type less at confirmation prompts.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Go ahead and delete text if you start typing while the region is enabled.
(delete-selection-mode t)

;; Use IBuffer in place of the usual buffer-menu.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enable fido-mode for completion.
(fido-mode 1)
(fido-vertical-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and Mode Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up ace-window
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;; Set up expand-region, a nifty little tool.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Set up yasnippet for snippets support. Don't load it everywhere. Use the
;; minor mode where needed.
(use-package yasnippet
  :ensure t)

;; Bind a useful keystroke to magit-status since I use it so damned much.
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Enable which-key mode to prompt for key combinations that I can never seem to
;; remember.
(use-package which-key
  :ensure t
  :config (which-key-mode 1))

;; Enable auto-complete everywhere.
;; (use-package auto-complete
;;   :config (global-auto-complete-mode 1))

;; Use web-mode for dealing with HTML templates (erb, jinja2, etc.)
(use-package web-mode
  :ensure t)

;; I deal with lots of CSV files. Use csv-mode to make life easier.
(use-package csv-mode
  :ensure t
  :mode "\\.csv$")

;; Set up YAML file support. This format has become quite popular.
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

;; Markdown is likewise popular and used all over the place.
(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

;; Set up tree-sitter.
(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :after tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities and Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easy toggle to full-screen mode.
(defun fullscreen ()
  "Toggles the current frame to full-screen and back."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [(meta return)] 'fullscreen)


;; Utility functions for common stuff.

(defun untabify-buffer ()
  "Run untabify on the whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Run indent-region on the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun revert-buffer-without-confirm ()
  "Call revert-buffer without confirming the revert."
  (interactive)
  (revert-buffer nil t)
  (message "Reloaded."))

;; Copy the current buffer full path to the clipboard.
(defun copy-file-name ()
  "Copies the full path of the current buffer to the system clipboard."
  (interactive)
  (let ((filename (or (buffer-file-name) default-directory)))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; Taken from http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; I do this too damned often to not have some utility commands for it.
(defun url-escape-region (start end)
  "URL encode the region between START and END in the current buffer."
  (interactive "r")
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (url-hexify-string text)))))

(defun url-unescape-region (start end)
  "URL decode the region between START and END in the current buffer."
  (interactive "r")
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (url-unhex-string text)))))
