;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up load path for the one-off files.
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Load the PATH from the system.
(when (equal system-type 'darwin)
  (exec-path-from-shell-initialize))

;; Set the initial frame size/position for Carbon Emacs.
(if (boundp 'mac-carbon-version-string)
    (setq initial-frame-alist '( (top . 1) (left . 1) )))

;; Use spaces instead of tabs pretty much everywhere. I tend to hate tabs and will
;; happily use C-q to get them.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Highlight parens/brackets/etc.
(show-paren-mode)

;; In the general case, use newline and indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; Use buffer-menu rather than the buffer list.
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Set up windmove to move between windows in a frame. Use the Meta key rather
;; than the default Shift key.
(require 'windmove)
(windmove-default-keybindings 'meta)

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

;; Store backup files in a separate directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Load the misc library and rewire M-z to zap up to a char rather than
;; blow the char away.
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Set up expand-region, a nifty little tool.
(global-set-key (kbd "C-=") 'er/expand-region)

;; Bind C-x C-d to dired as well. I never use list-directory, but I sometimes
;; hit that keybinding by accident.
(global-set-key (kbd "C-x C-d") 'dired)

;; Set up just the rectangle stuff from CUA mode.
(cua-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and Mode Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Support for NSIS script development
(autoload 'nsi-mode "nsi-mode" "nsi editing mode." t)
(add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode))

;; Set up yasnippet for snippets support.
(require 'yasnippet)
(yas-global-mode 1)

;; Load imenu. I use this for jumping around source files.
(require 'imenu)

;; Set up access to the MySQL command line interface.
(setq sql-mysql-program "/usr/local/bin/mysql")

;; Set up org mode for tracking TODOs and such.
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "\C-ca") 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/TODO.org"))

;; Set up utility for finding files within a project.
; This lets us scope our searches to the app subdirectories in the Sermo codebase.
;(global-set-key (kbd "M-p") 'find-file-in-project)
(global-set-key (kbd "C-x t") 'find-file-in-project)

;; Go ahead and delete text if you start typing while the region is enabled.
(delete-selection-mode t)

;; Use IBuffer in place of the usual buffer-menu.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Bind a useful keystroke to magit-status since I use it so damned much.
(global-set-key (kbd "C-x g") 'magit-status)

;; Bind keys for multiple-cursors mode.
(global-set-key (kbd "M-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-]") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-]") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-[") 'mc/edit-lines)

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

;; Inspired by
;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer.
(defun copy-file-name ()
  "Show the full path file name in the minibuffer and copy it to
the kill ring."
  (interactive)
  (when buffer-file-name
    (message (buffer-file-name))
    (kill-new (file-truename buffer-file-name))))
