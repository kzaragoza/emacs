;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up load path for the one-off files.
(add-to-list 'load-path "~/.emacs.d/vendor")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and Mode Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Support for NSIS script development
(autoload 'nsi-mode "nsi-mode" "nsi editing mode." t)
(add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode))

;; Set up yasnippet for snippets support.
(require 'yasnippet-bundle)

;; Load ido-mode
(ido-mode t)

;; Load up Git custom support.
;(require 'git)
(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(autoload 'magit-status "magit.el" "Start Magit Git integration" 't)
(if (boundp 'Info-additional-directory-list)
    (add-to-list 'Info-additional-directory-list "~/.emacs.d/vendor/magit")
  (setq Info-additional-directory-list '("~/.emacs.d/vendor/magit")))

;; Set up access to the MySQL command line interface.
(setq sql-mysql-program "/usr/local/mysql/bin/mysql")

;; Set up Steve Yegge's JS2 mode.
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities and Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easy toggle to full-screen mode.
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [(meta return)] 'fullscreen)

