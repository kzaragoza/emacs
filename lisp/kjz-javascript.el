;; Set up Steve Yegge's JS2 mode.
(autoload 'js2-mode "js2" nil t)
; Don't load it for .js files. We'll try espresso mode for a while and manually switch if needed.
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Set up espresso mode. This mode might work better with mmm-mode.
(autoload 'espresso-mode "espresso" "Start espresso mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
