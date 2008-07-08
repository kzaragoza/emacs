;;; show-whitespace-mode.el --- Simple mode to highlight whitespaces

;; Copyright (C) 2002 by Aurélien Tisné
;; Author: Aurélien Tisné <address@bogus.example.com>
;; Keywords: convenience, editing
;; Created: 7 Aug 2002

;; This file is not part of GNU Emacs.

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Introduction:
;;
;; Whitespaces (spaces and tabs) are not visible by default. This package,
;; when it is activated, highlights spaces and tabs. It may be useful to see
;; trailing whitespaces (and to decide to use 'delete-trailing-whitespace').
;; Highlight rules can be easily change to fit your need (specific highlights
;; for leading and trailing spaces for instance).
;;
;; You can choose between two modes of highlighting:
;;  - color: show whitespaces with faces
;;  - mark: show whitespaces with specific characters
;; Use 'cutomize-group RET show-whitespace' to see the description.

;; Usage:
;;
;; Go to the appropriate buffer and press:
;;   M-x show-whitespace-mode RET

;; The function `turn-on-show-whitespace-mode' could be added to any major
;; mode hook to activate Show-Whitespace Mode for all buffers in that
;; mode.  For example, the following line will activate Show-Whitespace
;; Mode in all SGML mode buffers:
;;
;; (add-hook 'sgml-mode-hook 'turn-on-show-whitespace-mode)

;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.

;; Compatibility:
;;
;;  This version of show-whitespace has been developed with NTEmacs 21.2.1
;;  under MS Windows XP (nt5.1). It has been tested on linux both in X
;;  and console.
;;  Please, let me know if it works with other OS and versions of Emacs.

;; Thanks:
;;
;; I would like to thank Pieter Pareit for the suggestion of the mark style;
;; and for his precious tips.
 
;; Bug:
;;
;; - Using the mark mode, some special characters (like french accents)
;;   are displayed in decimal notation. (I don't understand why!)

;;; History:
;;
;; 1.0  14 Nov 2002  21.2.1
;;  - First release
;;
;; 2.0  22 Nov 2002  21.2.1
;;  - Add the style concept including the mark style (suggested by Pieter Pareit).
;;  - Add show-whitespace keywords at the end to be compliant with the
;;    outline mode.
;;
;; 2.1  04 Jan 2003  21.2.1
;;  - Show RET in mark style with ¶.
;;  - Show unbreakable spaces (suggested by Benjamin Drieu).

;;; Code:

(defgroup show-whitespace nil
  "Minor mode to show whitespaces."
  :prefix "show-ws-"
  :group 'editing
)

;; Variables:

(defcustom show-ws-style 'color
  "*Define the style whitespaces will be shown.

Supported values are 'color' and 'mark':
  If color, whitespaces are highilted with faces.
  If mark, whitespaces are substituated by visible characters."
  :type '(choice (const color) (const mark))
  :group 'show-whitespace
)

(defvar show-ws-active-style nil
  "Internal use.
Used to ensure to disable the mode in the same style it has been enabled.")

(defcustom show-ws-visible-space 183    ; ?\xB7
  "*Used to show a space.

Used when `show-ws-style' is mark.
It's not a good idea to choose a character that may appear in the buffer."
  :type 'character
  :group 'show-whitespace)

(defcustom show-ws-unbr-space 186       ; ?\xBA
  "*Used to show an unbreakable space.

Used when `show-ws-style' is mark.
It's not a good idea to choose a character that may appear in the buffer."
  :type 'character
  :group 'show-whitespace)

(defcustom show-ws-visible-tab 187      ; ?\xBB
  "*Used to show a tab.  It is followed by a tab.

Used when `show-ws-style' is mark.
It's not a good idea to choose a character that may appear in the buffer."
  :type 'character
  :group 'show-whitespace)

(defcustom show-ws-visible-ret 182      ; ?\xB6
  "*Used to show a carriage return.  It is folowed by RET.

Used when `show-ws-style' is mark.
It's not a good idea to choose a character that may appear in the buffer."
  :type 'character
  :group 'show-whitespace)

(defvar show-ws-initial-font-lock-keywords nil
  "Used to save initial `font-lock-keywords' value to be able to restore\
it when the mode is switched off.")

(defvar show-ws-initial-display-table nil
  "Used to save initial `buffer-display-table' to be able to restore\
it when the mode is switched off.")

;; Faces:

(defface show-ws-spaces
  `((((type tty) (class color))
     (:background "LemonChiffon1"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color))
     (:background "LemonChiffon1"))
    (t (:background "LemonChiffon1")))
  "Face for highlighting spaces."
  :group 'show-whitespace)

(defface show-ws-unbr-spaces
  `((((type tty) (class color))
     (:background "LemonChiffon3"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color))
     (:background "LemonChiffon3"))
    (t (:background "LemonChiffon3")))
  "Face for highlighting unbreakable spaces."
  :group 'show-whitespace)

(defface show-ws-tabs
  `((((type tty) (class color))
     (:background "LemonChiffon2"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color))
     (:background "LemonChiffon2"))
    (t (:background "LemonChiffon2")))
  "Face for highlighting tabs."
  :group 'show-whitespace)


;; Functions:

;;;###autoload
(define-minor-mode show-whitespace-mode
  "Toggle whitespace highlighting in current buffer.

With arg, turn Show-Whitespace mode on if and only if arg is positive.
This is a minor mode that affects only the current buffer."
  ;; the initial value
  nil
  ;; the indicator for the mode line
  " show-ws"
  ;; the keymap
  nil
  ;; the body
  (if show-whitespace-mode
      ;; Show whitespaces distinguishing spaces and tabs
      (progn
        (setq show-ws-active-style show-ws-style)
        (if (eq show-ws-style 'mark)
            (progn                      ; mark
              (if buffer-display-table
                  (progn                ; backup the initial table
                    (make-local-variable 'show-ws-initial-display-table)
                    (setq show-ws-initial-display-table
                          (copy-sequence buffer-display-table)))
                (setq buffer-display-table (make-display-table)))
              (aset buffer-display-table ?\  (vector show-ws-visible-space))
              (aset buffer-display-table ?\xA0 (vector show-ws-unbr-space))
              (aset buffer-display-table ?\é ?\è)
              (aset buffer-display-table ?\t (vector show-ws-visible-tab ?\t))
              (aset buffer-display-table ?\n (vector show-ws-visible-ret ?\n))
              )

          (progn                        ; color
            (make-local-variable 'show-ws-initial-font-lock-keywords)
            (setq show-ws-initial-font-lock-keywords font-lock-keywords)
            (font-lock-add-keywords nil
                                    '(
                                      ;; show spaces
                                      ("[ ]+"   (0 'show-ws-spaces t))
                                      ;; show unbreakable spaces [C-q 240 RET]
                                      ("[ ]+" (0 'show-ws-unbr-spaces t))
                                      ;; show tabs
                                      ("[\t]+" (0 'show-ws-tabs t))
                                      ) t)
            (font-lock-fontify-buffer))))


    ;; revert to initial display
    (if (eq show-ws-active-style 'mark)
        ;; mark
        (setq buffer-display-table show-ws-initial-display-table)

      (progn                            ; color
        (setq font-lock-keywords show-ws-initial-font-lock-keywords)
        (font-lock-fontify-buffer)))))


;;;###autoload
(defun turn-on-show-whitespace-mode ()
  "Turn on Show-Whitespace Mode.

This function is designed to be added to hooks, for example:
  (add-hook 'sgml-mode-hook 'turn-on-show-whitespace-mode)"
  (show-whitespace-mode 1))


;;  Allow this feature to be used.
(provide 'show-whitespace-mode)

;;; show-whitespace-mode.el ends here
