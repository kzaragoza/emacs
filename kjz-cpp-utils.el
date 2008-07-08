;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some supporting stuff for C++ development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cc-mode customization
(setq-default c-basic-offset 4)
(setq c-default-style '((other . "user")))

;; Because the "brilliant" people around here (Lotus) like to use C++
;; style comments in their C code, I may as well make sure I'm always
;; in c++-mode for any C files.  Otherwise, the syntax coloring gets
;; really screwed up.
(setq auto-mode-alist
      (append
       '(("\\.c$" . c++-mode)
	 ("\\.h$" . c++-mode)
	 ("\\.hpp$" . c++-mode))
       auto-mode-alist))



(setq *class-header* 
"///////////////////////////////////////////////////////////////////////////
// Class:
//
// Description:
//
///////////////////////////////////////////////////////////////////////////
")

(setq *function-header* 
"///////////////////////////////////////////////////////////////////////////
// Function: 
//
// Input:
// Output:
// Returns:
// Throws:
// Description:
//
///////////////////////////////////////////////////////////////////////////
")

(setq *file-header* 
"///////////////////////////////////////////////////////////////////////////
// File:
// Copyright 1999-2000, Lotus Development
//
// Description:
//
///////////////////////////////////////////////////////////////////////////
")

(defun class-header ()
  (interactive)
  (insert *class-header*))
(defun function-header ()
  (interactive)
  (insert *function-header*))
(defun file-header ()
  (interactive)
  (let ((pos (point)))
    (goto-char (point-min))
    (insert *file-header*)
    (goto-char (+ pos (length *file-header*)))))
