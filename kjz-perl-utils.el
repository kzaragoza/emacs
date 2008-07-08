;; Set up CPerl mode instead of Perl mode
(setq auto-mode-alist
      (append
       '(("\\.pl$" . cperl-mode)
         ("\\.pm$" . cperl-mode))
       auto-mode-alist))

(setq *perl-function-header*
"###########################################################################
# Function:
#
# Arguments:
#
# Returns:
#
# Description:
#
###########################################################################
")

(defun perl-function-header ()
  (interactive)
  (insert *perl-function-header*))
  
