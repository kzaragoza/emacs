;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local settings for my work machine.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "sql"
  '(load-library "sql-indent"))

;; Set up some groups for ibuffer to make finding things easier.
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Magit" (name . "magit"))
         ("Emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Help\\*$")
                   (name . "^\\*info\\*$")
                   (name . "^\\*Completions\\*$"))))))

(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; When searching for files, make sure we find .erb and .rake files too.
(require 'find-file-in-project)
(add-to-list 'ffip-patterns "*.rake" t)
(add-to-list 'ffip-patterns "*.erb" t)
(setq ffip-limit 10240)

;; Load environment from shell since Emacs.app doesn't actually run from a shell
;; and thus doesn't have all the environment variables set.
(setq exec-path-from-shell-variables
      (append exec-path-from-shell-variables '("rvm_bin_path"
                                               "GEM_HOME"
                                               "GEM_PATH"
                                               "IRBRC"
                                               "MY_RUBY_HOME"
                                               "rvm_path"
                                               "rvm_prefix"
                                               "rvm_version"
                                               "V2_HOME"
                                               "SERMO_SETTINGS_PATH"
                                               "LOCALHOST_LOCALDOMAIN"
                                               "DEV_ASSET_HOST"
                                               "BUILD_TARGET"
                                               "WORKING_COPY"
                                               "ENABLE_JASMINE"
                                               "EMAIL_OVERRIDE"
                                               "PASSENGER_VERSION")))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Copy that environment into eshell when it starts up.
(add-hook 'eshell-load-hook
          (lambda ()
            (exec-path-from-shell-copy-envs '("rvm_bin_path" "GEM_HOME" "GEM_PATH" "IRBRC" "MY_RUBY_HOME" "rvm_path" "rvm_prefix" "rvm_version" "V2_HOME" "SERMO_SETTINGS_PATH" "LOCALHOST_LOCALDOMAIN" "DEV_ASSET_HOST" "BUILD_TARGET" "WORKING_COPY" "ENABLE_JASMINE" "EMAIL_OVERRIDE" "PASSENGER_VERSION"))))

;; Set up TAGS files and related functionality.

(defun kjz-retag-local ()
  (interactive)
  (let* ((command "(cd /Users/kris.zaragoza/Projects/sermo; ctags --tag-relative -R -e --exclude=.git)") 
         (command-buffer-name "*Retag Output*"))
    (start-process-shell-command "retag" command-buffer-name command)
    (pop-to-buffer command-buffer-name)))

(defun kjz-retag-vagrant ()
  (interactive)
  (let* ((default-directory "/ssh:default:/home/vagrant/src/sermo/")
         (command "ctags --tag-relative -R -e -f /home/vagrant/src/sermo/TAGS --exclude=.git --exclude=.rvm")
         (command-buffer-name "*Retag Output*"))
    (start-file-process "retag" (get-buffer-create command-buffer-name) "ctags" "--tag-relative" "-R" "-e" "-f" "/home/vagrant/src/sermo/TAGS" "--exclude=.git" "--exclude=.rvm")
    (pop-to-buffer command-buffer-name)))

; Auto-revert TAGS files without asking.
(setq tags-revert-without-query t)

; Bump the threshold warning of large files. The TAGS file can get big.
(setq large-file-warning-threshold 30000000) ; 30 MB

; Search up the directory tree to find a TAGS table.
(require 'etags-table)
(setq etags-table-search-up-depth 10)

;; Load custom utility functions for running psql against various environments.
(load "kjz-psql.el")
