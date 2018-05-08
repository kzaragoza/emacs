;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local settings for my work machine.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kjz-make-psql "owl" "tools.sermo.owl" 54321 "dball.sermo.owl" 5432 "suds_owl" "sermo")
(kjz-make-psql "ape" "tools.sermo.ape" 54322 "dball.sermo.ape" 5432 "suds_ape" "sermo")
(kjz-make-psql "bee" "tools.sermo.bee" 54323 "dball.sermo.bee" 5432 "suds_bee" "sermo")
(kjz-make-psql "eel" "tools.sermo.eel" 54324 "dball.sermo.eel" 5432 "suds_eel" "sermo")
(kjz-make-psql "eeldw" "tools.sermo.eel" 54325 "dw.sermo.eel" 5432 "telemetry_eel" "sermo")
(kjz-make-psql "prod" "tools.sermo.prod" 54326 "dball.sermo.prod" 5432 "suds_production" "sermo")
(kjz-make-psql "proddw" "tools.sermo.prod" 54327 "dw.sermo.prod" 5432 "telemetry_production" "sermo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Warehouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun warehouse ()
  (interactive)
  (let ((buf (generate-new-buffer "*SQL Scratch warehouse"))
        (sql-server "warehouse.sermo.prod")
        (sql-database "warehouse_production")
        (sql-port 5432))
    (switch-to-buffer buf)
    (sql-mode)
    (sql-postgres)
    (other-window -1)))

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

;; Load environment from shell since Emacs.app doesn't actually run from a shell
;; and thus doesn't have all the environment variables set.
;; (setq exec-path-from-shell-variables
;;       (append exec-path-from-shell-variables '("rvm_bin_path"
;;                                                "GEM_HOME"
;;                                                "GEM_PATH"
;;                                                "IRBRC"
;;                                                "MY_RUBY_HOME"
;;                                                "rvm_path"
;;                                                "rvm_prefix"
;;                                                "rvm_version"
;;                                                "V2_HOME"
;;                                                "SERMO_SETTINGS_PATH"
;;                                                "LOCALHOST_LOCALDOMAIN"
;;                                                "DEV_ASSET_HOST"
;;                                                "BUILD_TARGET"
;;                                                "WORKING_COPY"
;;                                                "ENABLE_JASMINE"
;;                                                "EMAIL_OVERRIDE"
;;                                                "PASSENGER_VERSION")))
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;; Copy that environment into eshell when it starts up.
(add-hook 'eshell-load-hook
          (lambda ()
            (exec-path-from-shell-copy-envs '("rvm_bin_path" "GEM_HOME" "GEM_PATH" "IRBRC" "MY_RUBY_HOME" "rvm_path" "rvm_prefix" "rvm_version" "V2_HOME" "SERMO_SETTINGS_PATH" "LOCALHOST_LOCALDOMAIN" "DEV_ASSET_HOST" "BUILD_TARGET" "WORKING_COPY" "ENABLE_JASMINE" "EMAIL_OVERRIDE" "PASSENGER_VERSION"))))

;; Set up TAGS files and related functionality.

(defun kjz-retag-local ()
  (interactive)
  (let* ((command "(cd /Users/kris.zaragoza/src/sermo; ctags --tag-relative -R -e --exclude=.git)") 
         (command-buffer-name "*Retag Output*"))
    (start-process-shell-command "retag" command-buffer-name command)
    (pop-to-buffer command-buffer-name)))

(defun kjz-retag-vagrant ()
  (interactive)
  (let* ((default-directory "/ssh:vagrant:/home/vagrant/src/sermo/")
         (command "ctags --tag-relative -R -e -f /home/vagrant/src/sermo/TAGS --exclude=.git --exclude=.rvm")
         (command-buffer-name "*Retag Output*"))
    (start-file-process "retag" (get-buffer-create command-buffer-name) "ctags" "--tag-relative" "-R" "-e" "-f" "/home/vagrant/src/sermo/TAGS" "--exclude=.git" "--exclude=.rvm")
    (pop-to-buffer command-buffer-name)))

; Auto-revert TAGS files without asking.
(setq tags-revert-without-query t)

; Bump the threshold warning of large files. The TAGS file can get big.
(setq large-file-warning-threshold 30000000) ; 30 MB

;; Search up the directory tree to find a TAGS table.
(use-package etags-table
  :config (setq etags-table-search-up-depth 10))

;; Set up Tramp prompt scanning to find some of our customized prompts where we stick control characters after the $.
(setq tramp-shell-prompt-pattern "^.*$ .*")
