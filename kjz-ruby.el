;; Make sure Rake files open in ruby-mode.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; Make the return key do a newline and indent.
(add-hook 'ruby-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; Fix up our path by taking it from the shell.
(defun set-exec-path-from-shell-PATH () 
  (interactive) 
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'"))) 
    (setenv "PATH" path-from-shell) 
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; Set some environment variables we need for RVM support and finding the right gems.
(setenv "rvm_bin_path" "/Users/kzaragoza/.rvm/bin")
(setenv "GEM_HOME" "/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@sermo")
(setenv "GEM_PATH" "/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@sermo:/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@global")
(setenv "IRBRC" "/Users/kzaragoza/.rvm/rubies/ruby-1.9.3-p286/.irbrc")
(setenv "MY_RUBY_HOME" "/Users/kzaragoza/.rvm/rubies/ruby-1.9.3-p286")
(setenv "rvm_path" "/Users/kzaragoza/.rvm")
(setenv "rvm_prefix" "/Users/kzaragoza")
(setenv "rvm_version" "1.16.17 ()")
;; (setenv "GEM_PATH" "/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@sermo:/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@global")(setenv "GEM_PATH" "/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@sermo:/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@global")
;; (setenv "GEM_HOME" "/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@sermo:/Users/kzaragoza/.rvm/gems/ruby-1.9.3-p286@global")

;; Set up the Sermo dev environment.
(setenv "V2_HOME" "/Users/kzaragoza/git/sermo")
(setenv "SERMO_SETTINGS_PATH" "/Users/kzaragoza/git/sermo/lib/rails_deploy/config/settings.yml")
(setenv "LOCALHOST_LOCALDOMAIN" (concat "http://" (system-name)))
(setenv "DEV_ASSET_HOST" (concat "http://" (system-name) ":81"))
(setenv "BUILD_TARGET" "/tmp/builds")
(setenv "WORKING_COPY" "true")
(setenv "ENABLE_JASMINE" "true")
(setenv "EMAIL_OVERRIDE" "kris.zaragoza@worldone.com")
(setenv "PASSENGER_VERSION" "3.0.18")

