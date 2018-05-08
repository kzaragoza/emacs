(use-package sql
  :config
  (progn
    ;; Set the default proxy-buffer to nil.
    (setq proxy-buffer nil)
    (sql-set-product 'postgres)))

(defun foobuf ()
  "Create a scratch buffer to be used with one of the psql-* commands defined above."
  (interactive)
  (pop-to-buffer (generate-new-buffer "foo"))
  (sql-mode))

(defun kjz-create-psql-buffer (bufname)
  "Create a scratch buffer to be used with one of the psql-* commands."
  (let ((buf (generate-new-buffer (format "*SQL Scratch %s*" bufname))))
    (with-current-buffer buf
      (sql-mode)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  ;; Kill the psql buffer.
                  (if (and sql-buffer (get-buffer sql-buffer))
                      (with-current-buffer sql-buffer
                        (let ((kill-buffer-query-functions nil)) ; Suppress kill confirmation prompts.
                          (comint-send-eof)
                          (kill-buffer)))))
                nil
                t))
    buf))

(defun kjz-setup-proxy (relay-host local-port db-host db-port)
  (let ((proxy-arg (format "%d:%s:%d" local-port db-host db-port))
        (new-buffer-name (generate-new-buffer-name (format "*Proxy %s*" db-host))))
    (start-process "proxy" new-buffer-name "/usr/bin/ssh" "-L" proxy-arg relay-host)
    (with-current-buffer new-buffer-name
      (comint-mode)
      ; Now that everything is kicked off, wait for the prompt to show up.
      (unless (string-match "\\] \\$?" (buffer-string))
        (sleep-for 1)))
    (get-buffer new-buffer-name)))

(defun kjz-psql-connect (relay-host local-port db-host db-port db-name db-user)
  (let* ((sql-connection-alist '((my-db (sql-product 'postgres)
                                        (sql-port local-port)
                                        (sql-server "localhost")
                                        (sql-user db-user)
                                        (sql-database db-name))))
         (proc-buffer (kjz-setup-proxy relay-host local-port db-host db-port)))
    (sql-connect 'my-db)
    (make-local-variable 'proxy-buffer)
    (setq proxy-buffer proc-buffer)
    (add-hook 'kill-buffer-hook
              (lambda ()
                ;; Kill the proxy buffer.
                (if (get-buffer proxy-buffer)
                    (with-current-buffer proxy-buffer
                      (let ((kill-buffer-query-functions nil)) ;Suppress kill confirmation prompts.
                        (comint-send-eof)
                        (kill-buffer)))))
              nil
              t)))

(defmacro kjz-make-psql (name relay-host local-port db-host db-port db-name db-user)
  "Macro to generate a psql-[fleet] command."
  (let ((funsymbol (intern (format "psql-%s" name)))
        (docstring (format "Connects to %s on %s through a port forwarded by SSH.

Since Emacs uses psql under the covers, the password for
the connection will be taken from the .pgpass file in your
home directory. Make sure this is up to date for each of
the established forwarded ports." db-name db-host)))
    `(defun ,funsymbol ()
       ,docstring
       (interactive)
       (let ((buf (kjz-create-psql-buffer ,db-host)))
         (switch-to-buffer buf)
         (kjz-psql-connect ,relay-host ,local-port ,db-host ,db-port ,db-name ,db-user)
         (other-window -1)))))

