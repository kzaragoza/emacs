;; Set up PostgreSQL as the default product.
(setq sql-mode-hook
      (list
       (lambda ()
         (sql-set-product 'postgres))))

(defun sermo-psql (server database relay-host)
  "Set up a psql session to one of the fleet database
servers. This presumes that the relay host already has a .pg_pass
file set up for the user's account (as they all should). This
also always logs into the DB server as the user sermo.

Note, this doesn't work well with the shell prompts we have set
up in the various environments. In order for our systems to play
nice with Tramp, we need to alter the shell prompt when accessed
through Tramp. Drop this into the .bash_profile file on each
async box:

if [ $TERM = dumb ]; then
	PS1=\"[\\u@\\H \\w] \"
fi

That will alter the prompt to something that Tramp will parse
correctly without having to delve into strange regexp black magic
and customizing shell-prompt-pattern.
"
  (let ((product 'postgres)
        (sql-user "sermo")
        (sql-server server)
        (sql-database database)
        (new-name nil))
    ;; Largely lifted from sql-product-interactive in sql.el
    
    (let ((buf (sql-find-sqli-buffer product sql-connection))
          (default-directory (format "/ssh:%s:" relay-host)))
      (if (and (not new-name) buf)
          (pop-to-buffer buf)

        ;; We have a new name or sql-buffer doesn't exist or match
        ;; Start by remembering where we start
        (let ((start-buffer (current-buffer))
              new-sqli-buffer)

          ;; Connect to database, bouncing through the relay host, if provided.
          (message "Login...")
          (funcall (sql-get-product-feature product :sqli-comint-func)
                   product
                   (sql-get-product-feature product :sqli-options))

          ;; Set SQLi mode.
          (let ((sql-interactive-product product))
            (sql-interactive-mode))

          ;; Set the new buffer name
          (setq new-sqli-buffer (current-buffer))
          (when new-name
            (sql-rename-buffer new-name))
          (set (make-local-variable 'sql-buffer)
               (buffer-name new-sqli-buffer))

          ;; Set `sql-buffer' in the start buffer
          (with-current-buffer start-buffer
            (when (derived-mode-p 'sql-mode)
              (setq sql-buffer (buffer-name new-sqli-buffer))
              (run-hooks 'sql-set-sqli-hook)))

          ;; All done.
          (message "Login...done")
          (run-hooks 'sql-login-hook)
          (pop-to-buffer new-sqli-buffer))))))


(defmacro make-psql (fleet host db-server database)
  "Macro to generate a psql-[fleet] command."
  (let ((funsymbol (intern (format "psql-%s" fleet))))
    `(defun ,funsymbol ()
       (interactive)
       (sermo-psql ,db-server ,database ,host ))))

(make-psql "prod" "async.sermo.prod" "dball" "suds_production")
(make-psql "dw" "async.sermo.prod" "dw" "telemetry_production")
(make-psql "owl" "async.sermo.owl" "dball" "suds_owl")
(make-psql "ape" "async.sermo.ape" "dball" "suds_ape")
(make-psql "bee" "async.sermo.bee" "dball" "suds_bee")

