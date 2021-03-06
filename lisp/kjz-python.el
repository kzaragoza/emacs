;; Set up support for Python development.
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; Set up the Speedbar to recognize whether there is an up-to-date object file
;; for the given python script.
(add-hook 'speedbar-load-hook
          (lambda ()
            (setq speedbar-obj-alist (append '(("\\.py$" . ".pyc") ("\\.py$" . ".pyo")) speedbar-obj-alist))))

;; Define a couple of useful commands for Django development.
(defun django-start-server ()
  "Starts the Django development server."
  (interactive)
  (let ((server-cmd (expand-file-name (concat (file-name-as-directory (django-find-project-root)) "manage.py"))))
    (if (file-exists-p server-cmd)
        (progn
          (start-process "django-dev-server" "*Django Server*" "python" server-cmd "runserver")
          (message "Django server started."))
      (message "Failed to start Django server."))))

(defun django-stop-server()
  "Stops the Django development server."
  (interactive)
  (interrupt-process "django-dev-server") ; Send a SIGINT to stop the server.
  (message "Django server stopped."))

(defun django-find-project-root (&optional root)
  (when (null root) (setq root default-directory))
  (cond
   ((member "manage.py" (directory-files root)) (expand-file-name root)) ; Found it. Return the directory name.
   ((equal (expand-file-name root) "/") nil) ; Hit root directory. Return nil.
   (t (django-find-project-root (concat (file-name-as-directory root) ".."))))) ; Recurse and try parent directory.
