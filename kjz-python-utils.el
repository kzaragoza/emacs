;; Set up some stuff for Python mode
;; (autoload 'python-mode "python-mode" "Python editing mode." t)
;; (setq auto-mode-alist
;;       (append '(("\\.py$" . python-mode)) auto-mode-alist))
;; (setq interpreter-mode-alist
;;       (append '(("python" . python-mode)) interpreter-mode-alist))

;; Add iPython support.
;(require 'ipython)

;; Set up the Speedbar to recognize whether there is an up-to-date object file
;; for the given python script.
(add-hook 'speedbar-load-hook
	  (lambda ()
	    (setq speedbar-obj-alist (append '(("\\.py$" . ".pyc") ("\\.py$" . ".pyo")) speedbar-obj-alist))))

(defun convert-tabs-to-spaces ()
"Finds all tab characters from the point forward to end of buffer,
replacing them with four (4) spaces. This is most useful for
normalizing Python code that has tabs in place for indentation."
  (interactive)
  (while (search-forward "	" nil t)
    (replace-match "    " nil t)))


;; Define a snippet for Django models.
(yas/define 'python-mode "render"
            "render_to_response(\"${1:template}\", ${2:locals()}, context_instance=RequestContext(request))$0")

;; Define a couple of useful commands for Django development.
(defun django-start-server ()
  "Starts the Django development server."
  (interactive)
  (start-process "django-dev-server" "*Django Server*" "/Users/kjz/projects/dtg/manage.py" "runserver")
  (message "Django server started."))

(defun django-stop-server()
  "Stops the Django development server."
  (interactive)
  (signal-process "django-dev-server" 2) ; Send a SIGINT to stop the server.
  (message "Django server stopped."))
