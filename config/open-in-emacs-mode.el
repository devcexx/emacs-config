(defconst open-in-emacs--path "/org/gnu/Emacs/OpenInEmacs")
(defconst open-in-emacs--interface "org.gnu.Emacs.OpenInEmacs")

(defvar open-in-emacs--dbus-object nil)

(defun open-in-emacs--handle (&rest args)
  "Handle a DBus call to OpenInEmacs function with the given ARGS."
  (unless (seq-every-p #'stringp args)
    (error (signal 'dbus-error "All the files must be strings")))
  
  (dolist (file args)
    (condition-case err
	(find-file-other-window file)
      (error (signal 'dbus-error (cdr err)))))
  
  (let ((frame (window-frame (selected-window))))
    (make-frame-visible frame)
    (when window-system
      (x-focus-frame frame)))
  t)

(defun open-in-emacs--enable ()
  "Enable the Open In Emacs mode, setting all the required dbus services and methods as required."
  (pcase (dbus-register-service :session dbus-service-emacs :replace-existing)
    ((or :primary-owner :already-owner)
     ; DBus registration success
     (setq open-in-emacs--dbus-object (dbus-register-method :session dbus-service-emacs
     open-in-emacs--path open-in-emacs--interface "OpenInEmacs"
     #'open-in-emacs--handle)))
    (_ (warn "Couldn't register Emacs in DBus. Is there another Emacs instance already running?"))))


(defun open-in-emacs--disable ()
  "Disable the Open In Emacs mode, unregistering all the methods associated."
  (when open-in-emacs--dbus-object
    (dbus-unregister-object open-in-emacs--dbus-object)
    (setq open-in-emacs--dbus-object nil)))

(define-minor-mode open-in-emacs-mode
  "Minor mode that enables opening files through a DBus request."
  :init-value nil
  :global t
  :group 'mode

  (if open-in-emacs-mode
      (open-in-emacs--enable)
      (open-in-emacs--disable)))

(provide 'open-in-emacs-mode)
