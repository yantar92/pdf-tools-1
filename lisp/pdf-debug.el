;;; pdf-view.el --- Easily log debug messages. -*- lexical-binding:t -*-

;;; Debug
(setq pdf-debug-log-counter 0)

(defun pdf-debug-format-log-message (counter message &rest objects)
  (apply #'concat (number-to-string counter) " " (if (listp message)
                                  (propertize (car message)
                                              'face
                                              (let ((face (cadr message)))
                                                (pcase face
                                                  ((pred symbolp) face)
                                                  ((pred stringp) (list :foreground face))
                                                  ((pred listp) face))))
                                message)
         (mapcar (lambda (o)
                   (concat "\n" (prin1-to-string o)))
                 objects)))

(defun pdf-debug-log (message &optional level &rest objects)
  (let* ((counter (cl-incf pdf-debug-log-counter))
         (message (apply #'pdf-debug-format-log-message counter message objects)))
    (display-warning '(pdf-scroll) message level "*pdf-debug-log*")))

(defun pdf-debug-debug (message &rest objects)
  (apply #'pdf-debug-log message :debug objects))

(defun pdf-debug-warn (message &rest objects)
  (apply #'pdf-debug-log message :warning objects))

(defun pdf-debug-log-buffer ()
  (interactive)
  (switch-to-buffer "*pdf-debug-log*"))

(when (boundp 'spacemacs-version)
  (spacemacs/set-leader-keys "bl" #'pdf-debug-log-buffer))

(provide 'pdf-debug)

;;; pdf-debug.el ends here
