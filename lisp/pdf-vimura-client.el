(require 'epc)
(require 'svg)

(let ((default-directory (file-name-directory (locate-library "pdf-tools"))))
  (defvar my-epc (epc:start-epc "python" '("vimura_epc_server.py"))))

(defun pdf-vimura-init ()
  ;; (epc:call-sync my-epc 'vimura_init list (buffer-file-name)))
  (epc:call-sync my-epc 'open (list "/mnt/4EEDC07F44412A81/git/Masterthesis/main.pdf"))
  (setq-local vimura-doc-pages (epc:call-sync my-epc 'number_of_pages nil)))

(defun pdf-vimura-restart-server ()
  "DON'T FORGET TO SAVE CHANGES TO SERVER .PY FILE FIRST"
  (interactive)
  (epc:stop-epc my-epc)
  (setq my-epc (epc:start-epc "python" '("vimura_epc_server.py")))

  (pdf-vimura-init))

(defun pdf-vimura-format-edges (edges-string)
  (mapcar #'string-to-number (split-string edges-string)))

(provide 'pdf-vimura-client)
