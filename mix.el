;;; MIX game server

;; Settings

(defvar mix-master-host "63.197.64.78"
  "Host address of the MIX master server.")

(defvar mix-master-port 21999
  "Port of the MIX master server.")

(defvar mix-id (format "9000%04X" (random (expt 2 16)))
  "Unique ID for this server.")

(defvar mix-port 33335
  "MIX server port number.")

(defvar mix-name "Emacs-MIX"
  "Name of the MIX server.")

(defvar mix-player-count 0
  "Current player count.")

;; MIX master server functions

(defun mix-build-register-message ()
  "Generate packet for registering with the MIX master server."
  (format "!version=41252,nump=%d,id=%s,port=%d,name=%s\0"
	  mix-player-count mix-id mix-port mix-name))

(defun mix-register ()
  "Register this MIX server with the MIX master."
  (process-send-string
   (make-network-process
    :name     "mm-report"
    :host     mix-master-host
    :service  mix-master-port
    :family   'ipv4
    :type     'datagram)
   (mix-build-register-message)))

;; Echo client functions

(defvar mix-client-list '()
  "List of connected clients.")

(defun rem-from-list (list el)
  "Opposite of add-to-list."
  (set list (remq el (symbol-value list))))

(defun mix-start ()
  (mix-stop)
  (setq mix-client-list '())
  (make-network-process
   :name     "mix"
   :service  mix-port
   :sentinel 'mix-sentinel
   :server   t
   :family   'ipv4
   :filter   'mix-filter))

(defun mix-stop ()
  "Stop the emacs web server."
  (interactive)
  (if (process-status "mix") (delete-process "mix")))

(defun mix-sentinel (proc stat)
  "Mix server's sentinel: called when status changes."
  (mix-log (concat "mix: (" (symbol-name (process-status proc)) ") " stat))
  (if (eq (process-status proc) 'open)
      (add-to-list 'mix-client-list proc)
    (rem-from-list 'mix-client-list proc)))

(defun mix-filter (proc data)
  "Echo data in to all clients."
  (mix-datalog data)
  (dolist (client mix-client-list)
    (unless (eq client proc)
      (process-send-string client data))))

(defun mix-datalog (data)
  "Write the packet to the data log."
  (with-current-buffer (get-buffer-create "*mix-data*")
    (insert data)))

(defun mix-log (string)
  "Write the status to the log."
  (with-current-buffer (get-buffer-create "*mix*")
    (insert string)))
