;;; mix.el --- MIX game server
;;
;; This MIX server register with the master MIX server to act as a
;; public server. You will need to port forward both TCP and UDP on
;; the port your server runs on for it to work properly.
;;
;;; TODO
;; * mix-unregister on mix-stop
;; * Figure out the meaning of the US and ID ping fields

;; Configuration

(defvar mix-master-host "63.197.64.78"
  "Host address of the MIX master server.")

(defvar mix-master-port 21999
  "Port of the MIX master server.")

(defvar mix-id (format "%04X%04X" (random (expt 2 16)) (random (expt 2 16)))
  "Unique ID for this server.")

(defvar mix-port 33335
  "MIX server port number.")

(defvar mix-name "Emacs-MIX"
  "Name of the MIX server.")

;; MIX master server functions (mix-register)

(defun mix-build-register-message ()
  "Generate packet for registering with the MIX master server."
  (format "!version=41252,nump=%d,id=%s,port=%d,name=%s\0"
	  (length mix-client-list) mix-id mix-port mix-name))

(defun mix-register ()
  "Register this MIX server with the MIX master."
  (process-send-string
   (make-network-process
    :name     "mix-master-register"
    :host     mix-master-host
    :service  mix-master-port
    :family   'ipv4
    :type     'datagram)
   (mix-build-register-message))
  (if (process-status "mix-master-register")
      (delete-process "mix-master-register")))

;; Ping handler

(defun mix-ping-start ()
  "Start the process that handles pings."
  (interactive)
  (make-network-process
   :name     "mix-ping"
   :service  mix-port
   :type     'datagram
   :server   t
   :family   'ipv4
   :filter   'mix-ping))

(defun mix-ping (proc data)
  "Respond with standard ping reply."
  (process-send-string proc
		       (format "#name=%s //ID:%s //TM:%X //US:0.3.5\0"
			       mix-name mix-id (float-time))))

(defun mix-ping-stop ()
  "Stop the MIX ping server."
  (interactive)
  (when (process-status "mix-ping")
    (delete-process "mix-ping")))

;; Echo client functions

(defvar mix-client-list '()
  "List of connected clients.")

(defvar mix-timer nil
  "The handle for the registration timer.")

(defun rem-from-list (list el)
  "Opposite of add-to-list."
  (set list (remq el (symbol-value list))))

(defun mix-start ()
  "Start the MIX server."
  (interactive)
  (mix-stop)
  (setq mix-client-list '())
  (make-network-process
   :name     "mix"
   :service  mix-port
   :sentinel 'mix-sentinel
   :server   t
   :family   'ipv4
   :filter   'mix-filter)
  (mix-ping-start)
  (run-at-time 0 300 'mix-register))

(defun mix-stop ()
  "Stop the MIX server."
  (interactive)
  (when mix-timer
    (cancel-timer mix-timer)
    (setq mix-timer nil))
  (when (process-status "mix")
    (delete-process "mix"))
  (dolist (client mix-client-list)
    (delete-process client))
  (mix-ping-stop))

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
    (goto-char (point-max))
    (insert data "\n")))

(defun mix-log (string)
  "Write the status to the log."
  (with-current-buffer (get-buffer-create "*mix*")
    (insert string)))
