;;; mix.el --- MIX game server

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This MIX server will automatically register with the master MIX
;; server to act as a public server. If behind NAT, you will need to
;; port forward both TCP and UDP on the selected port your server.

;; Start and stop the server with `mix-start' and `mix-stop'.

;; TODO:

;; * mix-unregister on `mix-stop'
;; * Figure out the meaning of the US and ID ping fields

;;; Code:

(defgroup mix ()
  "MIX game server for Synthetic Reality games."
  :group 'games)

(defcustom mix-port 8888
  "MIX server port number."
  :group 'mix
  :type 'integer)

(defcustom mix-name "Emacs-MIX"
  "Name of the MIX server."
  :group 'mix
  :type 'string)

;; Master server registration

(defvar mix-master-host "63.197.64.78"
  "Host address of the MIX master server.")

(defvar mix-master-ports '(21999 22999 23999)
  "Port of the MIX master server. The different ports are for each game.")

(defun mix-make-id ()
  "Compute a unique, consistent ID for this server."
  (let ((uniq (format "%s%s%s%s%s%s" (user-uid) (system-name) (user-full-name)
                      (user-login-name) user-mail-address mix-port)))
    (substring (upcase (md5 uniq)) 0 7)))

(defun mix-build-register-message ()
  "Generate packet for registering with the MIX master server."
  (format "!version=41252,nump=%d,id=%s,port=%d,name=%s\0"
          (length mix-client-list) (mix-make-id) mix-port mix-name))

(defun mix-register ()
  "Register this MIX server with the MIX master."
  (dolist (mix-master-port mix-master-ports)
    (process-send-string
     (make-network-process
      :name     "mix-master-register"
      :host     mix-master-host
      :service  mix-master-port
      :family   'ipv4
      :type     'datagram)
     (mix-build-register-message))
    (mix-log (format "registered with master server %d\n" mix-master-port))
    (if (process-status "mix-master-register")
        (delete-process "mix-master-register"))))

;; Ping handler

(defun mix-ping-start ()
  "Start the process that handles pings."
  (make-network-process
   :name     "mix-ping"
   :service  mix-port
   :type     'datagram
   :server   t
   :family   'ipv4
   :filter   'mix-ping))

(defun mix-ping (proc data)
  "Respond with standard ping reply."
  (mix-log (concat "PING\n"))
  (process-send-string
   proc
   (format "#name=%s //ID:%s //TM:%X //US:0.3.5\0"
           mix-name (mix-make-id) (float-time))))

(defun mix-ping-stop ()
  "Stop the MIX ping server."
  (when (process-status "mix-ping")
    (delete-process "mix-ping")))

;; Echo client functions

(defvar mix-client-list '()
  "List of connected clients.")

(defun mix--rem-from-list (list el)
  "Opposite of add-to-list."
  (set list (remq el (symbol-value list))))

;;;###autoload
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

;;;###autoload
(defun mix-stop ()
  "Stop the MIX server."
  (interactive)
  (cancel-function-timers 'mix-register)
  (when (process-status "mix")
    (delete-process "mix"))
  (dolist (client mix-client-list)
    (delete-process client))
  (mix-ping-stop))

(defun mix-sentinel (proc stat)
  "Mix server's sentinel: called when status changes."
  (mix-log (concat "(" (symbol-name (process-status proc)) ") " stat))
  (if (eq (process-status proc) 'open)
      (add-to-list 'mix-client-list proc)
    (mix--rem-from-list 'mix-client-list proc)))

(defun mix-filter (proc data)
  "Echo data in to all clients."
  (mix-datalog data)
  (dolist (client mix-client-list)
    (unless (eq client proc)
      (process-send-string client data))))

(defun mix-datalog (data)
  "Write the packet to the data log."
  (mix-log-string "*mix-data*" (concat data "\n")))

(defun mix-log (string)
  "Write the status to the log."
  (mix-log-string "*mix*"
                  (concat (format-time-string "%Y %b %d %H:%M:%S")
                          " - " string)))

(defun mix-log-string (buffer-name string)
  "Add a string to a log in a buffer."
  (with-current-buffer (get-buffer-create buffer-name)
    (toggle-read-only 1)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert string))))

(provide 'mix)

;;; mix.el ends here
