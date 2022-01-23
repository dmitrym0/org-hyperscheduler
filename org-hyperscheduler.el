
(require 'org)
(require 'websocket)
(require 'cl)

(setq websocket-debug t)

(defvar wstest-server-buffer (get-buffer-create "*wstest-server*"))
(defvar wstest-server-name "wstest-server")

(defun print-entries ()
(message (cdr (assoc "ITEM" (org-entry-properties)))))

;(org-map-entries #'print-entries nil)


(defvar mock-org-contents-runtime
"* Heading 1
** Subheading 2
*** TODO a task
")

(defun print-entries ()
  (let* ((props (org-entry-properties))
         (reslist ())
         )
    props
    )
  )

(defun get-entries()
  (org-entry-properties)
  )


(defun get-dummy-schedule ()
  (with-temp-buffer
    (org-mode)
    (insert mock-org-contents-runtime)
    (get-calendar-entries)
    )
  )


(setq org-hs-ws-server
          (websocket-server
           44445
           :host 'local
           :on-open #'org-hs--ws-on-open
           :on-message #'org-hs--ws-on-message
           :on-close #'org-hs--ws-on-close))


(defun org-hs--ws-on-message (_ws frame)
  "Functions to run when the  server receives a message.
Takes _WS and FRAME as arguments."
  (let* ((msg (json-parse-string
               (websocket-frame-text frame) :object-type 'alist))
         (command (alist-get 'command msg))
         (data (alist-get 'data msg)))
    (message command)
    (message data)
    (cond ((string= command "getAgenda")
           (org-hs--get-agenda))
          ((string= command "delete")
           (org-roam-ui--on-msg-delete-node data))
          ((string= command "create")
           (org-roam-ui--on-msg-create-node data))
          (t
           (message
            "Something went wrong when receiving a message from org-roam-ui")))))



(defun org-hs--ws-on-open (ws)
  "Open the websocket WS and send initial data."
  (progn
    (setq org-roam-hs-ws-socket ws)
    (message "--------")
    )
)


(defun get-agenda ()
  (let* ((props (org-entry-properties))
         (js-date (get-js-date-pair ))
         )
    (print props)
    (push `(startDate . ,(cdr (assoc 'startDate js-date))) props)
    (push `(endDate . ,(cdr (assoc 'endDate js-date))) props)
    (push `(allDay . ,(cdr (assoc 'allDay js-date))) props)
    props
    )
  )

(defun get-calendar-entries (scope)
    (org-map-entries #'get-agenda "TODO=\"TODO\"+SCHEDULED>=\"<2008-10-11>\"" nil))
          
(provide 'org-hyperscheduler)

(defun org-hs--get-agenda ()
    (websocket-send-text org-roam-hs-ws-socket (json-encode (get-calendar-entries)))
  )





(defun get-js-date-pair ()
  (let* ((plist (car (cdr (org-element-property :scheduled  (org-element-at-point)))))
         (year-start (plist-get plist :year-start))
         (month-start (plist-get plist :month-start))
         (day-start (plist-get plist :day-start))
         (hour-start (plist-get plist :hour-start))
         (minute-start (plist-get plist :minute-start))
         (year-end (plist-get plist :year-end))
         (month-end (plist-get plist :month-end))
         (day-end (plist-get plist :day-end))
         (hour-end (plist-get plist :hour-end))
         (minute-end (plist-get plist :minute-end))
         (start (date-time-to-iso8601-js-like 0 minute-start hour-start day-start month-start year-start))
         (end (date-time-to-iso8601-js-like 0 minute-end hour-end day-end month-end year-end) )
         (all-day (if (eq hour-start nil) "true" "false"))
         (combined `((startDate . ,start) ( endDate . ,end) (allDay . ,all-day))))
    combined
    )
  )

(defun date-time-to-iso8601-js-like (seconds minutes hour day month year)
  (message (format "params %s %s %s %s %s %s" seconds minutes hour day month year))
  (let* ((minutes (or minutes 0))
        (hour (or hour 0)))
    (concat
     (format-time-string "%Y-%m-%dT%T"  (encode-time seconds minutes hour day month year))
     ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
      (format-time-string "%z")))))


