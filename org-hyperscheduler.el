;;; org-hyperscheduler.el --- web representation of org-agenda
;; Copyright © 2022 Dmitry Markushevich

;; TODO: Add license.

;; ---------------------------------------------------------------------------------------------------
(require 'org)
(require 'websocket)
(require 'cl-lib)

;; ---------------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------------
;; options

(defgroup org-hyperscheduler nil
  "org-hyperscheduler"
  :group 'org-hyperscheduler
  :prefix "org-hyperscheduler-"
  :link '(url-link :tag "Github" "https://github.com/dmitrym0/org-hyperscheduler"))

(defcustom org-hyperscheduler-readonly-mode t
  "If true, the web interface becomes read only.
   In Read-only mode, changes to agenda entries can only be made from Emacs.
   In Read-write mode, changes can be made either in Emacs or in the web-interface.
   **NOTE** that for bidirectional changes to work each eligible agenda entry must have an ~org-id~.
   This org-id will be added automatically by org-hyperscheduler. If you don't want org-hyperscheduler to modify your agenda entries, keep the read-only mode enabled."
  :group 'org-hyperscheduler
  :type 'boolean)

(defcustom org-hyperscheduler-hide-done-tasks t
  "If true, once a task transitions from TODO to DONE it disappears from the web calendar"
  :group 'org-hyperscheduler
  :type 'boolean)


(defcustom org-hyperscheduler-agenda-filter "TIMESTAMP>=\"<2022-01-31>\"|SCHEDULED>=\"<2022-01-31>\""
  "This is a filter to use to generate a list of agenda tasks/entries to show in the calendar."
  :group 'org-hyperscheduler
  :type 'string)

(defcustom org-hyperscheduler-inbox-file (concat org-directory "/inbox.org")
  "This is the file where newly created entries go (the ones created in the WebUI."
  :group 'org-hyperscheduler
  :type 'string)


;; ---------------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------------

(setq websocket-debug t)

(defvar wstest-server-buffer (get-buffer-create "*wstest-server*"))
(defvar wstest-server-name "wstest-server")

(defun print-entries ()
  (message (cdr (assoc "ITEM" (org-entry-properties)))))



(defvar mock-org-contents-runtime
  "* Heading 1
** Subheading 2
*** TODO a task
")


; modify the agenda filter if we want to hide done tasks.
(and org-hyperscheduler-hide-done-tasks (setq org-hyperscheduler-agenda-filter (format "%s/-DONE" org-hyperscheduler-agenda-filter)))


(defun print-entries ()
  (let* ((props (org-entry-properties))
         (reslist ())
         )
    props
    )
  )

(defun get-entries()
  (org-entry-properties))


; for testing
(defun get-dummy-schedule ()
  (with-temp-buffer
    (org-mode)
    (insert mock-org-contents-runtime)
    (get-calendar-entries)
    )
  )


(setq org-hs-ws-server
    ; only run the server if we are not in test env.
    (unless (boundp 'org-hyperscheduler-test-env) 
          (websocket-server
           44445
           :host 'local
           :on-open #'org-hs--ws-on-open
           :on-message #'org-hs--ws-on-message
           :on-close #'org-hs--ws-on-close)))

(defun org-hs-stop-server ()
  (interactive)
  (websocket-server-close org-hs-ws-server))

(defun org-hs--ws-on-message (_ws frame)
  "Functions to run when the server receives a message.
Takes _WS and FRAME as arguments."
  (let* ((msg (json-parse-string
               (websocket-frame-text frame) :object-type 'alist))
         (command (alist-get 'command msg))
         (data (alist-get 'data msg)))
    (message (format "Command=[%s] Data=[%s]" command data))
    (cond ((string= command "get-agenda")
           (org-hs--get-agenda))
          ((string= command "update-event")
           (org-hs--update-event data))
          ((string= command "add-scheduled-event")
           (org-hs--add-scheduled-event data))
          (nil
           (message
            "Something went wrong when receiving a message from org-hyperscheduler-ui")))))



(defun org-hs--ws-on-open (ws)
  "Open the websocket WS and send initial data."
  (progn
    (setq org-hs-ws-socket ws)
    (message "org-hyperscheduler: connection from the browser")
    )
)

(defun org-hs--update-event (data)
  "Update the given event with the content provided."
  (message "+org-hs-update-event")
  (let* ((id (alist-get 'id data))
         (timestamp (get-scheduled-timestamp-for-scheduled-event (alist-get 'start data) (alist-get 'end data))))
    (message (format "Updating ID: %s to timestamp: %s" id timestamp))
    (save-window-excursion
      (find-event-by-id id)
      (schedule-at-point timestamp)))
  (message "-org-hs-update-event"))
             
(defun org-hs--add-scheduled-event (data)
  "Create a new event in an inbox."
  (message "+org-hs--add-scheduled-event")
  (let* ((title (alist-get 'title data))
         (timestamp (get-scheduled-timestamp-for-scheduled-event (cdr (assoc 'startUnix data)) (cdr (assoc 'endUnix data)))))
    (save-window-excursion
      (find-file org-hyperscheduler-inbox-file)
      (goto-char (point-max))
      (insert (format "* TODO %s\n" title))
      (schedule-at-point timestamp)))
  (message "-org-hs--add-scheduled-event"))

(defun org-hs--ws-on-close (_websocket)
  (message "org-hs--ws-on-close"))

(defun org-hs--get-agenda ()
  (let* ((encoded-agenda (json-encode (get-calendar-entries 'agenda))))
     (message (format "Length of encoded agenda=%d bytes" (length encoded-agenda)))
     (websocket-send-text org-hs-ws-socket encoded-agenda)))

(defun find-event-by-id (id)
  "Find a event by ID so we can modify it."
  (let* ((location (org-id-find id)))
    (find-file (car location))
    (goto-char (cdr location))))

(defun get-agenda ()
  "Get an org agenda event and transform it into a form that is easily JSONable."
                                        ; TODO: should we preserve the original value?
  (setq org-id-prefix "org-hs-id-custom")
                                        ; silently eat the error that org-id-get-create generates in temp buffers.
  (condition-case nil
      (org-id-get-create)
    (error nil))
  (setq org-id-prefix nil)
  ;; TODO: Make the ignore tag configurable 
  (org-set-tags (org-uniquify (cons "DO_NOT_ORG_ROAM" (org-get-tags))))
  (let* ((props (org-entry-properties))
         (json-null json-false)
         (js-date (get-js-date-pair )))
    ;(print props)
    (push `(startDate . ,(cdr (assoc 'startDate js-date))) props)
    (push `(endDate . ,(cdr (assoc 'endDate js-date))) props)
    (push `(allDay . ,(cdr (assoc 'allDay js-date))) props)
    (push `(isReadOnly . ,org-hyperscheduler-readonly-mode) props)
    props
    )
  )

(defun get-calendar-entries (scope)
  "Get all agenda entries using our filter and return a structure that is JSONable"
  (org-map-entries #'get-agenda org-hyperscheduler-agenda-filter scope))


(defun get-js-date-pair ()
  "Convert from org timestamp to the format that TUI.calendar expects"
  (let* ((plist (car (cdr (org-element-property :scheduled  (org-element-at-point)))))
         (plist (or plist (car (cdr (org-timestamp-from-string (org-entry-get nil "TIMESTAMP"))))))
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
    combined))

; from https://wilkesley.org/~ian/xah/emacs/elisp_datetime.html
(defun date-time-to-iso8601-js-like (seconds minutes hour day month year)
  "Convert time stamps to ISO8601 format"
  ;; (message (format "params %s %s %s %s %s %s" seconds minutes hour day month year))
  (let* ((minutes (or minutes 0))
         (hour (or hour 0)))
    (concat
     (format-time-string "%Y-%m-%dT%T"  (encode-time seconds minutes hour day month year))
     ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
      (format-time-string "%z")))))


(defun get-scheduled-timestamp-for-scheduled-event (start-time-stamp stop-time-stamp) 
  "Convert a unix time stamp back to org timestamp"
  (concat (format-time-string "<%Y-%m-%d %a %H:%M" (seconds-to-time start-time-stamp))
          (format-time-string "-%H:%M>" (seconds-to-time stop-time-stamp))))



(defun schedule-at-point (timestamp)
  "Schedule a heading at point with a given timestamp"
  (org-schedule nil timestamp))


;; --- deal with exporting links
;; WIP


(defun get-link-location ()
 (org-element-property :contents-begin (car (org-element-map (org-element-headline-parser (point)) 'link #'identity))))



(defun get-link-label ()
(let* ((link-location (get-link-location)))
(goto-char link-location)
(get-link-label--)))

(defun get-link-label-- ()
  (let ((link (org-element-context)))
     (buffer-substring-no-properties (org-element-property :contents-begin link)
                                    (org-element-property :contents-end link))))


;; --- end links

(defvar org-hyperscheduler-root-dir
  (concat (file-name-directory
           (expand-file-name (or
                    load-file-name
                    buffer-file-name)))
          "."))


;;;###autoload
(defun org-hyperscheduler-open ()
  "Open org-hyperscheduler in the browser."
  (interactive)
  (let ((html-file-path  (format "file://%s/calendar/index.html" org-hyperscheduler-root-dir)))
  (browse-url html-file-path)))

;;;###autoload
(defalias 'org-hs-open #'org-hyperscheduler-open)

(provide 'org-hyperscheduler)
