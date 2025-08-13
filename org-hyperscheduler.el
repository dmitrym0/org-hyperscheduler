;;; -*- lexical-binding: t; -*-

;;; org-hyperscheduler.el --- UI (web) representation of org-agenda
;; Copyright © 2022 Dmitry Markushevich

;; Author: Dmitry Markushevich <dmitrym@gmail.com>
;; Keywords: org-mode, calendar
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (websocket "1.13") (log4e "0.3.3"))
;; URL: https://github.com/dmitrym0/org-hyperscheduler


;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;;
;; Shows your org-mode agenda in a web-based calendar view.


;;; Code:

;; ---------------------------------------------------------------------------------------------------
(require 'org)
(require 'org-element)
(require 'websocket)
(require 'cl-lib)
(require 'log4e)

;; ---------------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------------
;; options

(defgroup org-hyperscheduler nil
  "Org-hyperscheduler."
  :group 'org-hyperscheduler
  :prefix "org-hyperscheduler-"
  :link '(url-link :tag "Github" "https://github.com/dmitrym0/org-hyperscheduler"))

(defcustom org-hyperscheduler-readonly-mode  nil
  "If true, the web interface becomes read only.
In Read-only mode, changes to agenda entries can only be made from Emacs.

In Read-write mode, changes can be made either in Emacs or in the web-interface.

**NOTE**
For bidirectional changes to work each eligible agenda entry must have an ~ID~.

This org-id will be added automatically by org-hyperscheduler.
If you don't want org-hyperscheduler to modify your agenda entries,
keep the read-only mode enabled."
  :group 'org-hyperscheduler
  :type 'boolean)

(defcustom org-hyperscheduler-hide-done-tasks t
  "If true, once a task transitions from TODO to DONE it is hidden."
  :group 'org-hyperscheduler
  :type 'boolean)

(defcustom org-hyperscheduler-exclude-from-org-roam nil
  "In org-roam any entry with an :ID: property is treated like a node.
This is not desirable for calendar entries in most cases.

When this flag is set to true, org-hyperscheduler will insert a :ROAM_EXCLUDE:
property to hide calendar entries from org-roam.

Read-only mode (org-hyperscheduler-readonly-mode) needs to be disabled for
this setting to take effect."
  :group 'org-hyperscheduler
  :type 'boolean)

(defcustom org-hyperscheduler-agenda-filter "TIMESTAMP>=\"<2022-04-31>\"|SCHEDULED>=\"<2022-04-31>\""
  "Filter to generate a list of agenda entries to show in the calendar."
  :group 'org-hyperscheduler
  :type 'string)

(defcustom org-hyperscheduler-inbox-file (concat org-directory "/inbox.org")
  "This is the file where newly created entries go (the ones created in the WebUI."
  :group 'org-hyperscheduler
  :type 'string)

(defcustom org-hyperscheduler-default-calendar-view 'week
  "Default view for the web calendar: day, week, month"
  :options '(day week month)
  :group 'org-hyperscheduler)

(defcustom org-hyperscheduler-calendar-categories
  '(
    (work
     :name "Work Calendar"
     :color "#000000"
     :bg-color "#FFFF6e"
     :border-color "#FFFF6e"
     :read-only t
     :matcher (lambda (item) (string= (cdr (assoc "CALENDAR-ID" item))
                                      "dmitry@work.com")))


    (done
     :name "Done Items"
     :color "#000000"
     :bg-color "#C0C0C0"
     :border-color "#C0C0C0"
     :read-only t
     :matcher (lambda (item) (string= (cdr (assoc "TODO" item)) "DONE")))

    (timestamped
     :name "Timestamped Items"
     :color "#000000"
     :bg-color "#00a9ff"
     :border-color "#00a9ff"
     :read-only nil
     :matcher (lambda (item) (and (not (assoc "SCHEDULED" item))
                                  (assoc "TIMESTAMP" item))))
    (clocked
     :name "Clocked Time"
     :color "#000000"
     :bg-color "#e2fee2"
     :border-color "#e2fee2"
     :read-only t
     :matcher (lambda (item) (string= (cdr (assoc 'calendarId item)) "clocked")))

    (cancelled
     :name "Cancelled"
     :color "#000000"
     :bg-color "#FAA0A0"
     :border-color "#FAA0A0"
     :read-only t
     :matcher (lambda (item) (string= (cdr (assoc "TODO" item)) "CANCELLED")))
    (scheduled
     :name "Scheduled Items"
     :color "#ffffff"
     :bg-color "#9e5fff"
     :border-color "#9e5fff"
     :read-only nil
     :matcher (lambda (item) (assoc "SCHEDULED" item)))


    )
 "Alist defining calendar categories for visual classification and behavior of agenda items.

  Each category is a (SYMBOL . PLIST) pair where SYMBOL is the category identifier
  and PLIST contains the following properties:

    :name STRING          - Display name shown in the web interface
    :color STRING         - Text color (hex format, e.g., \"#000000\")
    :bg-color STRING      - Background color (hex format, e.g., \"#FFFF6e\")
    :border-color STRING  - Border color (hex format, e.g., \"#FFFF6e\")
    :read-only BOOLEAN    - Whether items can be modified via web interface
    :matcher FUNCTION     - Lambda that takes an agenda item and returns t if
                           the item belongs to this category

  The matcher function receives an agenda item as an alist of properties
  (e.g., (\"ITEM\" . \"Task title\"), (\"TODO\" . \"DONE\"), etc.) and should
  return non-nil if the item matches this category.

  Categories are evaluated in order, with the first matching category assigned
  to each agenda item. Items that don't match any category default to 'scheduled.

  Example category definition:
    (work
     :name \"Work Calendar\"
     :color \"#000000\"
     :bg-color \"#FFFF6e\"
     :border-color \"#FFFF6e\"
     :read-only t
     :matcher (lambda (item)
               (string= (cdr (assoc \"CALENDAR-ID\" item))
                        \"work@company.com\")))

  This configuration system allows visual distinction of different agenda item
  types (external calendar events, completed tasks, scheduled items, etc.)
  while controlling editing permissions through the web interface."

  :group 'org-hyperscheduler
  :type '(alist :key-type symbol
                :value-type (plist :options
                            ((:name string)
                             (:color string)
                             (:bg-color string)
                             (:border-color string)
                             (:read-only boolean)
                             (:matcher function)))))


(defconst org-hyperscheduler-logbook-drawer-start-re "^[ \t]*:LOGBOOK:[ \t]*$"
  "Regular expression matching the first line of a clock drawer.")


;; ---------------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------------

;; Custom logging buffer functions
(defvar org-hyperscheduler-log-buffer-name "*org-hyperscheduler-log*"
  "Name of the org-hyperscheduler log buffer.")

(defun org-hyperscheduler--get-log-buffer ()
  "Get or create the org-hyperscheduler log buffer."
  (get-buffer-create org-hyperscheduler-log-buffer-name))

(defun org-hyperscheduler--log-to-buffer (level message)
  "Log MESSAGE with LEVEL to the org-hyperscheduler log buffer."
  (let ((buffer (org-hyperscheduler--get-log-buffer))
        (timestamp (format-time-string "%H:%M:%S")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "[%s] [%s] %s\n" timestamp level message))
      ;; Keep buffer size reasonable (last 1000 lines)
      (when (> (count-lines (point-min) (point-max)) 1000)
        (goto-char (point-min))
        (forward-line 200)
        (delete-region (point-min) (point))))))

(defun org-hyperscheduler-log-debug (message)
  "Log debug MESSAGE to org-hyperscheduler buffer."
  (org-hyperscheduler--log-to-buffer "DEBUG" message))

(defun org-hyperscheduler-log-info (message)
  "Log info MESSAGE to org-hyperscheduler buffer."
  (org-hyperscheduler--log-to-buffer "INFO" message))

(defun org-hyperscheduler-log-error (message)
  "Log error MESSAGE to org-hyperscheduler buffer."
  (org-hyperscheduler--log-to-buffer "ERROR" message))

(defun org-hyperscheduler-show-log ()
  "Show the org-hyperscheduler log buffer."
  (interactive)
  (pop-to-buffer (org-hyperscheduler--get-log-buffer)))

(defun org-hyperscheduler-clear-log ()
  "Clear the org-hyperscheduler log buffer."
  (interactive)
  (with-current-buffer (org-hyperscheduler--get-log-buffer)
    (erase-buffer)))

;; Override log4e functions to use custom buffer
(defun org-hs--log-debug (msg &rest args)
  "Override log4e debug to use custom buffer."
  (org-hyperscheduler-log-debug (apply #'format msg args)))

(defun org-hs--log-info (msg &rest args)
  "Override log4e info to use custom buffer."
  (org-hyperscheduler-log-info (apply #'format msg args)))

(defun org-hs--log-error (msg &rest args)
  "Override log4e error to use custom buffer."
  (org-hyperscheduler-log-error (apply #'format msg args)))

(defun org-hs--log-fatal (msg &rest args)
  "Override log4e fatal to use custom buffer."
  (org-hyperscheduler-log-error (apply #'format msg args)))

;; turn on logging and create org-hs--log-* methods
(log4e:deflogger "org-hs" "org-hyperscheduler %t [%l] %m" "%H:%M:%S")
(org-hs--log-enable-logging)
(org-hs--log-enable-debugging)
(org-hs--log-enable-messaging)
(org-hs--log-set-level 'debug)
(org-hs--log-debug "org-hs")

(defvar org-hyperscheduler-server-buffer (get-buffer-create "*org-hyperscheduler-server*"))
(defvar org-hyperscheduler-server-name "org-hyperscheduler-server")

;; modify the agenda filter if we want to hide done tasks.
;;(and org-hyperscheduler-hide-done-tasks (setq org-hyperscheduler-agenda-filter (format "%s/-DONE" org-hyperscheduler-agenda-filter)))

(defvar org-hyperscheduler-ws-server nil
  "The WebSocket server instance.")

(defvar org-hyperscheduler-ws-socket nil)

(defvar org-hyperscheduler-server-port 44445
  "Port for the WebSocket server.")

;;;###autoload
(defun org-hyperscheduler-start-server ()
  "Start the org-hyperscheduler WebSocket server."
  (interactive)
  (when org-hyperscheduler-ws-server
    (org-hyperscheduler-stop-server))

  (setq org-hyperscheduler-ws-server
        (websocket-server
         org-hyperscheduler-server-port
         :host 'local
         :on-open #'org-hyperscheduler--ws-on-open
         :on-message #'org-hyperscheduler--ws-on-message
         :on-close #'org-hyperscheduler--ws-on-close))

  (org-hs--log-debug (format "org-hyperscheduler WebSocket server started on port %d" org-hyperscheduler-server-port))
  (message (format "org-hyperscheduler server started on ws://127.0.0.1:%d" org-hyperscheduler-server-port)))

(defun org-hyperscheduler-stop-server ()
  "Stop the org-hyperscheduler WebSocket server and close connections."
  (interactive)
  (when org-hyperscheduler-ws-server
    (websocket-server-close org-hyperscheduler-ws-server)
    (setq org-hyperscheduler-ws-server nil)
    (setq org-hyperscheduler-ws-socket nil)
    (org-hs--log-debug "org-hyperscheduler WebSocket server stopped")
    (message "org-hyperscheduler server stopped")))

(defun org-hyperscheduler-server-running-p ()
  "Return t if the org-hyperscheduler server is running."
  (and org-hyperscheduler-ws-server t))

(defun org-hyperscheduler--ws-on-message (_ws frame)
  "Functions to run when the server receives a message.
Takes _WS and FRAME as arguments."
  (let* ((msg (json-parse-string
               (websocket-frame-text frame) :object-type 'alist))
         (command (alist-get 'command msg))
         (data (alist-get 'data msg)))
    (org-hs--log-debug (format "Command=[%s] Data=[%s]" command data))
    (cond ((string= command "get-agenda")
           (let ((start-date (alist-get 'startDate data))
                 (end-date (alist-get 'endDate data)))
             (org-hyperscheduler--get-agenda start-date end-date)))
          ((string= command "update-event")
           (org-hyperscheduler--update-event data)
           (org-hyperscheduler--send-single-entry-update))
          ((string= command "add-scheduled-event")
           (org-hyperscheduler--respond-with "new-event" (org-hyperscheduler--add-scheduled-event data)))
          ((string= command "remove-event")
           (org-hyperscheduler--remove-event (alist-get 'id data)))
          ((string= command "get-settings")
           (org-hyperscheduler--send-settings))
          (nil
           (org-hs--log-fatal
            "Something went wrong when receiving a message from org-hyperscheduler-ui")))))

(defun org-hyperscheduler--ws-on-open (ws)
  "Open the websocket WS and send initial data."
    (setq org-hyperscheduler-ws-socket ws)
    (org-hs--log-debug "org-hyperscheduler: connection from the browser")
    (org-hyperscheduler--send-calendar-config))

(defun org-hyperscheduler--update-event (data)
  "Update the given event with the DATA provided."
  (org-hs--log-debug "+org-hyperscheduler-update-event")
  (let* ((id (alist-get 'id data))
         (timestamp (org-hyperscheduler-get-scheduled-timestamp-for-scheduled-event (alist-get 'start data) (alist-get 'end data))))
    (org-hs--log-debug (format "Updating ID: %s to timestamp: %s" id timestamp))
    (save-window-excursion
      (org-hyperscheduler-find-event-by-id id)
      (org-hyperscheduler-schedule-at-point timestamp)))
  (org-hs--log-debug "-org-hyperscheduler-update-event"))


(defun org-hyperscheduler--send-single-entry-update (&rest params)
  "Send a single updated agenda entry through the websocket."
  (interactive)
  (org-hs--log-debug "org-hyperscheduler--send-single-entry-update.")
  (when (websocket-openp org-hyperscheduler-ws-socket)
    (let* ((entry (org-hyperscheduler-get-agenda))
           (encoded-entry (json-encode entry))
           (calendar-config (org-hyperscheduler--get-calendar-config))
           (response (json-encode `((command . "update-single-entry")
                                   (entry . ,entry)
                                   (calendarConfig . ,calendar-config)))))
      (org-hs--log-debug (format "Sending single entry update: %s" (cdr (assoc "ITEM" entry))))
      (websocket-send-text org-hyperscheduler-ws-socket response))))

(defun org-hyperscheduler--invalidate-remote-agenda (&rest params)
  "Sends an invalidate event through the websocket."
  (interactive)
  (org-hs--log-debug "org-hyperscheduler--invalidate-remote-agenda.")
  (when (websocket-openp org-hyperscheduler-ws-socket)
    (org-hs--log-debug "invalidating remote agenda.")
    (websocket-send-text org-hyperscheduler-ws-socket "{\"command\":\"invalidate\"}")))


;; TODO: fix the event structure. Structure for the event is inconsistent between this and update event (eg start vs startUnix).
(defun org-hyperscheduler--add-scheduled-event (data)
  "Create & return a new event from DATA in an inbox."
  (org-hs--log-debug "+org-hyperscheduler--add-scheduled-event")
  (let* ((title (alist-get 'title data))
         (timestamp (org-hyperscheduler-get-scheduled-timestamp-for-scheduled-event (cdr (assoc 'startUnix data)) (cdr (assoc 'endUnix data)))))
    (save-window-excursion
      (find-file org-hyperscheduler-inbox-file)
      (goto-char (point-max))
      (insert (format "* TODO %s\n" title))
      (org-hyperscheduler-schedule-at-point timestamp)
      (org-previous-visible-heading 1)
      (org-hyperscheduler-get-agenda))))
  ;; (org-hs--log-debug "-org-hyperscheduler--add-scheduled-event"))

(defun org-hyperscheduler--remove-event (event_id)
  "Remove the heading specific by EVENT_ID (an org-id)."
  (save-window-excursion
    (org-hyperscheduler-find-event-by-id event_id)
    (org-cut-subtree)))

(defun org-hyperscheduler--ws-on-close (_websocket)
  "This the websocket connection callback."
  (org-hs--log-debug "org-hyperscheduler--ws-on-close"))

(defun org-hyperscheduler--encode-agenda (&optional start-date end-date)
  "Encode our agenda to JSON.
Optional START-DATE and END-DATE parameters filter the agenda to a specific window."
  ;; convert agenda list to an array so that json.el does it's thing
  (json-encode (cl-map 'array #'identity (org-hyperscheduler-get-calendar-entries 'agenda start-date end-date))))

(defun org-hyperscheduler--get-agenda (&optional start-date end-date)
  "Get the agenda and send it through to the client.
Optional START-DATE and END-DATE parameters filter the agenda to a specific window."
  (org-hyperscheduler-log-info (format "Getting agenda between %s and %s" start-date end-date))
  (let* ((encoded-agenda (org-hyperscheduler--encode-agenda start-date end-date))
         (response (concat "{\"agenda\":" encoded-agenda "}")))
     (org-hs--log-debug (format "Length of encoded agenda=%d bytes" (length encoded-agenda)))
     (websocket-send-text org-hyperscheduler-ws-socket response)))

(defun org-hyperscheduler--send-settings ()
  "Send settings to the web UI."
  (org-hs--log-debug "Sending settings")
  (websocket-send-text org-hyperscheduler-ws-socket (json-encode `((command . "update-settings")
                                                                   (data . ,(org-hyperscheduler--get-settings))))))

(defun org-hyperscheduler--send-calendar-config ()
  "Send calendar configuration to the frontend."
  (let* ((config (org-hyperscheduler--get-calendar-config))
         (response (json-encode `((command . "update-calendar-config")
                                 (data . ,config)))))
    (websocket-send-text org-hyperscheduler-ws-socket response)))

(defun org-hyperscheduler--send-ack ()
  "Send an ok to the client."
  (websocket-send-text org-hyperscheduler-ws-socket (json-encode `((command . "response")
                                                                   (data . "ok")))))

(defun org-hyperscheduler--respond-with (c d)
  "Send a COMMAND with DATA to the client."
  (message "-------")
  (websocket-send-text org-hyperscheduler-ws-socket (json-encode `((command . ,c)
                                                                   (data . ,d)))))
(defun org-hyperscheduler--build-date-filter (start-date end-date)
  "Build an org-ql date filter from START-DATE and END-DATE strings.
Dates should be in ISO8601 format (YYYY-MM-DD).
Returns nil if no valid date range is provided."
  (when (and start-date end-date)
    (let ((start-org-date (org-hyperscheduler--iso8601-to-org-date start-date))
          (end-org-date (org-hyperscheduler--iso8601-to-org-date end-date)))
      (when (and start-org-date end-org-date)
        `(or
          (and (ts-active :from ,start-org-date :to ,end-org-date))
          (and (scheduled :from ,start-org-date :to ,end-org-date))
          (and (deadline :from ,start-org-date :to ,end-org-date))
          (and (clocked :from ,start-org-date :to ,end-org-date)))))))

(defun org-hyperscheduler--iso8601-to-org-date (iso-date)
  "Convert ISO8601 date string (YYYY-MM-DD) to org-mode date format.
Returns nil if the date string is invalid."
  (when (and iso-date (string-match "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" iso-date))
    (let ((year (string-to-number (match-string 1 iso-date)))
          (month (string-to-number (match-string 2 iso-date)))
          (day (string-to-number (match-string 3 iso-date))))
      (format "<%04d-%02d-%02d>" year month day))))

(defun org-hyperscheduler--get-settings ()
  "Get current settings so they can be fired off to the UI"
  `((defaultCalendarView . week)
    (showDone . ,t)
    (showClocked . ,t)
    (calendarConfig . ,(org-hyperscheduler--get-calendar-config))))


(defun org-hyperscheduler-find-event-by-id (id)
  "Find the heading specified by ID and go to it."
  (let* ((location (org-id-find id)))
    (find-file (car location))
    (goto-char (cdr location))))

;; Matcher helper functions
(defun org-hyperscheduler--assign-calendar-category (agenda-item)
  "Assign calendar category to an agenda item based on configured matchers."
  (org-hyperscheduler-log-debug (format "🔍 assign-category called with item: %s"
                                         agenda-item))
  (let ((category-id 'scheduled))  ; default category
    (catch 'found
      (dolist (category org-hyperscheduler-calendar-categories)
        (let* ((category-name (car category))
               (category-config (cdr category))
               (matcher (plist-get category-config :matcher)))
          (org-hyperscheduler-log-debug (format "🟡 Checking category %s for item %s"
                                               category-name
                                               (cdr (assoc "ITEM" agenda-item))))
          (when (and matcher (funcall matcher agenda-item))
            (org-hyperscheduler-log-debug (format "✅ Matched category %s" category-name))
            (setq category-id category-name)
            (throw 'found category-id)))))
    (org-hyperscheduler-log-debug (format "📋 Final category for '%s': %s"
                                         (cdr (assoc "ITEM" agenda-item))
                                         category-id))
    category-id))

(defun org-hyperscheduler--get-calendar-config ()
  "Generate calendar configuration for the frontend."
  (mapcar (lambda (category)
            (let* ((category-name (car category))
                   (config (cdr category)))
              `((id . ,(symbol-name category-name))
                (name . ,(plist-get config :name))
                (color . ,(plist-get config :color))
                (bgColor . ,(plist-get config :bg-color))
                (borderColor . ,(plist-get config :border-color))
                (dragBgColor . ,(plist-get config :bg-color))
                (readOnly . ,(plist-get config :read-only)))))
          org-hyperscheduler-calendar-categories))

(defun org-hyperscheduler-get-agenda ()
  "Get an org agenda event and transform it into a form that is easily JSONable."
  ;; silently eat the error that org-id-get-create generates in temp buffers.
  ;; I'd like a custom prefix in case we ever have to filter all org-hs created properties out.
  (org-hyperscheduler-log-debug "😀 Getting agenda..")
  (condition-case nil
      ; second param to org-id-get is whether to create an id or not
      (org-id-get (point) (not org-hyperscheduler-readonly-mode) "org-hyperscheduler-id")
    (error nil))
  ;; hide tasks from org-roam https://www.orgroam.com/manual.html#What-to-cache
  (when (and
         (not org-hyperscheduler-readonly-mode)
         org-hyperscheduler-exclude-from-org-roam)
    (org-entry-put (point) "ROAM_EXCLUDE" "t"))
  ;;(message (format "headline -> %s (buffer %s)" (cdr (assoc "ITEM" (org-entry-properties))) (current-buffer)))
  (let* ((props (org-entry-properties))
         (json-null json-false)
         (js-date (org-hyperscheduler-get-js-date-pair-for-headline))
         (clocked-list (org-hyperscheduler--get-clocked-times-for-headline-js))
         (category-id (org-hyperscheduler--assign-calendar-category props))
         )
    (push `(startDate . ,(cdr (assoc 'startDate js-date))) props)
    (push `(endDate . ,(cdr (assoc 'endDate js-date))) props)
    (push `(allDay . ,(cdr (assoc 'allDay js-date))) props)
    ;; TODO list to array.
    (push `(clockedList . ,(cl-map 'array #'identity clocked-list)) props)
    ;; Add category information
    (push `(calendarId . ,(symbol-name category-id)) props)

    props))

(defun org-hyperscheduler-get-calendar-entries (scope &optional start-date end-date)
  "Get all agenda entries using our filter and `org-mode' SCOPE.
Optional START-DATE and END-DATE parameters filter the agenda to a specific window.
Return a structure that is JSONable."

  (org-hyperscheduler-log-debug "collecting agenda..")
  (org-hyperscheduler-log-debug (format "Date window: %s to %s" start-date end-date))

  (let* ((date-filter (org-hyperscheduler--build-date-filter start-date end-date))
         (query (if date-filter
                    `(and (or
                           (ts-active)
                           (clocked)
                           (and
                            (todo "TODO" "DONE")
                            (or
                             (scheduled) (planning))))
                          ,date-filter)
                  '(or
                    (ts-active)
                    (clocked)
                    (and
                     (todo "TODO" "DONE")
                     (or
                      (scheduled) (planning))))))
         (entries (org-ql-select org-agenda-files query :action #'org-hyperscheduler-get-agenda))
         (all-entries '()))



    (org-hyperscheduler-log-debug (format "There are %s entries found by jql" (length entries)))
    (org-hyperscheduler-log-debug (format "There are %s entries in all-entries" (length all-entries)))

    ;; Process each entry and extract clocked entries as separate events
    (dolist (entry entries)
      ;; Add the main entry
      (push entry all-entries)

      ;; Extract and add individual clocked entries if they exist
      (let ((clocked-list (cdr (assoc 'clockedList entry)))
            (category-id (cdr (assoc 'calendarId entry)))
            (item-title (cdr (assoc "ITEM" entry)))
            (item-id (cdr (assoc "ID" entry))))
        (when (and clocked-list (> (length clocked-list) 0))
          ;; Check if this category should show clocked events separately
          (let* ((category-symbol (intern category-id))
                 (category-config (cdr (assoc category-symbol org-hyperscheduler-calendar-categories))))
            (when t
              (dotimes (i (length clocked-list))
                (let* ((clocked-entry (aref clocked-list i))
                       (clocked-start (cdr (assoc 'startDate clocked-entry)))
                       (clocked-end (cdr (assoc 'endDate clocked-entry)))
                       (clocked-all-day (cdr (assoc 'allDay clocked-entry)))
                       (clocked-calendar-entry `(("ITEM" . ,(format "%s (Clocked)" item-title))
                                                ("ID" . ,(format "%s-clocked-%d" item-id i))
                                                ("TODO" . "")
                                                (startDate . ,clocked-start)
                                                (endDate . ,clocked-end)
                                                (allDay . ,clocked-all-day)
                                                (clockedList . [])
                                                (calendarId . "clocked")
                                                )))
                  (push clocked-calendar-entry all-entries))))))))

    (org-hyperscheduler-log-debug (format "There are %s entries processed agenda." (length all-entries)))
    (-each all-entries (lambda (entry)
                         (org-hyperscheduler-log-debug (format "-> %s" (json-encode entry)))))
    all-entries))




(defun org-hyperscheduler-get-js-date-pair-for-headline ()
  "Converts headline's timestamp into JS format."
  ;; get the date propertye
  (let* ((plist (car (cdr (org-element-property :scheduled  (org-element-at-point)))))
         (plist (or plist (car (cdr (org-timestamp-from-string (org-entry-get nil "TIMESTAMP")))))))
    (when plist
      (org-hyperscheduler-get-js-date-pair-from-plist plist))))


(defun org-hyperscheduler--get-clocked-times-for-headline-js ()
  "Generates a list of js-dates pairs from clocked entries.
   Suitable for sending over to the UI."
  (save-excursion
    (let ((js-dates '())
          (drawer (re-search-forward org-hyperscheduler-logbook-drawer-start-re (save-excursion (org-end-of-subtree)) t)))
      (when drawer
        ;; drawer gets us the end of the :LOGBOOK: so +1 should get us to the first :CLOCK: entry if it exists
        (goto-char (+ 1 drawer))
        (while (eq 'clock (car (org-element-at-point)))
          (push (org-hyperscheduler-get-js-date-pair-from-plist (car (cdr (org-element-property :value (org-element-at-point))))) js-dates)
          (forward-line))
        js-dates))))


(defun org-hyperscheduler-get-js-date-pair-from-plist (plist)
  "Convert from org timestamp to the format that TUI.calendar expects."
  (let* ((year-start (plist-get plist :year-start))
         (month-start (plist-get plist :month-start))
         (day-start (plist-get plist :day-start))
         (hour-start (plist-get plist :hour-start))
         (minute-start (plist-get plist :minute-start))
         (year-end (plist-get plist :year-end))
         (month-end (plist-get plist :month-end))
         (day-end (plist-get plist :day-end))
         (hour-end (plist-get plist :hour-end))
         (minute-end (plist-get plist :minute-end))
         (start (org-hyperscheduler-date-time-to-iso8601-js-like  0 minute-start hour-start day-start month-start year-start))
         (end (org-hyperscheduler-date-time-to-iso8601-js-like  0 minute-end hour-end day-end month-end year-end) )
         (all-day (if (not hour-start) "true" "false"))
         (combined `((startDate . ,start) ( endDate . ,end) (allDay . ,all-day))))
    combined))

;; from https://wilkesley.org/~ian/xah/emacs/elisp_datetime.html
(defun org-hyperscheduler-date-time-to-iso8601-js-like  (seconds minutes hour day month year)
  "Convert time stamps to ISO8601 format.
Argument SECONDS seconds.
Argument MINUTES minutes.
Argument HOUR minutes.
Argument DAY day of the month.
Argument MONTH month.
Argument YEAR year."
  ;; (message (format "params %s %s %s %s %s %s" seconds minutes hour day month year))
  (let* ((minutes (or minutes 0))
         (hour (or hour 0)))
    (concat
     (format-time-string "%Y-%m-%dT%T"  (encode-time seconds minutes hour day month year))
     ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
      (format-time-string "%z")))))

(defun org-hyperscheduler-get-scheduled-timestamp-for-scheduled-event (start-time-stamp stop-time-stamp)
  "Convert a unix START-TIME-STAMP and STOP-TIME-STAMP back to org format event."
  (concat (format-time-string "<%Y-%m-%d %a %H:%M" (seconds-to-time start-time-stamp))
          (format-time-string "-%H:%M>" (seconds-to-time stop-time-stamp))))

(defun org-hyperscheduler-schedule-at-point (timestamp)
  "Schedule a heading at point with a given TIMESTAMP."
  (org-schedule nil timestamp))

(defvar org-hyperscheduler-root-dir
  (concat (file-name-directory
           (expand-file-name (or
                    load-file-name
                    buffer-file-name)))
          "."))



;;;###autoload
(defun org-hyperscheduler-reload ()
  "Reload org-hyperscheduler cleanly for development."
  (interactive)
  (org-hs--log-debug "Reloading org-hyperscheduler...")

  ;; Stop server and clear state
  (when (org-hyperscheduler-server-running-p)
    (org-hyperscheduler-stop-server))
  (org-hyperscheduler-clear-hooks)
  (org-hyperscheduler-clear-all-advice)

  ;; Reset variables
  (setq org-hyperscheduler-ws-socket nil)
  (setq org-hyperscheduler-ws-server nil)

  ;; Reload the file
  (let ((file-path (concat org-hyperscheduler-root-dir "/org-hyperscheduler.el")))
    (if (file-exists-p file-path)
        (progn
          (unload-feature 'org-hyperscheduler)
          (load-file file-path)
          (require 'org-hyperscheduler)
          (org-hs--log-debug "org-hyperscheduler reloaded from file")
          (message "org-hyperscheduler reloaded successfully"))
      (progn
        ;; Fallback: just re-evaluate current buffer if file not found
        (eval-buffer)
        (org-hs--log-debug "org-hyperscheduler reloaded from buffer")
        (message "org-hyperscheduler reloaded from current buffer"))))

  ;; Restart server unless in test environment
  (unless (boundp 'org-hyperscheduler-test-env)
    (org-hyperscheduler-start-server)))

;;;###autoload
(defun org-hyperscheduler-open ()
  "Open org-hyperscheduler in the browser."
  (interactive)
  (unless (org-hyperscheduler-server-running-p)
    (org-hyperscheduler-start-server))
  (let ((html-file-path  (format "file://%s/calendar/index.html" org-hyperscheduler-root-dir)))
    (browse-url html-file-path)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode event management here
;;
;; i want the webview to update instantly when a task is changed in org-mode.
;;
;; the code below is authoritative, but currently:
;;
;; - =org-after-todo-state-change-hook= fires when =org-todo= is invoked. it might be better to advice that function instead
;; - =org-clock-out= hook is called when a timer completes -- like an org-pomodoro running out.
;; - =org-schedule= is advised because that's how i get the new schedule

;; List of hooks that should invalidate the agenda on the browser side.
(setq org-hyperscheduler-agenda-invalidating-hooks '(
;;                                                      org-timer-done-hook
;;                                                      org-clock-out-hook
;;                                                      org-trigger-hook
;;                                                      org-property-changed-functions
;;                                                      org-insert-heading-hook
                                                     ))

;; List of hooks that should send single entry updates
(setq org-hyperscheduler-single-entry-hooks '(org-after-todo-state-change-hook
                                               org-clock-out-hook))


(defun org-hyperscheduler-update-remote-agenda (&rest params)
  "Ask web UI to update it's agenda."
  (org-hs--log-debug "org-hyperscheduler-update-remote-agenda")
  (org-hyperscheduler--send-single-entry-update))

(defun org-hyperscheduler-clear-all-advice ()
  "Remove all org-hyperscheduler advice from org-schedule."
  (interactive)
  (advice-remove 'org-schedule #'org-hyperscheduler-update-remote-agenda)
  (advice-remove 'org-schedule #'dm-echo)
  (org-hs--log-debug "Cleared all advice from org-schedule"))

(defun org-hyperscheduler-add-schedule-advice ()
  "Add advice to org-schedule to trigger single entry updates."
  (interactive)
  (advice-add 'org-schedule :after #'org-hyperscheduler-update-remote-agenda)
  (org-hs--log-debug "Added advice to org-schedule"))


(defun org-hyperscheduler-clear-hooks ()
  "Remove all hooks that org-hyperscheduler binds to."
  (org-hs--log-debug "♻️ 🗑️ org-hyperscheduler-clear-hooks")
  (dolist (hook-to-bind-to org-hyperscheduler-agenda-invalidating-hooks)
    (remove-hook hook-to-bind-to  #'org-hyperscheduler--invalidate-remote-agenda))
  (dolist (hook-to-bind-to org-hyperscheduler-single-entry-hooks)
    (remove-hook hook-to-bind-to  #'org-hyperscheduler--send-single-entry-update)))


(defun org-hyperscheduler-register-hooks ()
  "Bind hooks to invalidate agenda on the browser side."
  (dolist (hook-to-bind-to org-hyperscheduler-agenda-invalidating-hooks)
    (add-hook hook-to-bind-to  #'org-hyperscheduler--invalidate-remote-agenda))
  (dolist (hook-to-bind-to org-hyperscheduler-single-entry-hooks)
    (add-hook hook-to-bind-to  #'org-hyperscheduler--send-single-entry-update)))


;; Clear any existing advice and register fresh hooks
(org-hyperscheduler-clear-all-advice)
(org-hyperscheduler-register-hooks)
(org-hyperscheduler-add-schedule-advice)

;; Auto-start server unless in test environment
(unless (boundp 'org-hyperscheduler-test-env)
  (org-hyperscheduler-start-server))


(provide 'org-hyperscheduler)
;;; org-hyperscheduler.el ends here
