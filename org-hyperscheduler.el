;;; org-hyperscheduler.el --- UI (web) representation of org-agenda  -*- lexical-binding: t; -*-
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

(defcustom org-hyperscheduler-readonly-mode t
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


;; ---------------------------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------------------------

;; turn on logging and create org-hs--log-* methods
(log4e:deflogger "org-hs" "org-hyperscheduler %t [%l] %m" "%H:%M:%S")
(org-hs--log-enable-logging)
(org-hs--log-enable-messaging)

(defvar org-hyperscheduler-server-buffer (get-buffer-create "*org-hyperscheduler-server*"))
(defvar org-hyperscheduler-server-name "org-hyperscheduler-server")

;; modify the agenda filter if we want to hide done tasks.
(and org-hyperscheduler-hide-done-tasks (setq org-hyperscheduler-agenda-filter (format "%s/-DONE" org-hyperscheduler-agenda-filter)))

(defvar org-hyperscheduler-ws-server
  ;; only run the server if we are not in test env.
  (unless (boundp 'org-hyperscheduler-test-env)
    (websocket-server
     44445
     :host 'local
     :on-open #'org-hyperscheduler--ws-on-open
     :on-message #'org-hyperscheduler--ws-on-message
     :on-close #'org-hyperscheduler--ws-on-close)))

(defvar org-hyperscheduler-ws-socket nil)

(defun org-hyperscheduler-stop-server ()
  "Stops the websocket server and closed connections."
  (interactive)
  (websocket-server-close org-hyperscheduler-ws-server))

(defun org-hyperscheduler--ws-on-message (_ws frame)
  "Functions to run when the server receives a message.
Takes _WS and FRAME as arguments."
  (let* ((msg (json-parse-string
               (websocket-frame-text frame) :object-type 'alist))
         (command (alist-get 'command msg))
         (data (alist-get 'data msg)))
    (org-hs--log-debug (format "Command=[%s] Data=[%s]" command data))
    (cond ((string= command "get-agenda")
           (org-hyperscheduler--get-agenda))
          ((string= command "update-event")
           (org-hyperscheduler--update-event data))
          ((string= command "add-scheduled-event")
           (org-hyperscheduler--add-scheduled-event data))
          ((string= command "remove-event")
           (org-hyperscheduler--remove-event (alist-get 'id data)))
          (nil
           (org-hs--log-fatal
            "Something went wrong when receiving a message from org-hyperscheduler-ui")))))

(defun org-hyperscheduler--ws-on-open (ws)
  "Open the websocket WS and send initial data."
    (setq org-hyperscheduler-ws-socket ws)
    (org-hs--log-debug "org-hyperscheduler: connection from the browser"))

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

;; TODO: fix the event structure. Structure for the event is inconsistent between this and update event (eg start vs startUnix).
(defun org-hyperscheduler--add-scheduled-event (data)
  "Create a new event from DATA in an inbox."
  (org-hs--log-debug "+org-hyperscheduler--add-scheduled-event")
  (let* ((title (alist-get 'title data))
         (timestamp (org-hyperscheduler-get-scheduled-timestamp-for-scheduled-event (cdr (assoc 'startUnix data)) (cdr (assoc 'endUnix data)))))
    (save-window-excursion
      (find-file org-hyperscheduler-inbox-file)
      (goto-char (point-max))
      (insert (format "* TODO %s\n" title))
      (org-hyperscheduler-schedule-at-point timestamp)))
  (org-hs--log-debug "-org-hyperscheduler--add-scheduled-event"))

(defun org-hyperscheduler--remove-event (event_id)
  "Remove the heading specific by EVENT_ID (an org-id)."
  (save-window-excursion
    (org-hyperscheduler-find-event-by-id event_id)
    (org-cut-subtree)))

(defun org-hyperscheduler--ws-on-close (_websocket)
  "This the websocket connection callback."
  (org-hs--log-debug "org-hyperscheduler--ws-on-close"))

(defun org-hyperscheduler--encode-agenda ()
  "Encode our agenda to JSON."
  ;; want json-encode-array here in case we get an empty list. then we want "[]"
  (json-encode-array (org-hyperscheduler-get-calendar-entries 'agenda)))

(defun org-hyperscheduler--get-agenda ()
  "Get the agenda and send it through to the client."
  (let* ((encoded-agenda (org-hyperscheduler--encode-agenda))
         (response (concat "{\"agenda\":" encoded-agenda "}")))
     (org-hs--log-debug (format "Length of encoded agenda=%d bytes" (length encoded-agenda)))
     (websocket-send-text org-hyperscheduler-ws-socket response)))

(defun org-hyperscheduler-find-event-by-id (id)
  "Find the heading specified by ID and go to it."
  (let* ((location (org-id-find id)))
    (find-file (car location))
    (goto-char (cdr location))))

(defun org-hyperscheduler-get-agenda ()
  "Get an org agenda event and transform it into a form that is easily JSONable."
  ;; silently eat the error that org-id-get-create generates in temp buffers.
  ;; I'd like a custom prefix in case we ever have to filter all org-hs created properties out.
  (condition-case nil
      ; second param to org-id-get is whether to create an id or not
      (org-id-get (point) (not org-hyperscheduler-readonly-mode) "org-hyperscheduler-id")
    (error nil))
  ;; hide tasks from org-roam https://www.orgroam.com/manual.html#What-to-cache
  (when (and
         (not org-hyperscheduler-readonly-mode)
         org-hyperscheduler-exclude-from-org-roam)
    (org-entry-put (point) "ROAM_EXCLUDE" "t"))
  (let* ((props (org-entry-properties))
         (json-null json-false)
         (js-date (org-hyperscheduler-get-js-date-pair )))
    (push `(startDate . ,(cdr (assoc 'startDate js-date))) props)
    (push `(endDate . ,(cdr (assoc 'endDate js-date))) props)
    (push `(allDay . ,(cdr (assoc 'allDay js-date))) props)
    (push `(isReadOnly . ,org-hyperscheduler-readonly-mode) props)
    props))

(defun org-hyperscheduler-get-calendar-entries (scope)
  "Get all agenda entries using our filter and `org-mode' SCOPE.
Return a structure that is JSONable."
  (org-map-entries #'org-hyperscheduler-get-agenda org-hyperscheduler-agenda-filter scope))


(defun org-hyperscheduler-get-js-date-pair ()
  "Convert from org timestamp to the format that TUI.calendar expects."
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
(defun org-hyperscheduler-open ()
  "Open org-hyperscheduler in the browser."
  (interactive)
  (let ((html-file-path  (format "file://%s/calendar/index.html" org-hyperscheduler-root-dir)))
  (browse-url html-file-path)))

(provide 'org-hyperscheduler)

;;; org-hyperscheduler.el ends here
