;;; -*- lexical-binding: t; -*-


(when (require 'undercover nil t)
  (setq undercover-force-coverage t)

  (message "Enable test coverage.")
  (undercover "*.el"
              (:report-format 'lcov)
              (:send-report nil)))



(setq org-hyperscheduler-test-env t)

(require 'org-hyperscheduler)
(require 'org-id)
(require 'org-ql)



(defvar mock-org-contents
"* TODO a task aaa
SCHEDULED: <2022-01-23 Sun>
:PROPERTIES:
:ID:       FAKE_ID0
:END:
* TODO a second task
SCHEDULED: <2022-01-23 Sun 14:00-15:00>
:PROPERTIES:
:ID:       FAKE_ID1
:END:
")


(defvar mock-org-contents-org-ql-test
"* This heading should be returned because it has a time stamp
:PROPERTIES:
:ID:       org-hyperscheduler-id:99F9F5B6-5DF8-4E07-8BA1-1AF24361B266
:ROAM_EXCLUDE: t
<2022-09-06 Tue 18:30>
:END:
* This heading should NOT be there because it doesn't have any scheduling info
:PROPERTIES:
:END:
* This heading should be there because it's got clocking info
:LOGBOOK:
CLOCK: [2022-09-06 Tue 16:34]--[2022-09-06 Tue 17:03] =>  0:29
CLOCK: [2022-09-06 Tue 16:02]--[2022-09-06 Tue 16:31] =>  0:29
:END:
* TODO This heading shouldn't be there because it doesn't have any scheduling info
* TODO This heading should be there because it contains scheduling info with time
SCHEDULED: <2026-01-23 Sun 14:00-15:00>
:PROPERTIES:
:ID:       FAKE_ID1
:END:
* TODO This heading should be there because it contains scheduling info, all day
SCHEDULED: <2026-01-23 Sun>
:PROPERTIES:
:ID:       FAKE_ID2
:END:
* This heading shouldn't be there because it has an inactive time stamp
[2022-09-06 Tue]
* DONE This heading should be there because it's done and scheduled so we want it to appear greyed out on the calendar.
SCHEDULED: <2022-11-02 Wed 09:30-10:00>
:PROPERTIES:

")

(defvar mock-org-contents-windowed-test
"* TODO Task in January 2022
SCHEDULED: <2022-01-15 Sat 10:00-11:00>
:PROPERTIES:
:ID:       WINDOW_TEST_1
:END:
* TODO Task in February 2022
SCHEDULED: <2022-02-15 Tue 14:00-15:00>
:PROPERTIES:
:ID:       WINDOW_TEST_2
:END:
* TODO Task in March 2022
SCHEDULED: <2022-03-15 Tue 16:00-17:00>
:PROPERTIES:
:ID:       WINDOW_TEST_3
:END:
* TODO Task in December 2025
SCHEDULED: <2025-12-25 Thu 09:00-10:00>
:PROPERTIES:
:ID:       WINDOW_TEST_4
:END:
")

(defvar mock-org-contents-unprocessed
  "* TODO a task aaa
SCHEDULED: <2022-01-23 Sun>
")


(org-hs--log-set-level 'debug)



(message "org-agenda-files: %s" org-agenda-files)

;; this is how I ship org-hs: readonly mode enabled, org-roam exclusion disabled.
(defun org-hs-default-state ()
  (setq org-hyperscheduler-exclude-from-org-roam nil)
  (setq org-hyperscheduler-readonly-mode t))


(setq org-hyperscheduler-agenda-filter "TIMESTAMP>=\"<2022-01-01>\"|SCHEDULED>=\"<2022-01-01>\"")
(setq tempfile "")
(setq org-hs-test-file nil)


;; this is a convenience method for unit tests. sets up a temp buffer with mock contents and execs the lambda
;;
;; initially tried to use with-temp-buffer, but org-id doesn't like transient buffers and seems to need
;; actual files. so here we are.
;;
;; TODO: asserts can fail and that kills the stack, catch the exception so that we can do proper cleanup.
(defun with-mock-contents (contents lambda)

  (setq org-id-locations-file (make-temp-file "org-hyperscheduler-id-locations-file"))

  ;;  (setenv "TZ" "America/Los_Angeles")
  ;; set time zone. all expectations are for PST.
  ; (set-time-zone-rule "US/Pacific")

  (set-time-zone-rule '(-25200 "PST"))

  (message "--------------------------------------------")
  (message (format "\nCurrent time zone: %s\n" (current-time-zone)))




  ;; TODO: when an assert fails in buttercup, an exception (??) is thrown,
  ;; so temp file isnt being cleaned up. This is the sledgehammer approach.
  ;; Needs to be fixed so that it's cleaned up properly.
  (when org-hs-test-file
    (progn
      (message (format "Removing org-hs-test-file: %s\n" org-hs-test-file))
      (delete-file org-hs-test-file)))

  (setq tempfile (make-temp-file "org-hs" nil ".org" contents))

  (message (format "Creating a tempfile: %s\n" tempfile))
  (setq org-agenda-files (list tempfile))
  (setq org-hs-test-file tempfile)
  (setq org-id-track-globally t)
  (org-mode)
  (message "Opening the file..")
  (find-file tempfile)
  (org-element-cache-reset)
  (message "Starting the test..")
  (funcall lambda)
  (message "About to kill buffer..")
  (kill-current-buffer)
  (message (format "Removing tempfile %s" tempfile))
  (delete-file tempfile)
  (message "+++++++++++++++++++++++++++++++++++++++++")
  )

; utility method to generate websocket frame for later consumption.
(defun make-ws-frame (payload)
  (message "Payload: %s" payload)
  (websocket-read-frame (websocket-encode-frame
                         (make-websocket-frame :opcode 'text
                                               :payload (encode-coding-string payload 'raw-text)
                                               :completep t)
                         t)))


(describe "Agenda functionality"
          (it "can get the correct entries"
              (with-mock-contents
               mock-org-contents
               (lambda () (let ((number-of-todo-entries (length (org-hyperscheduler-get-calendar-entries nil))))
                             (expect number-of-todo-entries :to-be 2))))
              )


          (it "has the correct properties"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                  (second-entry (car (cdr todo-entries))))
                  (message (format "---> %s" todo-entries))
                  (expect (cdr (assoc "ID" second-entry)) :to-equal "FAKE_ID0")))))


          (it "can produce a json representation"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                (let ((json-representation (json-encode (org-hyperscheduler-get-calendar-entries nil))))
                  (expect (string-match "a task aaa" json-representation) :not :to-be nil)))))



          (it "can insert org-id into a heading"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (condition-case nil
                    (org-id-get-create)
                  (error nil))
                (expect (org-id-get-create) :not :to-be nil)))

          (it "can reset org-id-prefix"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (setq original org-id-prefix
                )))



          (it "can encode empty agenda correctly"
              (with-mock-contents "" (lambda ()
                                        (let* ((encoded-agenda (org-hyperscheduler--encode-agenda)))
                                          (expect encoded-agenda :to-equal "[]")))))


          (it "can update an existing scheduled event"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                  (org-hyperscheduler--update-event '(( id . "FAKE_ID1") (start . 1904247000 ) (end . 1904547000)))
                  (org-hyperscheduler-find-event-by-id "FAKE_ID1")
                  (let* ((plist (car (cdr (org-element-property :scheduled  (org-element-at-point)))))
                         (rawvalue (plist-get plist :raw-value)))
                    ;(message "%s" plist)
                    (expect rawvalue :to-equal "<2030-05-05 Sun 14:30-01:50>")))))

          (it "can insert a new scheduled event into the list"
              (with-mock-contents
               ""
               (lambda ()
                  (let* ((old_inbox org-hyperscheduler-inbox-file))
                    (setq org-hyperscheduler-inbox-file tempfile) ;; tempfile is set in with-mock-contents
                    (let* ((new-event (org-hyperscheduler--add-scheduled-event '(( title . "test") (startUnix . 1904247000 ) (endUnix . 1904547000)))))

                    (expect (alist-get "ITEM" new-event nil nil 'equal) :to-equal "test")
                    (expect (alist-get 'startDate new-event) :to-equal "2030-05-05T14:30:00-07:00"))

                    ; we have to reset the inbox path, because "defaults" checks for it.
                    (setq org-hyperscheduler-inbox-file old_inbox)))))


          (it "can deal with clocked entries"
              (with-mock-contents
               mock-org-contents-org-ql-test
               (lambda ()
                 ;; Navigate to the heading with clocked entries
                 (goto-char (point-min))
                 (re-search-forward "This heading should be there because it's got clocking info")
                 (let* ((agenda-entry (org-hyperscheduler-get-agenda))
                        (clocked-list (cdr (assoc 'clockedList agenda-entry))))
                   ;; Should have clocked times
                   (expect clocked-list :not :to-be nil)
                   (expect (length clocked-list) :to-be 2)

                   ;; Verify the first clocked entry (most recent)
                   (let ((first-clocked-item (aref clocked-list 1)))
                     (expect (cdr (assoc 'startDate first-clocked-item)) :to-equal "2022-09-06T16:34:00-07:00")
                     (expect (cdr (assoc 'endDate first-clocked-item)) :to-equal "2022-09-06T17:03:00-07:00")
                     (expect (cdr (assoc 'allDay first-clocked-item)) :to-equal "false"))

                   ;; Verify the second clocked entry
                   (let ((second-clocked-item (aref clocked-list 0)))
                     (expect (cdr (assoc 'startDate second-clocked-item)) :to-equal "2022-09-06T16:02:00-07:00")
                     (expect (cdr (assoc 'endDate second-clocked-item)) :to-equal "2022-09-06T16:31:00-07:00")
                     (expect (cdr (assoc 'allDay second-clocked-item)) :to-equal "false"))))))

          (it "can deal with no clocked entries"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 ;; Navigate to a heading without clocked entries
                 (goto-char (point-min))
                 (re-search-forward "a task aaa")
                 (let* ((agenda-entry (org-hyperscheduler-get-agenda))
                        (clocked-list (cdr (assoc 'clockedList agenda-entry))))
                   ;; Should have clockedList property, but it should be empty
                   (expect clocked-list :not :to-be nil)
                   (expect (length clocked-list) :to-be 0)))))

          (it "can insert a new timestamped event into the list")
          (it "can delete an existing event from the list"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                  (expect (org-id-find "FAKE_ID1") :not :to-be nil)
                  (org-hyperscheduler--remove-event "FAKE_ID1")
                  (expect (org-id-find "FAKE_ID1") :to-be nil))))


)


(describe "read write mode"
          (before-all
              (setq org-hyperscheduler-exclude-from-org-roam t)
              (setq org-hyperscheduler-readonly-mode nil))


          (after-all
           (org-hs-default-state))

          (it "has roam ignore property"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                  (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                         (roam-ignore-prop  (org-entry-get (point) "ROAM_EXCLUDE")))
                    (expect roam-ignore-prop :not :to-be nil)))))

          (it "has the correct ID prefix"
              (with-mock-contents
               mock-org-contents-unprocessed
               (lambda ()
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                       (current-id (org-id-get)))
                  (expect (string-match "org-hyperscheduler-id.*" current-id))))))

          (it "does NOT have ROAM_EXCLUDE property when exclusion is disabled"
              (setq org-hyperscheduler-exclude-from-org-roam nil)
              (with-mock-contents
               mock-org-contents
               (lambda ()

                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                       (roam-ignore-prop  (org-entry-get (point) "ROAM_EXCLUDE")))
                  (expect roam-ignore-prop :to-be nil)))))
          )




(describe "readonly mode"

          (before-all
           (org-hs-default-state))


          (after-all
           (org-hs-default-state))

          (it "does NOT have a generated ID"
              (with-mock-contents
               mock-org-contents-unprocessed
               (lambda ()

                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                       (current-id (org-id-get)))
                  (expect current-id :to-be nil)))))


          (it "does NOT have ROAM_EXCLUDE property when the flag is set in READONLY mode"
              (setq org-hyperscheduler-exclude-from-org-roam t)
              (with-mock-contents
               mock-org-contents
               (lambda ()
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                       (roam-ignore-prop  (org-entry-get (point) "ROAM_EXCLUDE")))
                  (expect roam-ignore-prop :to-be nil)))))
          )


(describe "ISO8601 date formatting"
          (it "can parse the dates correctly"
          (setq org-id-track-globally nil)
          (with-temp-buffer
            (insert mock-org-contents)
            (org-mode)
            (org-previous-visible-heading 1)
            (let ((js-date (org-hyperscheduler-get-js-date-pair-for-headline)))
              (expect (cdr (assoc 'startDate js-date)) :to-equal "2022-01-23T14:00:00-07:00")
              (expect (cdr (assoc 'endDate js-date)) :to-equal "2022-01-23T15:00:00-07:00")
              (expect (cdr (assoc 'allDay js-date)) :to-equal "false")
              )
            )
          )


          (it "can detect all day correctly"
          (setq org-id-track-globally nil)
          (with-temp-buffer
            (insert mock-org-contents)
            (org-mode)
            (org-next-visible-heading -1) ;; seems kinda flaky?
            (beginning-of-buffer)
            (let ((js-date (org-hyperscheduler-get-js-date-pair-for-headline)))
              (expect (cdr (assoc 'allDay js-date)) :to-equal "true")
              )
            )
          ))


;; note that this is not technically a correct result, because i"m hardcoding the pacific summer time (see set-time-zone above.)
(describe "time stamp generation"
          (it "can create a proper emacs timestamp from unix timestamp"
              (expect (org-hyperscheduler-get-scheduled-timestamp-for-scheduled-event 1643657400 (seconds-to-time 1643757400)) :to-equal "<2022-01-31 Mon 12:30-16:16>")))





(describe "webservices functionality"
          (before-each
           ;; intercept websocket-send-text.
           (spy-on 'websocket-send-text :and-call-fake (lambda (socket text)
                                                         (setq __response text)
                                                         )))

          ;; TODO Use the with-mock-contents to insert actual agenda and verify the result
          (it "can get agenda via websocket"
              (setq org-agenda-files nil)
              (org-element-cache-reset t)
              (let* ((frame (make-ws-frame "{\"command\":\"get-agenda\"}")))
                (org-hyperscheduler--ws-on-message nil frame)
                (expect 'websocket-send-text :to-have-been-called)
                ; agenda is set in ~before-each~
                (expect __response :to-equal "{\"agenda\":[]}") ;; no agenda, empty array
                ))


          (it "can remove events by id via websocket"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                  (org-hyperscheduler--ws-on-message nil (make-ws-frame "{\"command\":\"remove-event\", \"data\":{\"id\":\"FAKE_ID1\"}}"))
                  (expect (org-id-find "FAKE_ID1") :to-be nil))))


          (it "can send settings."
              (let* ((frame (make-ws-frame "{\"command\":\"get-settings\"}")))
                (org-hyperscheduler--ws-on-message nil frame)
                (expect 'websocket-send-text :to-have-been-called)
                ; agenda is set in ~before-each~
                (expect (string-match "defaultCalendarView.*" __response))
                ))


)



(describe "defaults"
          (it "should have a default for the inbox file"
              (expect org-hyperscheduler-inbox-file :to-equal "~/org/inbox.org"))
          (it "should have a default for the agenda filter"
              (expect org-hyperscheduler-agenda-filter :not :to-be nil))
          )


(describe "misc"
          (it "should find IDs in transient buffers"
              (with-mock-contents mock-org-contents (lambda() (org-hyperscheduler-find-event-by-id "FAKE_ID1")))
              ))


(describe "settigns"
          (it "should be able to get settings"
              (expect (org-hyperscheduler--get-settings) :not :to-be nil)))


(describe "org-hyperscheduler-open functionality"
          (before-each
           ;; Mock browse-url to avoid actually opening a browser
           (spy-on 'browse-url :and-call-fake (lambda (url)
                                                (setq __opened-url url)))
           ;; Use a different port for tests to avoid conflicts
           (setq org-hyperscheduler-server-port (+ 44445 (random 1000))))

          (after-each
           ;; Always clean up server after each test
           (when (org-hyperscheduler-server-running-p)
             (org-hyperscheduler-stop-server)))

          (it "can open org-hyperscheduler and start server if not running"
              (let ((org-hyperscheduler-test-env t))
                ;; Ensure server is stopped initially
                (when (org-hyperscheduler-server-running-p)
                  (org-hyperscheduler-stop-server))

                ;; Verify server is not running
                (expect (org-hyperscheduler-server-running-p) :to-be nil)

                ;; Call org-hyperscheduler-open
                (org-hyperscheduler-open)

                ;; Verify server was started
                (expect (org-hyperscheduler-server-running-p) :to-be t)

                ;; Verify browser was opened with correct URL
                (expect 'browse-url :to-have-been-called)
                (expect __opened-url :to-match "file://.*calendar/index.html")))

          (it "can open org-hyperscheduler when server is already running"
              (let ((org-hyperscheduler-test-env t))
                ;; Start server first
                (org-hyperscheduler-start-server)
                (expect (org-hyperscheduler-server-running-p) :to-be t)

                ;; Call org-hyperscheduler-open
                (org-hyperscheduler-open)

                ;; Verify server is still running
                (expect (org-hyperscheduler-server-running-p) :to-be t)

                ;; Verify browser was opened
                (expect 'browse-url :to-have-been-called)
                (expect __opened-url :to-match "file://.*calendar/index.html"))))


(describe "windowed agenda functionality"
          (it "can parse ISO8601 dates correctly"
              (expect (org-hyperscheduler--iso8601-to-org-date "2022-01-23") :to-equal "<2022-01-23>")
              (expect (org-hyperscheduler--iso8601-to-org-date "2022-12-31") :to-equal "<2022-12-31>")
              (expect (org-hyperscheduler--iso8601-to-org-date "invalid") :to-be nil)
              (expect (org-hyperscheduler--iso8601-to-org-date nil) :to-be nil))

          (it "can build date filters correctly"
              (let ((filter (org-hyperscheduler--build-date-filter "2022-01-01" "2022-01-31")))
                (expect filter :not :to-be nil)
                (expect (car filter) :to-equal 'or)))

          (it "returns nil for invalid date ranges"
              (expect (org-hyperscheduler--build-date-filter nil nil) :to-be nil)
              (expect (org-hyperscheduler--build-date-filter "invalid" "2022-01-31") :to-be nil)
              (expect (org-hyperscheduler--build-date-filter "2022-01-01" "invalid") :to-be nil))

          (it "can get windowed agenda entries"
              (with-mock-contents
               mock-org-contents-windowed-test
               (lambda ()
                 (let ((windowed-entries (org-hyperscheduler-get-calendar-entries nil "2022-01-01" "2022-01-31"))
                       (all-entries (org-hyperscheduler-get-calendar-entries nil)))
                   ;; Should have entries within the date range
                   (expect (length windowed-entries) :to-be-greater-than 0)
                   ;; Windowed entries should be subset of all entries
                   (expect (length windowed-entries) :to-equal 1)))))

          (it "returns empty list for date range with no entries"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 (let ((windowed-entries (org-hyperscheduler-get-calendar-entries nil "2030-01-01" "2030-01-31")))
                   ;; Should have no entries in future date range
                   (expect (length windowed-entries) :to-be 0)))))

          (it "can handle websocket message with date parameters"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 (spy-on 'websocket-send-text :and-call-fake (lambda (socket text)
                                                               (setq __response text)))
                 (let* ((frame (make-ws-frame "{\"command\":\"get-agenda\", \"data\":{\"startDate\":\"2022-01-01\", \"endDate\":\"2022-01-31\"}}")))
                   (org-hyperscheduler--ws-on-message nil frame)
                   (expect 'websocket-send-text :to-have-been-called)
                   (expect (string-match "agenda" __response) :not :to-be nil)))))

          (it "can handle websocket message without date parameters (backward compatibility)"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 (spy-on 'websocket-send-text :and-call-fake (lambda (socket text)
                                                               (setq __response text)))
                 (let* ((frame (make-ws-frame "{\"command\":\"get-agenda\"}")))
                   (org-hyperscheduler--ws-on-message nil frame)
                   (expect 'websocket-send-text :to-have-been-called)
                   (expect (string-match "agenda" __response) :not :to-be nil)))))

          (it "can filter entries by specific date windows"
              (with-mock-contents
               mock-org-contents-windowed-test
               (lambda ()
                 ;; Test January 2022 window - should get 1 entry
                 (let ((jan-entries (org-hyperscheduler-get-calendar-entries nil "2022-01-01" "2022-01-31")))
                   (expect (length jan-entries) :to-be 1)
                   (expect (cdr (assoc "ID" (car jan-entries))) :to-equal "WINDOW_TEST_1"))

                 ;; Test February-March 2022 window - should get 2 entries
                 (let ((feb-mar-entries (org-hyperscheduler-get-calendar-entries nil "2022-02-01" "2022-03-31")))
                   (expect (length feb-mar-entries) :to-be 2))

                 ;; Test 2025 window - should get 1 entry
                 (let ((future-entries (org-hyperscheduler-get-calendar-entries nil "2025-01-01" "2025-12-31")))
                   (expect (length future-entries) :to-be 1)
                   (expect (cdr (assoc "ID" (car future-entries))) :to-equal "WINDOW_TEST_4"))

                 ;; Test narrow window with no entries
                 (let ((empty-entries (org-hyperscheduler-get-calendar-entries nil "2023-01-01" "2023-01-31")))
                   (expect (length empty-entries) :to-be 0))))))


(describe "calendar configuration"
          (before-each
           ;; override work calendar matcher
              (plist-put (cdr (assoc 'work org-hyperscheduler-calendar-categories))
                         :matcher
                         `(lambda (item) (string= (cdr (assoc "CALENDAR-ID" item)) "dmitry@work.com")))
           )
          (it "can assign work calendar category correctly"
              (let ((work-item '(("ITEM" . "Work meeting")
                                 ("TODO" . "TODO")
                                 ("CALENDAR-ID" . "dmitry@work.com")))
                    (non-work-item '(("ITEM" . "Personal task")
                                    ("TODO" . "TODO")
                                    ("SCHEDULED" . "<2024-01-15 Mon 10:00>"))))

                ;; Test work item gets work category
                (expect (org-hyperscheduler--assign-calendar-category work-item) :to-equal 'work)

                ;; Test non-work item gets scheduled category (default for scheduled items)
                (expect (org-hyperscheduler--assign-calendar-category non-work-item) :to-equal 'scheduled)))

          (it "can generate work calendar config correctly"
              (let ((calendar-config (org-hyperscheduler--get-calendar-config))
                    (work-config nil))
                ;; Find the work calendar in the config
                (dolist (config calendar-config)
                  (when (string= (cdr (assoc 'id config)) "work")
                    (setq work-config config)))

                ;; Verify work calendar config exists and has correct properties
                (expect work-config :not :to-be nil)
                (expect (cdr (assoc 'name work-config)) :to-equal "Work Calendar")
                (expect (cdr (assoc 'bgColor work-config)) :to-equal "#FFFF6e")
                (expect (cdr (assoc 'readOnly work-config)) :to-equal t)))

          (it "can handle work calendar items in full agenda flow"

              (message (format "🟡 %s" org-hyperscheduler-calendar-categories))

              (with-mock-contents
               "* TODO Work meeting with team
SCHEDULED: <2022-01-23 Sun 14:00-15:00>
:PROPERTIES:
:ID:       WORK_ITEM_TEST
:CALENDAR-ID: dmitry@work.com
:END:
"
               (lambda ()
                 (let* ((agenda-entries (org-hyperscheduler-get-calendar-entries nil))
                        (work-entry (car agenda-entries)))
                   ;; Should have one entry
                   (expect (length agenda-entries) :to-be 1)
                   ;; Should be assigned to work calendar
                   (expect (cdr (assoc 'calendarId work-entry)) :to-equal "work")
                   ;; Should have correct title
                   (expect (cdr (assoc "ITEM" work-entry)) :to-equal "Work meeting with team")
                   ;; Should have calendar-id property
                   (expect (cdr (assoc "CALENDAR-ID" work-entry)) :to-equal "dmitry@work.com")))))

          (it "can encode calendar configuration correctly in JSON"
              (let* ((calendar-config (org-hyperscheduler--get-calendar-config))
                     (json-config (json-encode calendar-config)))
                ;; Verify JSON contains expected structure
                (expect (string-match "\"id\":\"work\"" json-config) :not :to-be nil)
                (expect (string-match "\"name\":\"Work Calendar\"" json-config) :not :to-be nil)
                (expect (string-match "\"bgColor\":\"#FFFF6e\"" json-config) :not :to-be nil)
                (expect (string-match "\"id\":\"scheduled\"" json-config) :not :to-be nil)
                (expect (string-match "\"name\":\"Scheduled Items\"" json-config) :not :to-be nil)))

          (it "can send calendar configuration via websocket"
              (spy-on 'websocket-send-text :and-call-fake (lambda (socket text)
                                                            (setq __response text)))
              (setq org-hyperscheduler-ws-socket t)
              (org-hyperscheduler--send-calendar-config)


              (message "-> %s" __response)

              ;; Verify websocket message was sent
              (expect 'websocket-send-text :to-have-been-called)
              (expect __response :to-match "update-calendar-config")
              (expect __response :to-match "Work Calendar")
              (expect __response :to-match "work")
              (expect __response :to-match "#FFFF6e")))

(describe "single entry updates"
          (before-each
           ;; intercept websocket-send-text.
           (spy-on 'websocket-send-text :and-call-fake (lambda (socket text)
                                                         (setq __response text)
                                                         )))

          (it "can send single entry updates via websocket"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 (setq org-hyperscheduler-ws-socket t)
                 (spy-on 'websocket-openp :and-return-value t)

                 ;; Navigate to first heading
                 (goto-char (point-min))
                 (re-search-forward "a task aaa")

                 ;; Call single entry update
                 (org-hyperscheduler--send-single-entry-update)

                 ;; Verify websocket message was sent
                 (expect 'websocket-send-text :to-have-been-called)
                 (expect __response :to-match "update-single-entry")
                 (expect __response :to-match "a task aaa"))))

          (it "can handle update-event websocket messages that trigger single entry updates"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 (let* ((frame (make-ws-frame "{\"command\":\"update-event\", \"data\":{\"id\":\"FAKE_ID1\", \"start\":1643657400, \"end\":1643661000}}")))
                   ;; Mock the single entry update function
                   (spy-on 'org-hyperscheduler--send-single-entry-update)

                   ;; Process the websocket message
                   (org-hyperscheduler--ws-on-message nil frame)

                   ;; Verify single entry update was called (not invalidate)
                   (expect 'org-hyperscheduler--send-single-entry-update :to-have-been-called)))))

          (it "registers single entry hooks correctly"
              (let ((original-todo-hook org-after-todo-state-change-hook)
                    (original-clock-hook org-clock-out-hook))

                ;; Clear hooks first
                (org-hyperscheduler-clear-hooks)

                ;; Verify hooks are not present
                (expect (member #'org-hyperscheduler--send-single-entry-update
                               org-after-todo-state-change-hook) :to-be nil)
                (expect (member #'org-hyperscheduler--send-single-entry-update
                               org-clock-out-hook) :to-be nil)

                ;; Register hooks
                (org-hyperscheduler-register-hooks)

                ;; Verify hooks are present
                (expect (member #'org-hyperscheduler--send-single-entry-update
                               org-after-todo-state-change-hook) :not :to-be nil)
                (expect (member #'org-hyperscheduler--send-single-entry-update
                               org-clock-out-hook) :not :to-be nil)

                ;; Clean up - restore original hooks
                (setq org-after-todo-state-change-hook original-todo-hook)
                (setq org-clock-out-hook original-clock-hook)))

          (it "sends correct JSON structure for single entry updates"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 (setq org-hyperscheduler-ws-socket t)
                 (spy-on 'websocket-openp :and-return-value t)

                 ;; Navigate to first heading
                 (goto-char (point-min))
                 (re-search-forward "a task aaa")

                 ;; Call single entry update
                 (org-hyperscheduler--send-single-entry-update)

                 ;; Verify websocket message was sent with correct structure
                 (expect 'websocket-send-text :to-have-been-called)
                 (expect __response :to-match "update-single-entry")
                 (expect __response :to-match "a task aaa")
                 (expect __response :to-match "\"entry\":"))))

          (it "handles websocket closed gracefully during single entry update"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 (setq org-hyperscheduler-ws-socket t)
                 (spy-on 'websocket-openp :and-return-value nil)

                 ;; Navigate to first heading
                 (goto-char (point-min))
                 (re-search-forward "a task aaa")

                 ;; Call single entry update with closed websocket
                 (org-hyperscheduler--send-single-entry-update)

                 ;; Verify no websocket message was sent
                 (expect 'websocket-send-text :not :to-have-been-called))))

          (it "can clear single entry hooks without affecting other hooks"
              (let ((test-hook-called nil))
                ;; Add a test hook to verify it's not affected
                (add-hook 'org-after-todo-state-change-hook
                         (lambda () (setq test-hook-called t)))

                ;; Register org-hyperscheduler hooks
                (org-hyperscheduler-register-hooks)

                ;; Verify both hooks are present
                (expect (member #'org-hyperscheduler--send-single-entry-update
                               org-after-todo-state-change-hook) :not :to-be nil)

                ;; Clear org-hyperscheduler hooks
                (org-hyperscheduler-clear-hooks)

                ;; Verify org-hyperscheduler hook is removed but test hook remains
                (expect (member #'org-hyperscheduler--send-single-entry-update
                               org-after-todo-state-change-hook) :to-be nil)

                ;; Run the hook to verify test hook still works
                (run-hooks 'org-after-todo-state-change-hook)
                (expect test-hook-called :to-be t)

                ;; Clean up
                (remove-hook 'org-after-todo-state-change-hook
                            (lambda () (setq test-hook-called t)))))

          (it "sends single entry update when TODO state changes"
              (with-mock-contents
               "* TODO Test Task for State Change\nSCHEDULED: <2022-01-23 Sun>\n:PROPERTIES:\n:ID: STATE_CHANGE_TEST\n:END:\n"
               (lambda ()
                 (setq org-hyperscheduler-ws-socket t)
                 (spy-on 'websocket-openp :and-return-value t)

                 ;; Navigate to the heading
                 (goto-char (point-min))
                 (re-search-forward "Test Task for State Change")

                 ;; Register hooks
                 (org-hyperscheduler-register-hooks)

                 ;; Change TODO state (this should trigger the hook)
                 (org-todo "DONE")

                 ;; Verify websocket message was sent
                 (expect 'websocket-send-text :to-have-been-called)
                 (expect __response :to-match "update-single-entry")
                 (expect __response :to-match "Test Task for State Change")

                 ;; Clean up
                 (org-hyperscheduler-clear-hooks))))

          (it "sends single entry update with correct entry data structure"
              (with-mock-contents
               mock-org-contents
               (lambda ()
                 (setq org-hyperscheduler-ws-socket t)
                 (spy-on 'websocket-openp :and-return-value t)

                 ;; Navigate to second heading (has time info)
                 (goto-char (point-min))
                 (re-search-forward "a second task")

                 ;; Call single entry update
                 (org-hyperscheduler--send-single-entry-update)

                 ;; Verify websocket message contains required fields
                 (expect 'websocket-send-text :to-have-been-called)
                 (expect __response :to-match "update-single-entry")
                 (expect __response :to-match "FAKE_ID1")
                 (expect __response :to-match "a second task")
                 (expect __response :to-match "startDate")
                 (expect __response :to-match "endDate")
                 (expect __response :to-match "clockedList"))))

          (it "handles single entry update for entries with clocked time"
              (with-mock-contents
               "* TODO Task with Clock Time\nSCHEDULED: <2022-01-23 Sun>\n:PROPERTIES:\n:ID: CLOCKED_TASK_ID\n:END:\n:LOGBOOK:\nCLOCK: [2022-01-23 Sun 10:00]--[2022-01-23 Sun 11:00] =>  1:00\n:END:\n"
               (lambda ()
                 (setq org-hyperscheduler-ws-socket t)
                 (spy-on 'websocket-openp :and-return-value t)

                 ;; Navigate to the heading
                 (goto-char (point-min))
                 (re-search-forward "Task with Clock Time")

                 ;; Call single entry update
                 (org-hyperscheduler--send-single-entry-update)

                 ;; Verify websocket message includes clocked time data
                 (expect __response :to-match "update-single-entry")
                 (expect __response :to-match "clockedList")
                 (expect __response :to-match "Task with Clock Time")

                 ;; Parse and verify clocked data is included
                 (let* ((json-response (json-parse-string __response :object-type 'alist))
                        (entry (alist-get 'entry json-response))
                        (clocked-list (alist-get 'clockedList entry)))
                   (expect clocked-list :not :to-be nil))))))



(describe "org-ql"
          ;; note that we generate clocked entries as separate calendar entries
          (it "can get the right entries"
              (with-mock-contents
               mock-org-contents-org-ql-test
               (lambda () (let ((number-of-todo-entries (length (org-hyperscheduler-get-calendar-entries nil))))
                             (expect number-of-todo-entries :to-be 7))))
              )
          )
