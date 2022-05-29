(setq org-hyperscheduler-test-env t)

(require 'org-hyperscheduler)


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

(defvar mock-org-contents-unprocessed
  "* TODO a task aaa
SCHEDULED: <2022-01-23 Sun>
")


;; this is how I ship org-hs: readonly mode enabled, org-roam exclusion disabled.
(defun org-hs-default-state ()
  (setq org-hyperscheduler-exclude-from-org-roam nil)
  (setq org-hyperscheduler-readonly-mode t))


(setq org-hyperscheduler-agenda-filter "TIMESTAMP>=\"<2022-01-01>\"|SCHEDULED>=\"<2022-01-01>\"")


(describe "Agenda functionality"
          (it "can get the correct entries"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let ((number-of-todo-entries (length (get-calendar-entries nil))))
                  (expect number-of-todo-entries :to-be 2))))

          (it "has the correct properties"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (get-calendar-entries nil))
                  (second-entry (car (cdr todo-entries))))
                  (expect (cdr (assoc "ID" second-entry)) :to-equal "FAKE_ID1"))))

          (it "can produce a json representation"
              (with-temp-buffer
               (org-mode)
                (insert mock-org-contents)
                (let ((json-representation (json-encode (get-calendar-entries nil))))
                  (expect (string-match "a task aaa" json-representation) :not :to-be nil))))
          


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


          (it "can update an existing scheduled event")
          (it "can insert a new scheduled event into the list")
          (it "can insert a new timestamped event into the list")
          (it "can delete an existing event from the list")


)


(describe "read write mode"
          (before-all
              (setq org-hyperscheduler-exclude-from-org-roam t)
              (setq org-hyperscheduler-readonly-mode nil))


          (after-all
           (org-hs-default-state))

          (it "has roam ignore property"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (get-calendar-entries nil))
                       (roam-ignore-prop  (org-entry-get (point) "ROAM_EXCLUDE")))
                  (expect roam-ignore-prop :not :to-be nil))))

          (it "has the correct ID prefix"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents-unprocessed)
                (let* ((todo-entries (get-calendar-entries nil))
                       (current-id (org-id-get)))
                  (expect (string-match "org-hyperscheduler-id.*" current-id)))))

          (it "does NOT have ROAM_EXCLUDE property when exclusion is disabled"
              (setq org-hyperscheduler-exclude-from-org-roam nil)
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (get-calendar-entries nil))
                       (roam-ignore-prop  (org-entry-get (point) "ROAM_EXCLUDE")))
                  (expect roam-ignore-prop :to-be nil))))
          )




(describe "readonly mode"

          (before-all
           (org-hs-default-state))


          (after-all
           (org-hs-default-state))

          (it "does NOT have a generated ID"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents-unprocessed)
                (let* ((todo-entries (get-calendar-entries nil))
                       (current-id (org-id-get)))
                  (expect current-id :to-be nil))))


          (it "does NOT have ROAM_EXCLUDE property when the flag is set in READONLY mode"
              (setq org-hyperscheduler-exclude-from-org-roam t)
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (get-calendar-entries nil))
                       (roam-ignore-prop  (org-entry-get (point) "ROAM_EXCLUDE")))
                  (expect roam-ignore-prop :to-be nil))))

          )


(describe "ISO8601 date formatting"
          (it "can parse the dates correctly"
          (setq org-id-track-globally nil)
          (with-temp-buffer
            (insert mock-org-contents)
            (org-mode)
            (org-previous-visible-heading 1)
            (let ((js-date (get-js-date-pair)))
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
            (let ((js-date (get-js-date-pair)))
              (expect (cdr (assoc 'allDay js-date)) :to-equal "true")
              )
            )
          ))


(describe "time stamp generation"
          (it "can create a proper emacs timestamp from unix timestamp"
              (expect (get-scheduled-timestamp-for-scheduled-event 1643657400 (seconds-to-time 1643757400)) :to-equal "<2022-01-31 Mon 11:30-15:16>")))



(describe "webservices functionality"
          (it "can get agenda via webservices"

              (let* ((command "{\"command\":\"get-agenda\"}")
                     (frame (websocket-read-frame (websocket-encode-frame
                                                   (make-websocket-frame :opcode 'text
                                                                         :payload (encode-coding-string command 'raw-text)
                                                                         :completep t)
                                                   t))))
                                        ; TODO (expect (org-hs--ws-on-message nil frame) :not :to-be nil)
                                        ; TODO refactor message handlers so we can pass mock webservices to it. for now, we expect a void-variable exception
                (expect (org-hyperscheduler--ws-on-message nil frame) :to-throw 'void-variable)
                )))


(describe "defaults"
          (it "should have a default for the inbox file"
              (expect org-hyperscheduler-inbox-file :to-equal "~/org/inbox.org"))
          (it "should have a default for the agenda filter"
              (expect org-hyperscheduler-agenda-filter :not :to-be nil))
          )
