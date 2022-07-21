(setq org-hyperscheduler-test-env t)

(require 'org-hyperscheduler)
(require 'org-id)



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


(org-hs--log-set-level 'debug)


;; this is how I ship org-hs: readonly mode enabled, org-roam exclusion disabled.
(defun org-hs-default-state ()
  (setq org-hyperscheduler-exclude-from-org-roam nil)
  (setq org-hyperscheduler-readonly-mode t))


(setq org-hyperscheduler-agenda-filter "TIMESTAMP>=\"<2022-01-01>\"|SCHEDULED>=\"<2022-01-01>\"")
(setq org-hs-test-file nil)


;; this is a convenience method for unit tests. sets up a temp buffer with mock contents and execs the lambda
;;
;; initially tried to use with-temp-buffer, but org-id doesn't like transient buffers and seems to need
;; actual files. so here we are.
;;
;; TODO: asserts can fail and that kills the stack, catch the exception so that we can do proper cleanup.
(defun with-mock-contents (contents lambda)
  (org-mode)
  (setenv "TZ" "America/Los_Angeles")
  ;; set time zone. all expectations are for PST time.
  ; (set-time-zone-rule "US/Pacific")

  (message (format "\nCurrent time zone: %s\n" (current-time-zone)))


  (setq org-id-track-globally t)


  ;; TODO: when an assert fails in buttercup, an exception (??) is thrown,
  ;; so temp file isnt being cleaned up. This is the sledgehammer approach.
  ;; Needs to be fixed so that it's cleaned up properly.
  (when org-hs-test-file (delete-file org-hs-test-file))


  (let* ((tempfile (make-temp-file "org-hs" nil ".org" contents)))
    (setq org-agenda-files (list tempfile))
    (setq org-hs-test-file tempfile)
    (find-file tempfile)
    (org-element-cache-reset)
    (funcall lambda)
    (kill-current-buffer)
    (delete-file tempfile)
  ))

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
               '(lambda () (let ((number-of-todo-entries (length (org-hyperscheduler-get-calendar-entries nil))))
                             (expect number-of-todo-entries :to-be 2))))
              )


          (it "can get the correct entries"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let ((number-of-todo-entries (length (org-hyperscheduler-get-calendar-entries nil))))
                  (expect number-of-todo-entries :to-be 2))))

          (it "has the correct properties"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                  (second-entry (car (cdr todo-entries))))
                  (expect (cdr (assoc "ID" second-entry)) :to-equal "FAKE_ID1"))))

          (it "can produce a json representation"
              (with-temp-buffer
               (org-mode)
                (insert mock-org-contents)
                (let ((json-representation (json-encode (org-hyperscheduler-get-calendar-entries nil))))
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



          (it "can encode empty agenda correctly"
              (with-mock-contents "" '(lambda ()
                                        (let* ((encoded-agenda (org-hyperscheduler--encode-agenda)))
                                          (expect encoded-agenda :to-equal "[]")))))


          (it "can update an existing scheduled event"
              (with-mock-contents
               mock-org-contents
               '(lambda ()
                  (org-hyperscheduler--update-event '(( id . "FAKE_ID1") (start . 1904247000 ) (end . 1904547000)))
                  (org-hyperscheduler-find-event-by-id "FAKE_ID1")
                  (let* ((plist (car (cdr (org-element-property :scheduled  (org-element-at-point)))))
                         (rawvalue (plist-get plist :raw-value)))
                    ;(message "%s" plist)
                    (expect rawvalue :to-equal "<2030-05-05 Sun 14:30-01:50>")))))

          (it "can insert a new scheduled event into the list"
              (with-mock-contents
               ""
               '(lambda ()
                  (let* ((old_inbox org-hyperscheduler-inbox-file))
                    (setq org-hyperscheduler-inbox-file tempfile) ;; tempfile is set in with-mock-contents
                    (org-hyperscheduler--add-scheduled-event '(( title . "test") (startUnix . 1904247000 ) (endUnix . 1904547000)))
                    ; TODO should probably check for the right time stamps
                    (expect (org-hyperscheduler-get-agenda) :not :to-be nil)
                    ; we have to reset the inbox path, because "defaults" checks for it.
                    (setq org-hyperscheduler-inbox-file old_inbox)))))

          (it "can insert a new timestamped event into the list")
          (it "can delete an existing event from the list"
              (with-mock-contents
               mock-org-contents
               '(lambda ()
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
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                       (roam-ignore-prop  (org-entry-get (point) "ROAM_EXCLUDE")))
                  (expect roam-ignore-prop :not :to-be nil))))

          (it "has the correct ID prefix"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents-unprocessed)
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                       (current-id (org-id-get)))
                  (expect (string-match "org-hyperscheduler-id.*" current-id)))))

          (it "does NOT have ROAM_EXCLUDE property when exclusion is disabled"
              (setq org-hyperscheduler-exclude-from-org-roam nil)
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
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
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
                       (current-id (org-id-get)))
                  (expect current-id :to-be nil))))


          (it "does NOT have ROAM_EXCLUDE property when the flag is set in READONLY mode"
              (setq org-hyperscheduler-exclude-from-org-roam t)
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (org-hyperscheduler-get-calendar-entries nil))
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
            (let ((js-date (org-hyperscheduler-get-js-date-pair)))
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
            (let ((js-date (org-hyperscheduler-get-js-date-pair)))
              (expect (cdr (assoc 'allDay js-date)) :to-equal "true")
              )
            )
          ))


(describe "time stamp generation"
          (it "can create a proper emacs timestamp from unix timestamp"
              (expect (org-hyperscheduler-get-scheduled-timestamp-for-scheduled-event 1643657400 (seconds-to-time 1643757400)) :to-equal "<2022-01-31 Mon 11:30-15:16>")))





(describe "webservices functionality"
          (before-each
           ;; intercept websocket-send-text.
           (spy-on 'websocket-send-text :and-call-fake (lambda (socket text)
                                                         (setq __agenda text)
                                                         )))

          ;; TODO Use the with-mock-contents to insert actual agenda and verify the result
          (it "can get agenda via websocket"
              (setq org-agenda-files nil)
              (org-element-cache-reset t)
              (let* ((frame (make-ws-frame "{\"command\":\"get-agenda\"}")))
                (org-hyperscheduler--ws-on-message nil frame)
                (expect 'websocket-send-text :to-have-been-called)
                ; agenda is set in ~before-each~
                (expect __agenda :to-equal "{\"agenda\":[]}") ;; no agenda, empty array
                ))


          (it "can remove events by id via websocket"
              (with-mock-contents
               mock-org-contents
               '(lambda ()
                  (org-hyperscheduler--ws-on-message nil (make-ws-frame "{\"command\":\"remove-event\", \"data\":{\"id\":\"FAKE_ID1\"}}"))
                  (expect (org-id-find "FAKE_ID1") :to-be nil))))

)



(describe "defaults"
          (it "should have a default for the inbox file"
              (expect org-hyperscheduler-inbox-file :to-equal "~/org/inbox.org"))
          (it "should have a default for the agenda filter"
              (expect org-hyperscheduler-agenda-filter :not :to-be nil))
          )


(describe "misc"
          (it "should find IDs in transient buffers"
              (with-mock-contents mock-org-contents '(lambda() (org-hyperscheduler-find-event-by-id "FAKE_ID1")))
              ))
