
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

(describe "Getting the agenda"
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
                  (second-entry (car (cdr todo-entries ))))
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


          (it "has roam ignore tag"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let* ((todo-entries (get-calendar-entries nil))
                  (tags (org-get-tags)))
                  (expect (member "DO_NOT_ORG_ROAM" tags) :not :to-be nil))))


)


(describe "ISO8601 date formatting"
          (it "can parse the dates correctly"
          (setq org-id-track-globally nil)
          (with-temp-buffer
            (insert mock-org-contents)
            (org-mode)
            (org-previous-visible-heading 1)
            (let ((js-date (get-js-date-pair)))
              (expect (cdr (assoc 'startDate js-date)) :to-equal "2022-01-23T14:00:00-08:00")
              (expect (cdr (assoc 'endDate js-date)) :to-equal "2022-01-23T15:00:00-08:00")
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
