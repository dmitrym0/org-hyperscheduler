
(require 'org-hyperscheduler)


(defvar mock-org-contents
"* TODO a task aaa
SCHEDULED: <2022-01-23 Sun>
* TODO a second task
SCHEDULED: <2022-01-23 Sun 14:00-15:00>
")

(describe "Getting the agenda"
          (it "can get the correct entries"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let ((number-of-todo-entries (length (get-calendar-entries nil))))
                  (expect number-of-todo-entries :to-be 2))))

          (it "can produce a json representation"
               (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let ((json-representation (json-encode (get-calendar-entries nil))))
                  (expect (string-match "a task aaa" json-representation) :not :to-be nil))))
          )


(describe "ISO8601 date formatting"
          (it "can parse the dates correctly"
          (with-temp-buffer
            (insert mock-org-contents)
            (org-mode)
            (let ((js-date (get-js-date-pair)))
              (expect (cdr (assoc 'startDate js-date)) :to-equal "2022-01-23T14:00:00-08:00")
              (expect (cdr (assoc 'endDate js-date)) :to-equal "2022-01-23T15:00:00-08:00")
              (expect (cdr (assoc 'allDay js-date)) :to-equal "false")
              )
            )
          )


          (it "can detect all day correctly"
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

