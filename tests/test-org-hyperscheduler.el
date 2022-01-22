
(require 'org-hyperscheduler)


(defvar mock-org-contents
"* Heading 1
** Subheading 2
*** TODO a task aaa
*** TODO a second task
")

(describe "Setup the contents"
          (it "can iterate over them"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let ((number-of-todo-entries (length (get-calendar-entries))))
                  (expect number-of-todo-entries :to-be 2))))

          (it "can produce a json representation"
               (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let ((json-representation (json-encode (get-calendar-entries))))
                  (expect (string-match "a task aaa" json-representation) :not :to-be nil))))
          )
