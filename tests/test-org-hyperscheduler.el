
(require 'org-hyperscheduler)


(defvar mock-org-contents
"* Heading 1
** Subheading 2
*** TODO a task
")

(describe "Setup the contents"
          (it "can iterate over them"
              (with-temp-buffer
                (org-mode)
                (insert mock-org-contents)
                (let ((number-of-todo-entries (length (org-map-entries t "/+TODO" nil))))
                  (expect number-of-todo-entries :to-be 1)))))
