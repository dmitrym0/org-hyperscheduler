
(require 'org)

(defun print-entries ()
(message (cdr (assoc "ITEM" (org-entry-properties)))))

;(org-map-entries #'print-entries nil)

(provide 'org-hyperscheduler)
