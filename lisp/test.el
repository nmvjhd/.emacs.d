
(defun mtf-insert-button ()
  (interactive)
  (insert-button "test"
				 'follow-link t
				 'action #'(lambda (x) (message "hehe"))))


(provide 'test)
