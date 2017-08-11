;;通用工具函数

(defun find(atom list pred)
  (if (null atom)
      nil
    (or (funcall pred atom (car list))
	(find atom (cdr list) pred))))
	
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun write-string-to-file(str fileName)
  (if (stringp str)
      (progn
		(delete-file fileName)
		(write-region str nil fileName 0))))
	
(provide 'init-tools)
