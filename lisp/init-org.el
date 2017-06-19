(global-set-key (kbd "<f11>") 'org2md)

(defun org2md()
  "convert current org-mode format document to markdown format"
  (interactive)
  (let ((buffer-name (new-buffer-name buffer-file-name)))
	(progn
	 (call-process "pandoc" nil buffer-name nil "-f" "org" "-t" "markdown" buffer-file-name)
	 (switch-to-buffer buffer-name))))

(defun new-buffer-name(full-name)
  (let ((short-name (buffer-short-name full-name)))
	(replace-regexp-in-string "\\.\\w+\\|$" ".md" short-name)))

(defun buffer-short-name (full-name)
  (let ((path-list (split-string full-name "\/")))
	(car (last path-list))))

(provide 'init-org)

