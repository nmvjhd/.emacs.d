;;hot key

(global-set-key (kbd "<f1>") 'open-emacs-config)
(global-set-key (kbd "<f9>") 'insert-arrow)
(global-set-key (kbd "<f10>") 'eval-last-sexp)
(global-set-key (kbd "<f11>") 'org2md)
(global-set-key (kbd "<f12>") 'open-elisp-document)
(global-set-key [(meta up)] 'move-line-up)
(global-set-key [(meta down)] 'move-line-down)

(defun open-emacs-config()
	"open emacs config file for user edit"
	(interactive)
	(find-file "~/.emacs.d/init.el"))

(defun open-elisp-document()
       "open elisp.pdf using sumatrapdf"
       (interactive)
       (let ((documentName (concat (getenv "HOME") "/book/programing/lisp/elisp.pdf")))
	 (start-process "SumatraPDF" nil "~/installed/SumatraPDF/SumatraPDF.exe" documentName)))

(defun move-line-up ()
 "Moves current line up."
 (interactive)
   (transpose-lines 1)
   (previous-line)
   (previous-line))

(defun move-line-down ()
 "Moves current line Down."
 (interactive)
 (next-line)
 (move-line-up)
 (next-line))

(defun insert-arrow()
  ;;insert arrow at current point
  (interactive)
  (insert-string " --> "))

(defun org2md()
  "convert current org-mode format document to markdown format"
  (interactive)
  (let ((buffer-name (new-buffer-name buffer-file-name)))
	(progn
	  (if (has-buffer buffer-name) (kill-buffer buffer-name))
	 (call-process "pandoc" nil buffer-name nil "-f" "org" "-t" "markdown" buffer-file-name)
	 (switch-to-buffer buffer-name))))

(defun new-buffer-name(full-name)
  (let ((short-name (buffer-short-name full-name)))
	(replace-regexp-in-string "\\.\\w+\\|$" ".md" short-name)))

(defun buffer-short-name (full-name)
  (let ((path-list (split-string full-name "\/")))
	(car (last path-list))))

(defun has-buffer (buffer-name)
  (let ((buf (get-buffer buffer-name)))
	(not (eq nil buf))))

(provide 'init-hotkey)
