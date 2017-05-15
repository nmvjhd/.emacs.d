;;hot key

(global-set-key (kbd "<f1>") 'open-emacs-config)
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

(provide 'init-hotkey)
