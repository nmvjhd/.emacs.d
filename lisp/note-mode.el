;; my note mode


(add-to-list 'auto-mode-alist '("\\.note\\'" . note-mode))
(setq org-todo-keywords '((type "TODO" "TIPS" "IMPORTANT" "|" "DONE")))
(global-set-key (kbd "<f3>") 'open-note)

(defvar note-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<f8>") 'insert-note)
	(define-key map (kbd "<f9>") 'insert-arrow)
	(define-key map (kbd "RET") 'newline)
	;; (define-key map (kbd "<tab>") 'insert-tab)
	map))

(defun open-note()
  (interactive)
  (let ((note-filename (concat "~/note/" (get-year-month-string) ".note")))
    (find-file note-filename))
  (if (month-change?)
      (insert-string (concat "* " (get-year-string) "/" (get-month-string) "\n")))
  (end-of-buffer))

(defun insert-note()
  (interactive)
  (let ((temp-file (concat (getenv "HOME") "/note/temp.txt")))
    (if (equal (get-string-from-file temp-file) (get-year-month-day-string))
		(progn
		  (if (= (char-before) 10)
			  nil
			(newline))
		  (insert-string "*** "))
      (progn
		(write-string-to-file (get-year-month-day-string) temp-file)
		(if (= (char-before) 10)
			nil
		  (insert-string "\n"))
		(insert-string (concat "** " (get-year-month-day-string) "\n*** "))))))

(defun insert-arrow()
  (interactive)
  (insert-string " --> "))

(defun insert-tab()
  (interactive)
  (insert-string "\t"))

(defun month-change? ()
  (let* ((temp-file (concat (getenv "HOME") "/note/temp.txt"))
       (file-string (get-string-from-file temp-file))
       (file-month-string (substring file-string 5 7)))
    (not (equal file-month-string (get-month-string)))))

(defun get-year-month-day-string()
  (concat (get-year-string) "/" (get-month-string) "/" (get-day-string)))

(defun get-year-month-string()
  (concat (get-year-string) (get-month-string)))

(defun get-year-string()
  (nth 4 (split-string (current-time-string))))

(defun get-month-string()
  (defun index-of(atom list)
    (if (equal atom (car list))
	0
      (1+ (index-of atom (cdr list)))))
  (defun integer->month-string(month-num)
    (if (< month-num 10)
	(concat "0" (number-to-string month-num))
      (number-to-string month-num)))
  (let ((month-english (nth 1 (split-string (current-time-string))))
	(month-list '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (integer->month-string (1+ (index-of month-english month-list)))))

(defun get-day-string()
  (let ((short-day (nth 2 (split-string (current-time-string)))))
    (if (equal (length short-day) 1)
	(concat "0" short-day)
      short-day)))

(define-derived-mode note-mode org-mode "Note"
  "a mode used to write note"
   )

(provide 'note-mode)
