;;hot key

(global-set-key (kbd "<f1>") 'open-emacs-config) ; F1 打开init.el
(global-set-key (kbd "<f4>") 'close-current-buffer)	; F4 关闭当前buffer
(global-set-key (kbd "<f5>") 'new-temp-buffer) ; F5 创建新的临时buffer
(global-set-key (kbd "<f7>") 'one-key-push)	; F7 一键push
(global-set-key (kbd "<f8>") 'neotree-toggle) ; F8 开关neotree
(global-set-key (kbd "<f10>") 'eval-last-sexp) ; F10 执行光标前的S表达式
(global-set-key (kbd "<f11>") 'org2md)	; F11 org文档转换为markdown文档
(global-set-key (kbd "<C-f11>") 'md2org) ; Ctrl+F11 markdown文档转换为org文档
(global-set-key (kbd "<f12>") 'open-elisp-document)	; F12打开帮助文档
(global-set-key [(meta up)] 'move-line-up) ; Alt+向上箭头 行上移
(global-set-key [(meta down)] 'move-line-down) ; Alt+向下箭头 行下移
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase) ; 鼠标滚轮向上 放大字体
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease) ; 鼠标滚轮向下 缩小字体
(global-set-key (kbd "C-S-f") 'helm-projectile-find-file)
(global-unset-key (kbd "<C-down-mouse-1>"))
(global-set-key (kbd "<C-mouse-1>") 'helm-projectile-find-file-dwin)

(global-set-key (kbd "C-s") 'swiper) ; 使用swiper覆盖默认的搜索

(defun open-emacs-config()
	"open emacs config file for user edit"
	(interactive)
	(find-file "~/.emacs.d/init.el"))

(defun close-current-buffer()
  "close current buffer"
  (interactive)
  (kill-buffer (current-buffer)))

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

(defun insert-arrow ()
  "insert arrow at current point"
  (interactive)
  (insert-string " --> "))

(defun insert-bdarrow ()
  "insert a two way arrow"
  (interactive)
  (insert-string " <==> "))

(defun org2md()
  "convert current org-mode format document to markdown format"
  (interactive)
  (format-convert "org" "markdown" ".md"))

(defun md2org()
  "convert current org-mode format document to markdown format"
  (interactive)
  (format-convert "markdown" "org" ".org"))

(defun format-convert(from to to-extension)
  "convert from type format document to to type format"
  (interactive)
  (let ((buffer-name (new-buffer-name buffer-file-name to-extension)))
	(progn
	  (if (has-buffer buffer-name) (kill-buffer buffer-name))
	  (message (format "Convert %s from %s to %s. New buffer name is %s" buffer-name from to buffer-file-name))
	 (call-process "pandoc" nil buffer-name nil "-f" from "-t" to buffer-file-name)
	 (switch-to-buffer buffer-name))))

(defun new-buffer-name(full-name extension)
  (let ((short-name (buffer-short-name full-name)))
	(replace-regexp-in-string "\\.\\w+\\|$" extension short-name)))

(defun buffer-short-name (full-name)
  (let ((path-list (split-string full-name "\/")))
	(car (last path-list))))

(defun has-buffer (buffer-name)
  (let ((buf (get-buffer buffer-name)))
	(not (eq nil buf))))

(defun one-key-push ()
  "you just need to press one key to add commit push everything"
  (interactive)
  (let ((output-buffer "*one-key-push-message*"))
	(progn
	  (call-process "git" nil output-buffer nil "status")
	  (call-process "git" nil output-buffer nil "add" ".")
	  (call-process "git" nil output-buffer nil "commit" "-m" (read-from-minibuffer "commit message: "))
	  (call-process "git" nil output-buffer nil "push")
	  (switch-to-buffer output-buffer))))

(defvar buffer-mode-alist
  '(("java"   . java-mode)  
    ("c++"    . c++-mode)  
    ("perl"   . perl-mode)  
    ("python" . python-mode)  
    ("js"     . javascript-mode)  
    ("j"      . j-mode)  
    ("tcl"    . tcl-mode))  
  "生成草稿buffer的简短mode名称列表")

(defun new-temp-buffer ()
  "create a temp buffer, like a scratch"
  (interactive)  
  (let ((mode (ido-completing-read  
               "What kind of buffer mode ?:"  
               (append (all-completions ""  
                                        obarray  
                                        (lambda (s)   
                                          (and (fboundp s)  
                                               (string-match "-mode$" (symbol-name s)))))  
                       (mapcar 'car buffer-mode-alist)))))  
    (pop-to-buffer (get-buffer-create (format "* scratch * %s *" mode)))  
    (funcall (if (assoc mode buffer-mode-alist)  
                 (cdr (assoc mode buffer-mode-alist))  
               (intern mode)))
	(delete-other-windows)
	))

(provide 'init-hotkey)
