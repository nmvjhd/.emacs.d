;;emacs初始设置

(setf inhibit-startup-message t)
(setf gnus-inhibit-startup-message t)
(tool-bar-mode 0)

;;设置字体，为防止与cygwin冲突，因此加入系统类型判断
(if (equal system-type 'windows-nt)
    (set-fontset-font "fontset-default" 'gb18030' ("Microsoft YaHei" . "unicode-bmp")))

;;设置编码
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
;优先使用utf-8解码，如果解码失败，尝试使用gbk解码
(set-coding-system-priority 'chinese-iso-8bit)
(set-coding-system-priority 'utf-8)

;;显示行号
(global-linum-mode t)

;;自动补全括号
(electric-pair-mode t)

;;设置缩进(c++风格)
;(c-set-style "ellemtel")
(setf c-default-style "ellemtel" c-basic-offset 4)
(setf tab-width 4 indent-tabs-mode nil)
(setf default-tab-width 4)  ;设置TAB宽度为4

(show-paren-mode t)

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setf make-backup-files nil)

;;加载TAGS
(setf tags-file-name "~/.emacs.d/TAGS")

;;org-mode不折叠
(setq org-startup-folded nil)
(put 'upcase-region 'disabled nil)

;;设置emacs初始目录为HOME所在位置
(setq default-directory "~/")

(dolist (i '("~/.emacs.d/lisp/"
	     "~/.emacs.d/elpa"))
  (add-to-list 'load-path i))

;;使用ibuffer显示buffer list
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;允许Lexical Binding
;;(setq lexical-binding t)
;; (let ((y  4)) ;;使用lexical binding其值为6，否则为8
;;   (funcall
;;    (let ((y 3))
;;        (lambda (x) (* y 2))) 
;;     0))

(provide 'init-config)
