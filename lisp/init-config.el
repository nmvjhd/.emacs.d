;;emacs��ʼ����

(setf inhibit-startup-message t)
(setf gnus-inhibit-startup-message t)
(tool-bar-mode 0)

;;�������壬Ϊ��ֹ��cygwin��ͻ����˼���ϵͳ�����ж�
(if (equal system-type 'windows-nt)
    (set-fontset-font "fontset-default" 'gb18030' ("Microsoft YaHei" . "unicode-bmp")))

;;���ñ���
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
;����ʹ��utf-8���룬�������ʧ�ܣ�����ʹ��gbk����
(set-coding-system-priority 'chinese-iso-8bit)
(set-coding-system-priority 'utf-8)

;;��ʾ�к�
(global-linum-mode t)

;;�Զ���ȫ����
(electric-pair-mode t)

;;��������(c++���)
;(c-set-style "ellemtel")
(setf c-default-style "ellemtel" c-basic-offset 4)
(setf tab-width 4 indent-tabs-mode nil)
(setf default-tab-width 4)  ;����TAB���Ϊ4

(show-paren-mode t)

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setf make-backup-files nil)

;;����TAGS
(setf tags-file-name "~/.emacs.d/TAGS")

;;org-mode���۵�
(setq org-startup-folded nil)
(put 'upcase-region 'disabled nil)

;;����emacs��ʼĿ¼ΪHOME����λ��
(setq default-directory "~/")

(dolist (i '("~/.emacs.d/lisp/"
	     "~/.emacs.d/elpa"))
  (add-to-list 'load-path i))

;;ʹ��ibuffer��ʾbuffer list
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;����Lexical Binding
;;(setq lexical-binding t)
;; (let ((y  4)) ;;ʹ��lexical binding��ֵΪ6������Ϊ8
;;   (funcall
;;    (let ((y 3))
;;        (lambda (x) (* y 2))) 
;;     0))

(provide 'init-config)
