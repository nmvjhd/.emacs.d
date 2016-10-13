;;emacs��ʼ����

(setf inhibit-startup-message t)
(setf gnus-inhibit-startup-message t)
;; �˵���
(menu-bar-mode t)
;; ������
(tool-bar-mode 0)
;; �к���
(global-linum-mode t)
;; ������
(scroll-bar-mode t)
;; ����
(require 'ido)
(ido-mode t)

;; �����Զ���ȫ
(electric-pair-mode t)
;; �Զ���������
(show-paren-mode t)

;; ʹ��ibuffer��ʾbuffer-list
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; �������ã�Ϊ��ֹ��cygwin��ͻ����˼���ϵͳ�����ж�
(if (equal system-type 'windows-nt)
    (set-fontset-font "fontset-default" 'gb18030' ("Microsoft YaHei" . "unicode-bmp")))

;; ��������
; �����ļ���д��ʱĬ��ʹ��utf-8����
; (setq default-buffer-file-coding-system 'utf-8)
; �½����ȡ�ļ�Ĭ��ʹ��utf-8���루ʧ��ʱ���priority�����м���ƥ�䣩
(prefer-coding-system 'utf-8)
; ����ʹ��utf-8���룬�������ʧ�ܣ�����ʹ��gbk����
(set-coding-system-priority 'utf-8 'chinese-iso-8bit)

;;��������(c++���)
;(c-set-style "ellemtel")x
(setf c-default-style "ellemtel" c-basic-offset 4)
(setf tab-width 4 indent-tabs-mode nil)
(setf default-tab-width 4)  ;����TAB���Ϊ4

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setf make-backup-files nil)

;;����TAGS
(setf tags-file-name "~/.emacs.d/TAGS")

;;org-mode���۵�
(setq org-startup-folded nil)
(put 'upcase-region 'disabled nil)

;;����Lexical Binding
;;(setq lexical-binding t)
;; (let ((y  4)) ;;ʹ��lexical binding��ֵΪ6������Ϊ8
;;   (funcall
;;    (let ((y 3))
;;        (lambda (x) (* y 2))) 
;;     0))

(provide 'init-config)
