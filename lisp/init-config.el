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

(setf make-backup-files nil)

;;����Lexical Binding
;;(setq lexical-binding t)
;; (let ((y  4)) ;;ʹ��lexical binding��ֵΪ6������Ϊ8
;;   (funcall
;;    (let ((y 3))
;;        (lambda (x) (* y 2))) 
;;     0))

(provide 'init-config)
