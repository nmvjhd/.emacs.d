
;;emacs初始设置

(setf inhibit-startup-message t)
(setf gnus-inhibit-startup-message t)
;; 菜单栏
(if (display-graphic-p) (menu-bar-mode t) (menu-bar-mode 0))
;; 工具栏
(tool-bar-mode 0)
;; 行号栏
(global-linum-mode t)
;; 滚动栏
(if (display-graphic-p) (scroll-bar-mode t))
;; 底栏
;; (require 'ido)
;; (ido-mode t)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; 括号自动补全
(electric-pair-mode t)
;; 自动高亮括号
(show-paren-mode t)

;; 使用ibuffer显示buffer-list
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setf make-backup-files nil)

;;允许Lexical Binding
;;(setq lexical-binding t)
;; (let ((y  4)) ;;使用lexical binding其值为6，否则为8
;;   (funcall
;;    (let ((y 3))
;;        (lambda (x) (* y 2))) 
;;     0))

(provide 'init-config)
