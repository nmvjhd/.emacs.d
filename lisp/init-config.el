;;emacs初始设置

(setf inhibit-startup-message t)
(setf gnus-inhibit-startup-message t)
;; 菜单栏
(menu-bar-mode t)
;; 工具栏
(tool-bar-mode 0)
;; 行号栏
(global-linum-mode t)
;; 滚动栏
(scroll-bar-mode t)
;; 底栏
(require 'ido)
(ido-mode t)

;; 括号自动补全
(electric-pair-mode t)
;; 自动高亮括号
(show-paren-mode t)

;; 使用ibuffer显示buffer-list
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; 字体设置，为防止与cygwin冲突，因此加入系统类型判断
(if (equal system-type 'windows-nt)
    (set-fontset-font "fontset-default" 'gb18030' ("Microsoft YaHei" . "unicode-bmp")))

;; 编码设置
; 保存文件（写）时默认使用utf-8编码
; (setq default-buffer-file-coding-system 'utf-8)
; 新建与读取文件默认使用utf-8编码（失败时会从priority队列中继续匹配）
(prefer-coding-system 'utf-8)
; 优先使用utf-8解码，如果解码失败，尝试使用gbk解码
(set-coding-system-priority 'utf-8 'chinese-iso-8bit)

;;设置缩进(c++风格)
;(c-set-style "ellemtel")x
(setf c-default-style "ellemtel" c-basic-offset 4)
(setf tab-width 4 indent-tabs-mode nil)
(setf default-tab-width 4)  ;设置TAB宽度为4

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setf make-backup-files nil)

;;加载TAGS
(setf tags-file-name "~/.emacs.d/TAGS")

;;org-mode不折叠
(setq org-startup-folded nil)
(put 'upcase-region 'disabled nil)

;;允许Lexical Binding
;;(setq lexical-binding t)
;; (let ((y  4)) ;;使用lexical binding其值为6，否则为8
;;   (funcall
;;    (let ((y 3))
;;        (lambda (x) (* y 2))) 
;;     0))

(provide 'init-config)
