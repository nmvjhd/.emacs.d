;; 字体编码缩进设置


;; 字体设置，为防止与cygwin冲突，因此加入系统类型判断
;; (if (equal system-type 'windows-nt) ;这句代码会导致
;;   (set-fontset-font "fontset-default" 'gb18030' ("Microsoft YaHei" . "unicode-bmp")))

;; Setting English Font
(set-face-attribute
  'default nil :font "Consolas 12")
 
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft Yahei" :size 14)))

;; ;; 保存文件（写）时默认使用utf-8编码
;; (setq default-buffer-file-coding-system 'utf-8)
;; ;; 新建与读取文件默认使用utf-8编码（失败时会从priority队列中继续匹配）
;; (prefer-coding-system 'utf-8)
;; ;; 优先使用utf-8解码，如果解码失败，尝试使用gbk解码
;; (set-coding-system-priority 'utf-8 'chinese-iso-8bit)

;;设置缩进(c++风格)
;(c-set-style "ellemtel")x
(setf c-default-style "ellemtel" c-basic-offset 4)
(setf tab-width 4 indent-tabs-mode nil)
(setf default-tab-width 4)  ;设置TAB宽度为4

;; 注释反注释
(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'qiang-comment-dwim-line)

(provide 'init-fonts)
