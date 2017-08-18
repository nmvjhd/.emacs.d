(require 'iimage)

(add-hook 'org-mode-hook '(lambda()
							(progn						
							  (define-key org-mode-map
								(kbd "<f12>") 'turn-on-iimage-mode)
							  (setq truncate-lines nil))))

(setq iimage-mode-image-search-path '(list "." ".."))

(setq org-src-fontify-natively t)		;代码块高亮

;;org-mode不折叠
(setq org-startup-folded nil)
(put 'upcase-region 'disabled nil)

;;html导出设置

;; (let ((temp-func 'org-html-template))
;;   (defun org-html-template (contents info)
;; 	(let ((text (funcall temp-func contents info)))
;; 	  (concat "hahaha" text))))

(defun github-markdown-style ()
	(concat "<style>\n"
		  (get-string-from-file ".emacs.d/lisp/github-markdown.css")
		  "</style>\n"))

(defun post-style ()
	"<style>
	.markdown-body {
		box-sizing: border-box;
		min-width: 200px;
		max-width: 980px;
		margin: 0 auto;
		padding: 45px;
	}

	@media (max-width: 767px) {
		.markdown-body {
			padding: 15px;
		}
	}
    </style>")

(defconst org-html-style-default
  (concat
   (github-markdown-style)
   (post-style)))

(provide 'init-org)

