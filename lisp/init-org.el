(require 'iimage)
(require 'ox-html)

(add-hook 'org-mode-hook '(lambda()
							(define-key org-mode-map
							  (kbd "<f12>") 'turn-on-iimage-mode)
							(setq truncate-lines nil)))

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

;; (defconst org-html-style-default
;;   (defun github-markdown-style ()
;; 	(concat "<style>\n"
;; 			(get-string-from-file (concat (getenv "HOME") "/.emacs.d/lisp/github-markdown.css"))
;; 			"</style>\n"))

;;   (defun post-style ()
;; 	"<style>
;; 	.markdown-body {
;; 		box-sizing: border-box;
;; 		min-width: 200px;
;; 		max-width: 980px;
;; 		margin: 0 auto;
;; 		padding: 45px;
;; 	}

;; 	@media (max-width: 767px) {
;; 		.markdown-body {
;; 			padding: 15px;
;; 		}
;; 	}
;;     </style>")
;;   (concat
;;    (github-markdown-style)
;;    (post-style)))

(org-export-define-derived-backend 'pd-html 'html
  :translate-alist '((template . pd-html-template)))

(defun pd-html-template (contents info)
   (concat
      "<!DOCTYPE html>\n"
      (format "<html lang=\"%s\">\n" (plist-get info :language))
      "<head>\n"
      (format "<meta charset=\"%s\">\n"
       (coding-system-get org-html-coding-system 'mime-charset))
      (format "<title>%s</title>\n"
       (org-export-data (or (plist-get info :title) "") info))
      (format "<meta name=\"author\" content=\"%s\">\n"
       (org-export-data (plist-get info :author) info))
     "<link href=\"/css/style.css\" rel=\"stylesheet\" style=\"text/css\" />\n"
     "</head>\n"
     "<body>\n"
     (format "<h1 class=\"title\">%s</h1>\n"
       (org-export-data (or (plist-get info :title) "") info))
     contents
     "</body>\n"
     "</html>\n"))

;; (org-export-define-derived-backend 'md-html 'html
;;  :menu-entry
;;  '(?r "Export to MD-HTML"
;;        ((?r "As MD-HTML file" (lambda (a s v b) (org-rss-export-to-md-html a s v)))))
;;   :options-alist
;;   '((:description "DESCRIPTION" nil nil newline)
;;     (:keywords "KEYWORDS" nil nil space)
;;     (:with-toc nil nil nil) ;; Never include HTML's toc
;;     (:rss-extension "RSS_EXTENSION" nil org-rss-extension)
;;     (:rss-image-url "RSS_IMAGE_URL" nil org-rss-image-url)
;;     (:rss-categories nil nil org-rss-categories))
;;   :filters-alist '((:filter-final-output . org-rss-final-function))
;;   :translate-alist '((headline . org-rss-headline)
;;              (comment . (lambda (&rest args) ""))
;;              (comment-block . (lambda (&rest args) ""))
;;              (timestamp . (lambda (&rest args) ""))
;;              (plain-text . org-rss-plain-text)
;;              (section . org-rss-section)
;;              (template . org-rss-template)))

(provide 'init-org)

