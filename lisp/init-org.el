(require 'iimage)

(add-hook 'org-mode-hook '(lambda()
               (define-key org-mode-map
                 (kbd "<f12>") 'turn-on-iimage-mode)))

(setq iimage-mode-image-search-path '(list "." ".."))

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(setq org-src-fontify-natively t)		;代码块高亮

;;org-mode不折叠
(setq org-startup-folded nil)
(put 'upcase-region 'disabled nil)

(provide 'init-org)

