;; (defvar org-mode-map
;;   (let ((map (make-sparse-keymap)))
;; 	;; (define-key map (kbd "<f9>") 'insert-arrow)
;; 	map))

;; (provide 'init-org)


(require 'iimage)

(add-hook 'markdown-mode-hook '(lambda()
               (define-key markdown-mode-map
                 (kbd "<f12>") 'turn-on-iimage-mode)))

(setq iimage-mode-image-search-path '(list "." ".."))

(provide 'init-markdown)
