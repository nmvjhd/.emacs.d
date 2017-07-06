(require 'iimage)

(add-hook 'org-mode-hook '(lambda()
               (define-key org-mode-map
                 (kbd "<f12>") 'turn-on-iimage-mode)))

(setq iimage-mode-image-search-path '(list "." ".."))

(provide 'init-org)

