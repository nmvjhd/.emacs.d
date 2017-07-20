;;;;;;;;;;;;
;; Lisp
;;;;;;;;;;;;

(add-hook 'lisp-mode-hook
  (lambda ()
    (paredit-mode t)))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (paredit-mode t)))

(provide 'init-lisp)
