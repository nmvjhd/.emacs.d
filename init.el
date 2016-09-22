(dolist (path '("~/.emacs.d/lisp/"
	     "~/.emacs.d/elpa"))
  (add-to-list 'load-path path))

;;essencial software config
(require 'init-config)

;;package config
(require 'package)
(add-to-list 'package-archives
  ;'("melpa-stable" . "http://melpa-stable.org/packages/")
  '("melpa-unstable" . "http://melpa.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load download config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; minibuffer输入提示插件
(require 'ido)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load my config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'hotkey)
(require 'init-common)
(require 'init-tabbar)
(require 'init-scheme)
(require 'init-javascript)
(require 'init-note)
(require 'test)

(require 'xah-find)
(require 'xah-lookup)

(load-library "2048")


(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(define-key global-map (kbd "C-c a") 'wy-go-to-char)

;;iimage
;(add-hook 'org-mode-hook 'iimage-mode) ; enable iimage-mode for org-mode
(autoload 'iimage-mode "iimage" "support inline image minor mode" t)
(autoload 'turn-on-iimage-mode "iimage" "turn on inline image minor mode" t)

(defun replace-line-slash()
  (interactive)
  (move-beginning-of-line -1)
  (replace-string "\\" "/")
  (move-end-of-line nil))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (nodejs-repl xref-js2 graphviz-dot-mode js2-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
