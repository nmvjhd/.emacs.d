;;设置emacs初始目录为HOME所在位置
(setq default-directory "~/")

(dolist (path '("~/.emacs.d/lisp/"
	     "~/.emacs.d/elpa"))
  (add-to-list 'load-path path))

;;package config
(require 'package)
(add-to-list 'package-archives
  ;'("melpa-stable" . "http://melpa-stable.org/packages/")
  '("melpa-unstable" . "http://melpa.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load my config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)							;打开emacs server
(require 'init-common)
(require 'init-config)
(require 'init-fonts)
(require 'init-tabbar)
(require 'init-lisp)
(require 'init-scheme)
(require 'init-javascript)
(require 'init-org)
(require 'init-markdown)
(require 'init-note)
(require 'init-hotkey)
(require 'test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load download config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'xah-find)
(require 'xah-lookup)
(load-library "2048")

;;(load-theme 'leuven t)
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
 '(package-selected-packages
   (quote
	(pandoc-mode markdown-mode+ markdown-mode auctex leuven-theme nodejs-repl xref-js2 graphviz-dot-mode js2-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

