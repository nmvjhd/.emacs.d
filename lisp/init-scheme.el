;;;;;;;;;;;;
;; Scheme 
;;;;;;;;;;;;

(defalias 'first 'car)

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode t)
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))

;;paredit-mode
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(require 'cmuscheme)
;(setq scheme-program-name "~/installed/Racket/Racket.exe")         ;; 使用racket作为scheme解释器
(setq scheme-program-name "~/installed/ChezSchemeVersion8.4/bin/a6nt/petite.exe")	;;使用petite
;(setq scheme-program-name "~/installed/clisp-2.49/clisp.exe -q -K full")	;;使用clisp

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))


(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
	       'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))


(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))


(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(provide 'init-scheme)
