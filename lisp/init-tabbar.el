; tab标签插件
(require 'tabbar)
(tabbar-mode)

(defun my-tabbar-buffer-groups ()
  (list "user"))

 (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(provide 'init-tabbar)
