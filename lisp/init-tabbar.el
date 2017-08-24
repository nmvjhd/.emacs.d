; tab标签插件
(require 'tabbar)
(tabbar-mode)

(setq tabbar-buffer-groups-function
	  (lambda ()
		(list "All")))

(provide 'init-tabbar)
