;;; ui-config.el

;; bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)

;; cursor color
(if window-system
    (add-hook 'post-command-hook (lambda ()
                                   (set-cursor-color (if current-input-method
                                                         "red"
                                                       "black")))))

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(set-default-font "Liberation Mono 10")

(provide 'ui-config)
