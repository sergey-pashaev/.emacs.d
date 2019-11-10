;;; x220.el --- x220 specific configurations

;;; Commentary:

;;; Code:
(require 'browse-url)

;; browse with firefox
(setq browse-url-browser-function 'browse-url-firefox)

(when window-system
  (set-frame-font "Liberation Mono 11" nil t))

(when window-system
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-dark t)))

(provide 'x220)
;;; x220 ends here
