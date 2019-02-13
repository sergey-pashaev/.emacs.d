;;; thinkpad.el --- thinkpad-specific configurations

;;; Commentary:

;;; Code:
(when window-system
  (set-frame-font "Liberation Mono 12" nil t)

  (use-package solarized-theme
    :config
    (load-theme 'solarized-light t)))

(provide 'thinkpad)
;;; thinkpad ends here
