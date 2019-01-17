;;; thinkpad.el --- thinkpad-specific configurations

;;; Commentary:

;;; Code:
(set-frame-font "Liberation Mono 12" nil t)

(use-package nord-theme
  :config
  (load-theme 'nord t))

(provide 'thinkpad)
;;; thinkpad ends here
