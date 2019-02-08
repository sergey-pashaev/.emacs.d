;;; ops.el --- working ps specific configurations

;;; Commentary:

;;; Code:

(when window-system
  (use-package solarized-theme
    :config
    (load-theme 'solarized-light t)))

(provide 'ops)
;;; ops ends here
