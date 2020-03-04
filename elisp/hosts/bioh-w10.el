;;; bioh-w10.el --- bioh-w10 specific configurations

;;; Commentary:

;;; Code:
;; font
(defconst psv/default-font "Consolas 10")
(when window-system
  (set-frame-font psv/default-font nil t))

;; theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(provide 'bioh-w10)
;;; bioh-w10 ends here
