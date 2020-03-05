;;; x220.el --- x220 specific configurations

;;; Commentary:

;;; Code:
(require 'browse-url)

;; browse with firefox
(setq browse-url-browser-function 'browse-url-firefox)

;; font & theme
(when (display-graphic-p)
  (defconst psv/default-font "Liberation Mono 11")
  (set-frame-font psv/default-font nil t)
  (add-to-list 'default-frame-alist '(font . psv/default-font))
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-dark t)))

(provide 'x220)
;;; x220 ends here
