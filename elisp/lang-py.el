;;; lang-py.el --- Python language configuration

;;; Commentary:

;;; Code:

;; py settings
(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(provide 'lang-py)
;;; lang-py.el ends here
