;;; ops.el --- working ps specific configurations

;;; Commentary:

;;; Code:

(when window-system
  (use-package solarized-theme
    :config
    (load-theme 'solarized-light t)))

;;; C++
 (use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package company-c-headers :ensure t)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers)))

(use-package rtags
  :ensure t
  :config
  (progn
    (setq rtags-completions-enabled t)
    (setq rtags-autostart-diagnostics t)
    (rtags-enable-standard-keybindings)
    (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
    (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)))

(use-package company-rtags
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags)))

(use-package flycheck-rtags :ensure t)

(use-package helm-rtags
  :ensure t
  :config
  (setq rtags-display-result-backend 'helm))

(provide 'ops)
;;; ops ends here
