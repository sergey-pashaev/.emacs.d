;;; company-config.el

(require-or-install 'company)

(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(setq company-tooltip-limit 30)
(setq company-show-numbers t)
(setq company-dabbrev-other-buffers t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-tooltip-align-annotations t)

(global-company-mode)

(define-key company-mode-map [(meta return)] 'company-complete)

(provide 'company-config)
