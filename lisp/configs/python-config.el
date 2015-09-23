;;; python-config.el

;; pip install virtualenv

(require-or-install 'jedi)

(add-hook 'python-mode-hook 'jedi:setup)

(setq jedi:complete-on-dot t)

; (jedi:install-server)

(provide 'python-config)
