;;; restclient-config.el

(require-or-install 'restclient)

(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

(provide 'restclient-config)
