;;; spellcheck-config.el

(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "english")
(setq ispell-local-dictionary "russian")
(setq flyspell-default-dictionary "russian")
(setq ispell-extra-args '("--sug-mode=ultra"))

(provide 'spellcheck-config)
