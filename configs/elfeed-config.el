;;; elfeed-config.el

(require-or-install 'elfeed)

(setq elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
                     "http://planet.emacsen.org/ru/atom.xml"))

(provide 'elfeed-config)
