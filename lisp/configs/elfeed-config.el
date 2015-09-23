;;; elfeed-config.el

(require-or-install 'elfeed)

(setq elfeed-feeds '("http://www.reddit.com/r/emacs/.rss"
                     "http://planet.emacsen.org/ru/atom.xml"
                     "http://planet.emacsen.org/atom.xml"
                     "https://news.ycombinator.com/rss"
                     "https://meduza.io/rss/all"))

(provide 'elfeed-config)
