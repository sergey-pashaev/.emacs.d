;;; init.el --- Base config file to load different "layers" of configuration

;;; Commentary:
;; Here we load different modules/layers of my Emacs config.
;; Some inspiring Emacs configs:
;; - http://doc.rix.si/org/fsem.html
;; - http://pages.sachachua.com/.emacs.d/Sacha.html
;; - https://github.com/thomasf/dotfiles-thomasf-emacs/
;; - https://github.com/danielmai/.emacs.d
;; - https://github.com/bradwright/emacs-d
;; - https://github.com/magnars/.emacs.d
;; - https://github.com/rafadc/emacs.d/blob/master/settings.org
;; - http://writequit.org/eos/eos.html
;; - https://github.com/grettke/help/blob/master/help.org
;; - https://github.com/howardabrams/dot-files
;; - https://expoundite.net/guides/dotfile-management

;;; Code:
(defconst psv/elisp-dir (concat user-emacs-directory "elisp")
  "Directory which contains all config modules.")

;; todo: do we need fullpath here?
(defconst psv/private-dir (expand-file-name "private" user-emacs-directory)
  "Directory to store all private files.")

(defconst psv/temp-dir (format "%s/cache" psv/private-dir)
  "Directory to store all Emacs temp files are stored.")

(defconst psv/backup-root-dir (concat psv/temp-dir "/backup/")
  "Directory to store backup files.")

(add-to-list 'load-path psv/elisp-dir)

(require 'base)
(require 'base-env)
(require 'base-extensions)
(require 'base-functions)
(require 'base-global-keys)

(require 'base-org)

(require 'lang-elisp)
(require 'lang-cpp)

(require 'multi-magit)

(provide 'init)
;;; init.el ends here
