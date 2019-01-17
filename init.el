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
(defconst psv/elisp-dir (expand-file-name "elisp" user-emacs-directory)
  "Directory which contains all config modules.")

(add-to-list 'load-path psv/elisp-dir)

(require 'base)
(require 'base-extensions)
(require 'base-functions)
(require 'base-global-keys)
(require 'base-org)

(require 'lang-elisp)
(require 'lang-cpp)

(require 'multi-magit)

(defconst psv/hosts-dir (expand-file-name "hosts" psv/elisp-dir)
  "Directory which contains all host-specific settings.")

(add-to-list 'load-path psv/hosts-dir)

(let ((host-specific-config (intern (system-name))))
  (require host-specific-config nil 'noerror))

(provide 'init)
;;; init.el ends here
