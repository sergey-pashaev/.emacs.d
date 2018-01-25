;; inspiring emacs configs:
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

(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'base)
(require 'base-env)
(require 'base-extensions)
(require 'base-functions)
(require 'base-global-keys)

; (require 'base-org)

(require 'lang-elisp)
