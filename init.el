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

;; load common configs
(defconst psv/elisp-dir (expand-file-name "elisp" user-emacs-directory)
  "Directory which contains all config modules.")

(add-to-list 'load-path psv/elisp-dir)
(load "base")
(load "base-extensions")
(load "base-functions")
(load "base-global-keys")
(load "base-org")
(load "lang-elisp")
(load "lang-cpp")
(load "lang-py")
(load "lang-js")
(load "base-diminish")

;; load host specific configs
(defconst psv/hosts-dir (expand-file-name "hosts" psv/elisp-dir)
  "Directory which contains all host-specific settings.")

(add-to-list 'load-path psv/hosts-dir)
(let ((host-specific-config (intern (system-name))))
  (require host-specific-config nil 'noerror))

;; make compilation buffers support colors
(load "xterm-color")
(require 'compile)
(setq compilation-environment '("TERM=xterm-256color"))

(add-hook 'compilation-start-hook
          (lambda (proc)
            ;; We need to differentiate between compilation-mode buffers
            ;; and running as part of comint (which at this point we assume
            ;; has been configured separately for xterm-color)
            (when (eq (process-filter proc) 'compilation-filter)
              ;; This is a process associated with a compilation-mode buffer.
              ;; We may call `xterm-color-filter' before its own filter function.
              (set-process-filter
               proc
               (lambda (proc string)
                 (funcall 'compilation-filter proc
                          (xterm-color-filter string)))))))

(provide 'init)
;;; init.el ends here
