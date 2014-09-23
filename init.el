;; inspiring emacs configs:
;; - https://github.com/thomasf/dotfiles-thomasf-emacs/

;; set package archives
(require 'cl)
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa"     . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("gnu"       . "http://elpa.gnu.org/packages/")))

;; check internet connection and set flag
(setq psv/onlinep nil)

(unless
    (condition-case nil
	(delete-process
	 (make-network-process
	  :name "psv/check-internet-connection"
	  :host "elpa.gnu.org"
	  :service 80))
      (error t))
  (setq psv/onlinep t))

(when psv/onlinep
  (package-initialize t))

;; add everything to load-path
(defun psv/add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the
Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(mapc (lambda (p) (push p load-path))
      '("~/.emacs.d/"
	"~/.emacs.d/configs/"))

(mapc (lambda (p) (psv/add-subfolders-to-load-path p))
      '("~/.emacs.d/configs/"
	"~/.emacs.d/elpa/"))

;; set an explicit file to customization created via the UI
(setq custom-file (concat user-emacs-directory "custom.el"))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(when psv/onlinep
  (require-package 'use-package)
  (require 'use-package))

(load "minimal")

(when psv/onlinep
  (progn
    (load "normal")))

;;; jabber
;; (require-or-install 'jabber)

;; (defun psv/jabber-hook ()
;;   (define-key jabber-chat-mode-map (kbd "RET") 'jabber-chat-buffer-send)
;;   (define-key jabber-chat-mode-map (kbd "<C-return>") 'newline))

;; (use-package jabber
;;   :init
;;   (progn
;;     (setq
;;      jabber-chat-buffer-show-avatar nil
;;      jabber-roster-line-format " %c %-25n %u %-8s  %S"
;;      jabber-roster-show-bindings nil
;;      jabber-roster-show-title nil
;;      jabber-history-dir "~/.emacs.d/jabber-history"
;;      jabber-history-enabled t
;;      jabber-use-global-history nil)

;;     (add-hook 'jabber-chat-mode-hook 'psv/jabber-hook t))
;;   :bind (("C-x C-;" . jabber-activity-switch-to)
;;          ("<C-f10>" . jabber-switch-to-roster-buffer)))

;; ;;; irfc
;; (require-or-install 'irfc)
;; (use-package irfc
;;   :init
;;   (setq irfc-directory "~/workspace/rfc/"
;;         irfc-assoc-mode t))

;;; golang
;; mysql driver
;;go get -u github.com/go-sql-driver/mysql

;; dev utils
;;go get -u github.com/dougm/goflymake
;;go get -u github.com/sergey-pashaev/goflymake
;;go get -u github.com/nsf/gocode
;;go get -u code.google.com/p/rog-go/exp/cmd/godef
;;go get -u github.com/sriram-srinivasan/gore
;;go get -u github.com/golang/lint/golint
;(require-or-install 'go-mode)
;(require-or-install 'go-autocomplete)
;(require-or-install 'go-eldoc)
;(require-or-install 'golint)
;(require-or-install 'go-flycheck)

;; (defun psv/go-hook ()
;;   (go-eldoc-setup)
;;   (flycheck-mode t)
;;   (setq tab-width 4)
;;   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
;;   (local-set-key (kbd "C-c i") 'go-goto-imports))

;; (use-package go-mode
;;   :init
;;   (progn
;;     (add-hook 'go-mode-hook 'psv/go-hook)
;;     (add-to-list 'load-path "~/workspace/go/src/github.com/sergey-pashaev/goflymake")))

;; ;;; erc
;; (require-or-install 'erc)
;; (use-package erc
;;   :init
;;   (progn
;;     (setq erc-hide-list '("JOIN" "PART" "QUIT")
;;           erc-prompt-for-nickserv-password nil
;;           erc-save-buffer-on-part t)))
