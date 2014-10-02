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
    (load "normal")
    (load "im")
    (load "cpp")
    (load "go")))
