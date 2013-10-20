;;; package-sources-config.el

(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; require package if installed, otherwise install and require
(defun require-or-install (PKG)
  (if (package-installed-p PKG)
      (require PKG)
    (progn
      (package-install PKG)
      (require PKG))))

(provide 'package-sources-config)
