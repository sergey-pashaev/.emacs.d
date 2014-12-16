;; Setting all load paths and load all neccesary files.

;; a large part of Common Lisp implemented in Emacs Lisp
(require 'cl)

(require 'package)

;; determine the load path dirs
;; as relative to the location of this file
(defvar dotfiles-dir "~/.emacs.d/lisp/"
  "The root Emacs Lisp source folder")

;; external packages reside here
(defvar ext-dir (concat dotfiles-dir "vendor/")
  "The root folder for external packages")

;; useful functions reside here
(defvar utils-dir (concat dotfiles-dir "utils/")
  "The root folder for utilites")

;; configs reside here
(defvar configs-dir (concat dotfiles-dir "configs/")
  "The root folder for configuration files")

;; set an explicit file to customization created via the UI
(setq custom-file (concat dotfiles-dir "custom.el"))

;; add everything to the load path
(add-to-list 'load-path dotfiles-dir)

(defun add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the
Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(defun list-add-subfolders-to-load-path (dirs)
  (dolist (item dirs)
    (add-subfolders-to-load-path item)))

;; add the first lever subfolders automatically
(list-add-subfolders-to-load-path (list dotfiles-dir
                                        ext-dir
                                        utils-dir
                                        configs-dir))

(defun require-file (file)
  (message "Loading %s..." file)
  (require file)
  (message "Loaded %s." file))

(defun require-filelist (items filename-suffix)
  (dolist (item items)
    (require-file (intern (concatenate 'string
                                         (symbol-name item)
                                         filename-suffix)))))

(defun require-or-install (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if psv/onlinep
        (if (or (assoc package package-archive-contents) no-refresh)
            (progn
              (package-install package)
              (require package))
          (progn
            (package-refresh-contents)
            (require-package package min-version t)))
      (messsage "Offline. Can't download package"))))

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

(let ((load-start-time (float-time)))
  (progn
    ;; load ~/.emcas.d/utils.el
    (require 'utils)
    (require-filelist utils "-utils")

    ;; load ~/.emacs.d/configs.el
    (require 'configs)
    (require-filelist configs "-config")

    ;; load ~/.emacs.d/custom.el
    (load custom-file 'noerror)

    ;; You can keep system- or user-specific customizations here
    (setq system-specific-config (concat dotfiles-dir system-name ".el")
          user-specific-config (concat dotfiles-dir user-login-name ".el")
          user-specific-dir (concat dotfiles-dir user-login-name))

    (add-to-list 'load-path user-specific-dir)

    (if (file-exists-p system-specific-config)
        (load system-specific-config))
    (if (file-exists-p user-specific-config)
        (load user-specific-config))
    (if (file-exists-p user-specific-dir)
        (mapc #'load (directory-files user-specific-dir nil ".*el$")))
    (message "All configs loaded in %f seconds." (- (float-time) load-start-time))))
