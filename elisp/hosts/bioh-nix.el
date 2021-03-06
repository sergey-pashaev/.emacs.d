;;; bioh-nix.el --- bioh-nix specific configurations

;;; Commentary:

;;; Code:
;; font & theme
(when (display-graphic-p)
  (defconst psv/default-font "Liberation Mono 11")
  (set-frame-font psv/default-font nil t)
  (add-to-list 'default-frame-alist '(font . psv/default-font))
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-light t)))

(require 's)
(require 'f)
(require 'projectile)
(require 'browse-url)

(require 'base-functions)

;; Browse with yandex-browser
(defconst *psv/yandex-browser-program* "yandex-browser")
(defconst *psv/browser-url-yandex-browser-arguments* nil)

(defun psv/browse-url-yandex-browser (url &optional _new-window)
  "Ask the Yandex Browser WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `*psv/browser-url-yandex-browser-arguments*' are also
passed to browser.  The optional argument NEW-WINDOW is not
used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat *psv/yandex-browser-program* " " url)
           nil
	   *psv/yandex-browser-program*
	   (append
	    *psv/browser-url-yandex-browser-arguments*
	    (list url)))))

(setq browse-url-browser-function 'psv/browse-url-yandex-browser)

(defun psv/copy-projectile-buffer-relative-path-to-clipboard ()
  "Put the current file name to clipboard."
  (interactive)
  (let ((filename (psv/projectile-buffer-relative-path)))
    (psv/put-to-clipboard filename)
    (message "Copied: %s" filename)))

;; ripgrep specific files
(defconst *psv/ripgrep-mojom-files* '("*.mojom"))
(defconst *psv/ripgrep-build-files* '("*.gn" "DEPS"))
(defconst *psv/ripgrep-yaml-files* '("*.yaml"))
(defconst *psv/ripgrep-java-files* '("*.java"))
(defconst *psv/ripgrep-cpp-test-files* '("*test.cc" "*tests.cc"))
(defconst *psv/ripgrep-cpp-browsertest-files* '("*browsertest.cc" "*browsertests.cc"))

(require 'rx)

(defun psv/projectile-ripgrep-current-filename ()
  "Run a Ripgrep serach with current buffer filename at the current projectile project root."
  (interactive)
  (psv/projectile-ripgrep (regexp-quote (ff-basename (psv/buffer-file-path)))
                          nil
                          nil
                          nil))

(defun psv/projectile-ripgrep-cpp (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep c++ files for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          '("-tcpp")
                          nil
                          *psv/ripgrep-cpp-test-files*))

(defun psv/projectile-ripgrep-py (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep python files for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          '("-tpy")
                          nil
                          nil))

(defun psv/projectile-ripgrep-mojom (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep mojom files for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          nil
                          *psv/ripgrep-mojom-files*
                          nil))

(defun psv/projectile-ripgrep-tests (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep c++ tests for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          nil
                          *psv/ripgrep-cpp-test-files*
                          nil))

(defun psv/projectile-ripgrep-yaml (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep yaml for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          nil
                          *psv/ripgrep-yaml-files*
                          nil))

(defun psv/projectile-ripgrep-browsertests (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep c++ browsertests for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          nil
                          *psv/ripgrep-cpp-browsertest-files*
                          nil))

(defun psv/projectile-ripgrep-build (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep build files for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          nil
                          *psv/ripgrep-build-files*
                          nil))

(defun psv/projectile-ripgrep-java (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep java files for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          nil
                          *psv/ripgrep-java-files*
                          nil))

(defun psv/projectile-ripgrep (regexp args include exclude)
  "Run a Ripgrep search with `REGEXP' rooted at the current project root.

Pass list of strings `ARGS' to command line arguments, pass
`INCLUDE' & `EXCLUDE' list of strings as included/excluded glob
patterns."
  (psv/ripgrep regexp
               (projectile-project-root)
               args
               include
               (append exclude
                       projectile-globally-ignored-files
                       projectile-globally-ignored-directories)))

(defun psv/ripgrep (regexp dir args include exclude)
  "Run a Rupgrep search with `REGEXP'.

Rooted at the `DIR' with list of included globs `INCLUDE' and
  list of excluded globs `EXCLUDE'.' Pass list of strings `ARGS'
  to command line arguments."
  (ripgrep-regexp regexp
                  dir
                  (append '("-S")
                          args
                          (mapcar (lambda (val) (concat "--glob " val))
                                  include)
                          (mapcar (lambda (val) (concat "--glob !" val))
                                  exclude))))

(define-key projectile-mode-map (kbd "C-c p s f") 'psv/projectile-ripgrep-current-filename)
(define-key projectile-mode-map (kbd "C-c p s p") 'psv/projectile-ripgrep-cpp)
(define-key projectile-mode-map (kbd "C-c p s y") 'psv/projectile-ripgrep-py)
(define-key projectile-mode-map (kbd "C-c p s b") 'psv/projectile-ripgrep-build)
(define-key projectile-mode-map (kbd "C-c p s m") 'psv/projectile-ripgrep-mojom)
(define-key projectile-mode-map (kbd "C-c p s t") 'psv/projectile-ripgrep-tests)
(define-key projectile-mode-map (kbd "C-c p s y") 'psv/projectile-ripgrep-yaml)
(define-key projectile-mode-map (kbd "C-c p s j") 'psv/projectile-ripgrep-java)

(defhydra psv/hydra-buffer (:hint t)
  "Current buffer operations"
  ("n" psv/copy-file-name-to-clipboard "copy file name")
  ("i" psv/copy-include-statement-to-clipboard "copy #include statement")
  ("r" psv/copy-projectile-buffer-relative-path-to-clipboard "copy project relative path"))

(defhydra psv/hydra-projectile-ripgrep (:hint t)
  "Projectile ripgrep at:"
  ("f" psv/projectile-ripgrep-current-filename "current filename")
  ("p" psv/projectile-ripgrep-cpp "c++ files (*.h, *.cc, etc.)")
  ("y" psv/projectile-ripgrep-py "python files (*.py)")
  ("b" psv/projectile-ripgrep-build "build files")
  ("m" psv/projectile-ripgrep-mojom "mojom files (*.mojom)")
  ("t" psv/projectile-ripgrep-tests "c++ test files (*test.cc)")
  ("y" psv/projectile-ripgrep-yaml "yaml files (*.yaml)")
  ("j" psv/projectile-ripgrep-java "java files (*.java)"))

(provide 'bioh-nix)
;;; bioh-nix ends here
