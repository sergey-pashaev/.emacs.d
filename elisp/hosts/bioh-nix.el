;;; bioh-nix.el --- bioh-nix specific configurations

;;; Commentary:

;;; Code:
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

;; Chromium project related functions
(defconst *psv/chromium-project-root-path* (expand-file-name "~/workspace/ya/chromium/src/")
  "Chromium project root path.")

;; gn refs
(defun psv/gn-refs ()
  "Run gn refs for current file.
List all gn refs that using
current file in *psv/gn-ref* buffer."
  (interactive)
  (when (projectile-project-root)
    (let ((file (psv/buffer-file-path))
          (dir (concat (projectile-project-root) "src/")))
      (let ((default-directory dir)
            (program "/home/bioh/workspace/ya/depot_tools/gn")
            (cmd (format " refs out/Debug/ --all %s" file)))
        (start-process "psv/gn-ref-proc" "*psv/gn-ref*" program "refs" "out/Debug/" "-all" file)
        (message "gn refs started...")
        (switch-to-buffer-other-window "*psv/gn-ref*")))))

;; generate include statements for current file
(defun psv/projectile-buffer-relative-path ()
  "Return current buffer path relative to project root."
  (interactive)
  (let ((path (substring (psv/buffer-file-path) ; cut project root
                         (length (projectile-project-root)))))
    (if (string= (projectile-project-root)
                 *psv/chromium-project-root-path*)
        (concat "src/" path)
      path)))

(defun psv/make-include-statement ()
  "Generate include statement for current file."
  (let ((path (if (string= (projectile-project-root)
                           *psv/chromium-project-root-path*)
                  (substring (psv/projectile-buffer-relative-path) ; cut src/ for chromium
                             (length "src/"))
                (psv/projectile-buffer-relative-path))))
    (if (s-starts-with? "src/" path)
        (format "#include \"%s\"" (substring path (length "src/")))
      (format "#include \"%s\"" path))))

(defun psv/copy-include-statement-to-clipboard ()
  "Put the current file include statement name to clipboard."
  (interactive)
  (let ((include (psv/make-include-statement)))
    (psv/put-to-clipboard include)
    (message "Copied: %s" include)))

;; jump to same file in other repo/project
(defun psv/visit-file-in-other-project ()
  "Visit file in other project with same relative path as current buffer.
With passed unversal argument it visits file in other
window."
  (interactive)
  (let ((projects (projectile-relevant-known-projects))
        (position (point))
        (path (psv/projectile-buffer-relative-path)))
    (if projects
        (projectile-completing-read
         "Switch to file in project: " projects
         :action (lambda (project)
                   (let ((filepath (if (string= (expand-file-name project) *psv/chromium-project-root-path*)
                                       (concat "~/workspace/ya/chromium/" path)
                                     (concat project path))))
                     (if (f-exists? filepath)
                         (if current-prefix-arg
                             (progn
                               (delete-other-windows)
                               (split-window-right)
                               (other-window 1)
                               (find-file filepath)
                               (goto-char position)
                               (recenter-top-bottom)
                               (other-window 1))
                           (progn
                             (find-file filepath)
                             (goto-char position)
                             (recenter-top-bottom)))
                       (user-error (format "[%s] path:%s doesn't exist" project filepath))))))
      (user-error "There are no open projects"))))

;; diff with same file in other repo/project
(defun psv/diff-with-same-file-in-other-project ()
  "Diff current file with file on same path in other project."
  (interactive)
  (let ((projects (projectile-relevant-known-projects))
        (project (projectile-project-root))
        (filepath (psv/projectile-buffer-relative-path)))
    (if projects
        (projectile-completing-read
         "Diff with file in project: " projects
         :action (lambda (other-project)
                   (let ((other-filepath (if (string= (expand-file-name other-project) *psv/chromium-project-root-path*)
                                             (concat *psv/chromium-project-root-path*
                                                     (substring filepath (length "src/")))
                                           (concat other-project filepath))))
                     (if (f-exists? other-filepath)
                         (ediff (concat project filepath) other-filepath)
                       (user-error (format "path:%s doesn't exist" other-filepath))))))
      (user-error "There are no open projects"))))

;; search think at point at cs.chromium.org
(defconst psv/chromium-code-search-format "https://cs.chromium.org/search/?q=%s&sq=package:chromium&type=cs"
  "Chromium code search format, %s = SEARCH TERM.")

(defun psv/chromium-code-search-at-point ()
  "Search symbol at point at chromium sources."
  (interactive
   (let ((term (read-string "Search term: " (thing-at-point 'symbol))))
     (browse-url-default-browser (format psv/chromium-code-search-format term)))))

;; ripgrep specific files
(defconst *psv/ripgrep-mojom-files* '("*.mojom"))
(defconst *psv/ripgrep-build-files* '("*.gn" "DEPS"))
(defconst *psv/ripgrep-cpp-test-files* '("*test.cc" "*tests.cc"))

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

(defun psv/projectile-ripgrep-build (regexp)
    "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep build files for: " (thing-at-point 'symbol))))
  (psv/projectile-ripgrep regexp
                          nil
                          *psv/ripgrep-build-files*
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

(define-key projectile-mode-map (kbd "C-c p s p") 'psv/projectile-ripgrep-cpp)
(define-key projectile-mode-map (kbd "C-c p s y") 'psv/projectile-ripgrep-py)
(define-key projectile-mode-map (kbd "C-c p s b") 'psv/projectile-ripgrep-build)
(define-key projectile-mode-map (kbd "C-c p s m") 'psv/projectile-ripgrep-mojom)
(define-key projectile-mode-map (kbd "C-c p s t") 'psv/projectile-ripgrep-tests)

(provide 'bioh-nix)
;;; bioh-nix ends here
