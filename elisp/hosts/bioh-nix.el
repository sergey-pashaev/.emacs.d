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

(defun psv/chromium-project-path-p (path)
  "Whether given PATH is chromium project path."
  (s-starts-with? *psv/chromium-project-root-path* path))

;; (defun psv/chromium-buffer-relative-path()
;;   "Return current buffer path relative to chromium project root."
;;   (interactive)
;;   (let ((root (projectile-project-root)))
;;     (if (and root (psv/chromium-project-path-p root))
;;         (let* ((abs-path (psv/buffer-file-path))
;;                (rel-path (substring abs-path (length root))))
;;           (if (s-starts-with? "src/" rel-path)
;;               (substring rel-path (length "src/"))
;;             rel-path))
;;       (user-error "Not in chromium project"))))

(defun psv/chromium-buffer-relative-path()
  "Return current buffer path relative to chromium project root."
  (interactive)
  (let ((root (projectile-project-root)))
    (if root
        (let* ((abs-path (psv/buffer-file-path))
               (rel-path (substring abs-path (length root))))
          (if (s-starts-with? "src/" rel-path)
              (substring rel-path (length "src/"))
            rel-path)))))

  ;; (let ((path (substring (psv/buffer-file-path) ; cut project root
  ;;                        (length (projectile-project-root)))))
  ;;   (if (s-starts-with? "src/" path)
  ;;       (substring path (length "src/"))
  ;;     path)))

(defun psv/yb-project-path-p (path)
  "Whether given PATH is yandex-browser project path."
  (s-starts-with? (expand-file-name "~/workspace/ya/browser") path))

(defun psv/yb-buffer-relative-path ()
  "Return current buffer path relative to browser project root."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (psv/yb-project-path-p root))
        (let* ((abs-path (psv/buffer-file-path))
               (rel-path (substring abs-path (length root))))
          rel-path)
      (user-error "Not in yandex-browser project"))))

;; gn refs
(defun psv/gn-refs ()
  "Run gn refs for current file.
List all gn refs that using current file in *psv/gn-ref* buffer."
  (interactive)
  (when (projectile-project-root)
    (let ((file (psv/buffer-file-path))
          (dir (concat (projectile-project-root) "src/")))
      (let ((default-directory dir)
            (program "/home/bioh/workspace/ya/depot_tools/gn")
            (cmd (format " refs out/Debug/ --all %s" file))
            (buf (get-buffer-create "*psv/gn-ref*")))
        (with-current-buffer buf
          (kill-region (point-min) (point-max))
          (insert (format "cmd: %s refs out/Debug/ -all %s\n\n" program file)))
        (let ((proc (start-process "psv/gn-ref-proc" buf program "refs" "out/Debug/" "-all" file)))
;          (set-process-filter proc 'psv/gn-refs-filter-function)
          (set-process-sentinel proc 'psv/gn-refs-sentinel)
          (message "gn refs started...")
          (switch-to-buffer-other-window buf))))))

(defcustom psv/gn-refs-test-cmd "format string %s"
  "doc"
  :type 'string)

(defun psv/gn-refs-sentinel (proc _msg)
  "Process entire output of PROC line-wise."
  (when (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc))
             (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward ":\\([a-zA-Z_]+\\)" (point-max) t)
            (let ((target (match-string 1)))
              (psv/gn-refs-match-button 0 (format
                                           "ninja -C out/Debug -j 50 %s && ./out/Debug/%s --enable-pixel-output-in-tests --gtest_filter= --gtest_repeat=1 2>&1 | browser_log.py" target target)))))))))

(defun psv/gn-refs-filter-function (proc string)
  "Nop process filter function."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun psv/gn-refs-button-action (button)
  (psv/put-to-clipboard (button-get button 'cmd))
  (message "Copied: %s" (button-get button 'cmd)))

(defun psv/gn-refs-make-button (beg end cmd)
  (make-button beg end
               'action 'psv/gn-refs-button-action
               'follow-link t
               'cmd cmd
               'help-echo cmd))

(defun psv/gn-refs-match-button (match cmd)
  "Create button out of MATCH with given CMD as action."
  (psv/gn-refs-make-button (match-beginning match) (match-end match) cmd))

(defun psv/copy-projectile-buffer-relative-path-to-clipboard ()
  "Put the current file name to clipboard."
  (interactive)
  (let ((filename (psv/projectile-buffer-relative-path)))
    (psv/put-to-clipboard filename)
    (message "Copied: %s" filename)))

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

;; symbol reference url
(defconst psv/chromium-symbol-reference-format "https://source.chromium.org/chromium/chromium/src/+/%s:%s;?q=%s&ss=chromium"
  "Chromium code search symbol reference format.")

(defconst psv/yb-symbol-reference-format "<project_url>/browse/%s?at=refs%%2Fheads%%2F%s#%d"
  "YB bitbucket code reference format.")

(require 'url-util)

(defun psv/yb-make-line-reference (branch path line)
  "BRANCH = 'master', PATH = 'src/base/files/file.h', LINE = '32'."
  (format psv/yb-symbol-reference-format path (url-hexify-string branch) line))

(defun psv/yb-line-reference ()
  (interactive)
  (psv/yb-make-line-reference "master" ;(magit-get-current-branch)
                              (psv/yb-buffer-relative-path)
                              (line-number-at-pos (point))))

(defun psv/yb-code-go-to-line ()
  (interactive)
  (browse-url-default-browser (psv/yb-line-reference)))

(defun psv/yb-code-line-org (start end)
  (interactive "r")
  (let ((symbol (buffer-substring-no-properties start end))
        (url (psv/yb-line-reference)))
    (psv/put-to-clipboard (format "[[%s][%s]]" url symbol))))

(defun psv/make-chromium-symbol-reference (branch path symbol)
  "BRANCH = 'master', PATH =
'components/viz/service/surfaces/surface.cc', SYMBOL =
Surface::OnActivationDependencyResolved'."
  (format psv/chromium-symbol-reference-format branch path symbol))

(defun psv/chromium-symbol-reference ()
  (let ((symbol (read-string "Symbol: " (thing-at-point 'symbol)))
        (filepath (psv/chromium-buffer-relative-path)))
    (psv/make-chromium-symbol-reference "master" filepath symbol)))

(defun psv/chromium-copy-symbol-reference-org (start end)
  (interactive "r")
  (let ((symbol (buffer-substring-no-properties start end))
        (path (psv/chromium-buffer-relative-path)))
    (psv/put-to-clipboard (format "[[%s][%s]]"
                                  (psv/make-chromium-symbol-reference "master" path symbol)
                                  symbol))))

(defun psv/chromium-code-go-to-symbol-at-point ()
  (interactive)
  (let ((ref (psv/chromium-symbol-reference)))
    (browse-url-default-browser ref)))

(defun psv/chromium-code-go-to-symbol (start end)
  (interactive "r")
  (let ((symbol (buffer-substring-no-properties start end))
        (path (psv/chromium-buffer-relative-path)))
    (browse-url-default-browser (psv/make-chromium-symbol-reference "master" path symbol))))

(defun psv/chromium-code-search-at-point ()
  "Search symbol at point at chromium sources."
  (interactive
   (let ((term (read-string "Search term: " (thing-at-point 'symbol))))
     (browse-url-default-browser (format psv/chromium-code-search-format term)))))

;; ripgrep specific files
(defconst *psv/ripgrep-mojom-files* '("*.mojom"))
(defconst *psv/ripgrep-build-files* '("*.gn" "DEPS"))
(defconst *psv/ripgrep-yaml-files* '("*.yaml"))
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

(defhydra psv/hydra-buffer (:hint t)
  "Current buffer operations"
  ("n" psv/copy-file-name-to-clipboard "copy file name")
  ("i" psv/copy-include-statement-to-clipboard "copy #include statement")
  ("r" psv/copy-projectile-buffer-relative-path-to-clipboard "copy project relative path")
  )

(defhydra psv/hydra-projectile-ripgrep (:hint t)
  "Projectile ripgrep at:"
  ("n" psv/projectile-ripgrep-current-filename "current filename")
  ("p" psv/projectile-ripgrep-cpp "c++ files (*.h, *.cc, etc.)")
  ("y" psv/projectile-ripgrep-py "python files (*.py)")
  ("b" psv/projectile-ripgrep-build "build files")
  ("m" psv/projectile-ripgrep-mojom "mojom files (*.mojom)")
  ("t" psv/projectile-ripgrep-tests "c++ test files (*test.cc)")
  ("y" psv/projectile-ripgrep-yaml "yaml files (*.yaml)")
  )

(provide 'bioh-nix)
;;; bioh-nix ends here
