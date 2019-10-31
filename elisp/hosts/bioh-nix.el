;;; bioh-nix.el --- bioh-nix specific configurations

;;; Commentary:

;;; Code:

;; Browse with yandex-browser
(require 'browse-url)

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

(provide 'bioh-nix)
;;; bioh-nix ends here
