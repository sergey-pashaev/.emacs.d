;;; gtranslate-config.el

(require-or-install 'google-translate)

(setq google-translate-default-target-language "ru")
(setq google-translate-default-source-language "en")

(defun psv/gtr-en-ru (p1 p2)
  (interactive "r")
  (google-translate-translate "en" "ru" (replace-regexp-in-string "\\(\\\n\\|[ ]+\\)" " " (buffer-substring-no-properties p1 p2))))

(defun psv/gtr-ru-en (p1 p2)
  (interactive "r")
  (message (buffer-substring-no-properties p1 p2))
  (google-translate-translate "ru" "en" (replace-regexp-in-string "\\(\\\n\\|[ ]+\\)" " " (buffer-substring-no-properties p1 p2))))

(provide 'gtranslate-config)
