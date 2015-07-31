;;; org-config.el

(setq calendar-week-start-day 1)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "C-c t") 'org-time-stamp)
             (org-indent-mode t)
             (visual-line-mode t)))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(setq org-refile-use-outline-path 'file)

(org-babel-do-load-languages 'org-babel-load-languages
                            '((emacs-lisp . t)
                              (sh . t)
                              (ditaa . t)
                              (plantuml . t)
                              (dot . t)
                              (gnuplot . t)
                              (org . t)))

;; (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
;; (setq org-plantuml-jar-path "~/bin/plantuml.jar")

(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ditaa")
           (string= lang "dot")
           (string= lang "plantuml"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; follow links by RET
(setq org-return-follows-link t)

;; enable markdown export
(eval-after-load "org"
  '(require 'ox-md nil t))

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(eval-after-load "org"
  '(require 'ox-rst nil t))

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/notes.org")

(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/Dropbox/org/todo.org")  "* TODO %U %?\n")
              ("n" "Note" entry (file "~/Dropbox/org/notes.org") "* %U %?\n")
              ("l" "Link" entry (file "~/Dropbox/org/links.org") "* %U %?\n")
              ("o" "Todo (OPS)" entry (file "~/Dropbox/org/job/ops/todo.org.gpg")
               "* TODO %U %?\n  %i\n  %a"))))

(provide 'org-config)
