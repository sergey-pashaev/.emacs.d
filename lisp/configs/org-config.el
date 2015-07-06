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

(org-babel-do-load-languages 'org-babel-load-languages
                            '((emacs-lisp . t)
                              (sh . t)
                              (ditaa . t)
                              (plantuml . t)
                              (dot . t)
                              (gnuplot . t)
                              (org . t)))

;; (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
;; (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")

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
      (quote (("t" "Todo" entry (file "~/Dropbox/org/todo.org")
               "* TODO %?\n")
              ("T" "Todo (+time+link)" entry (file "~/Dropbox/org/todo.org")
               "* TODO %?\n%U\n%a\n")
              ("n" "Note" entry (file "~/Dropbox/org/notes.org")
               "* %? :note:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("l" "Link" entry (file+heading "~/Dropbox/org/links.org" "Links")
               "* %? :link:\n" :empty-lines 1))))

(provide 'org-config)
