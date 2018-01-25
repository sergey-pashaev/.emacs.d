;; org-mode
(defvar *psv/ditaa-path* (expand-file-name "~/bin/ditaa.jar"))
(defvar *psv/plantuml-path* (expand-file-name "~/bin/plantuml.jar"))

(defun psv/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ditaa")
           (string= lang "dot")
           (string= lang "plantuml"))))

(defun psv/org-mode-hook ()
  (bind-key "C-c t" 'org-time-stamp org-mode-map)
  (org-indent-mode t)
  (visual-line-mode t))

(use-package org-mode
  :init
  (progn
    (setq calendar-week-start-day 1
          org-confirm-babel-evaluate 'psv/org-confirm-babel-evaluate
          org-agenda-files '("~/Dropbox/org/gtd/")
	  org-agenda-custom-commands '(("x" "Daily habits"
					((agenda ""))
					((org-agenda-show-log t)
					 (org-agenda-ndays 7)
					 (org-agenda-log-mode-items '(state))
					 (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":daily:"))))
				       ("h" "Home"
					((agenda "")
					 (tags-todo "HOME")
					 (tags-todo "COMPUTER"))
					((org-agenda-ndays 7)
					 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":daily:"))))
				       ("o" "Office"
					((agenda "")
					 (tags-todo "OFFICE"))
					((org-agenda-ndays 7)
					 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":daily:"))))
				       ("b" "Buy list"
					((agenda "")
					 (tags-todo "STORE"))
					((org-agenda-ndays 7)
					 (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":daily:")))))
	  org-refile-targets '((org-agenda-files :maxlevel . 2))
	  org-refile-use-outline-path 'file
	  org-return-follows-link t	; follow links by ret
          org-ditaa-jar-path *psv/ditaa-path*
          org-plantuml-jar-path *psv/plantuml-path*
	  org-directory "~/Dropbox/org"
	  org-default-notes-file "~/Dropbox/org/notes.org"
	  org-capture-templates
	  (quote (("t" "Todo" entry (file "~/Dropbox/org/gtd/tasks.org")  "* TODO %?")
		  ("w" "Work Todo" entry (file "~/Dropbox/org/gtd/work/tasks.org")  "* TODO %?")
		  ("l" "Link" entry (file "~/Dropbox/org/links.org") "* %?")
		  ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org") "* %?"))))

    (add-hook 'org-mode-hook 'psv/org-mode-hook)

    ;; save the clock history across emacs sessions
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)

    ;; enable markdown export
    (eval-after-load "org"
      '(require 'ox-md nil t))

    (eval-after-load "org"
      '(require 'ox-gfm nil t))

    (eval-after-load "org"
      '(require 'ox-rst nil t))

    (org-babel-do-load-languages 'org-babel-load-languages
				 '((emacs-lisp . t)
				   (sh . t)
				   (ditaa . t)
				   (plantuml . t)
				   (dot . t)
				   (gnuplot . t)
				   (python . t)
				   (org . t))))
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c C-x C-j" . org-clock-goto)))

(provide 'base-org)