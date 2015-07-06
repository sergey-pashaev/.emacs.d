;;; org-config.el

(setq calendar-week-start-day 1)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c r") 'org-remember)

(setq org-agenda-files (list
                        ))

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "C-c t") 'org-time-stamp)
             (org-indent-mode t)
             (visual-line-mode t)))

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

(provide 'org-config)
