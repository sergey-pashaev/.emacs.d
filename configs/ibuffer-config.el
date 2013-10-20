;;; ibuffer-config.el

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org-mode" (or
                            (name . "^org$")
                            (mode . org-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("emacs lisp" (mode . emacs-lisp-mode))
               ("jabber / irc" (or
                          (name . "^\\*-jabber-chat-")
                          (name . "^\\*-jabber-roster-")
                          (mode . jabber-chat)
                          (mode . jabber-roster)
                          (mode . erc-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'ibuffer-config)
