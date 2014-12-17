;;; ibuffer-config.el

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org-mode" (or
                            (name . "^org$")
                            (mode . org-mode)))
               ("emacs lisp" (mode . emacs-lisp-mode))
               ("magit" (name . "^\\*magit"))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("jabber / irc" (or
                          (name . "^\\*-jabber-chat-")
                          (name . "^\\*-jabber-roster-")
                          (mode . jabber-chat)
                          (mode . jabber-roster)
                          (mode . erc-mode)))
               ("c++ / c" (or
                           (mode . c-mode)
                           (mode . c++-mode)))
               ("cedet" (or
                         (name . "^\\*CEDET Global\\*$")
                         (name . "^\\*Semantic SymRef\\*$")
                         (name . "^\\*Symref ")))
               ("go lang" (or
                           (mode . go-mode)
                           (name . "^\\*godoc")
                           (name . "^\\*godef")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'ibuffer-config)
