;;; projectile-config.el

(require-or-install 'projectile)

(setq projectile-cache-file (concat tempfiles-dir "projectile.cache"))
(setq projectile-known-projects-file (concat tempfiles-dir "projectile-bookmarks.eld"))

(projectile-global-mode)

(provide 'projectile-config)
