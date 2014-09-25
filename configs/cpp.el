;; common c settings
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style "k&r"
      c-basic-offset 4)

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; ;; c-eldoc
;; (require-package 'c-eldoc)
;; (require 'c-eldoc)

;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;; (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)


;; ;; helm-gtags
;; (require-package 'helm-gtags)
;; (require 'helm-gtags)

;; (setq
;;  helm-gtags-ignore-case t
;;  helm-gtags-auto-update t
;;  helm-gtags-use-input-at-cursor t
;;  helm-gtags-pulse-at-cursor t

;;  helm-gtags-suggested-key-mapping t
;;  )

;; ;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; ;; when navigate project tree with Dired
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)

;; ;; Enable helm-gtags-mode in Eshell for the same reason as above
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)

;; ;; Enable helm-gtags-mode in languages that GNU Global supports
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)

;; ;; key bindings
;; ;(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; ;; ggtags
;; (require-package 'ggtags)
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; ;; company
;; (require-package 'company)
;; (require 'company)

;; (add-hook 'after-init-hook 'global-company-mode)
;; (delete 'company-semantic company-backends)

;; ;; company-c-headers
;; (require-package 'company-c-headers)
;; (require 'company-c-headers)
;; (add-to-list 'company-backends 'company-c-headers)
;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
