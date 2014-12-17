;;; common-lisp-config.el

(setq inferior-lisp-program "sbcl")

;; the SBCL configuration file is in Common Lisp 
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

;; Use SLIME from Quicklisp
(defun load-common-lisp-slime ()
  (interactive)
  ;; Common Lisp support depends on SLIME being installed with Quicklisp
  (if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
      (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (message "%s" "SLIME is not installed. Use Quicklisp to install it.")))

;; start slime automatically when we open a lisp file
(defun start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'start-slime)

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t)

     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)
     ))

(provide 'common-lisp-config)
