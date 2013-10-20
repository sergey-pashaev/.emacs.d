;;; russian-config.el

(defun toggle-russian-input-method ()
  (interactive)
  (if (string= current-input-method "russian-computer")
      (progn
        (inactivate-input-method))
    (progn
      (set-input-method "russian-computer"))))

;; toggle input method
(global-set-key (kbd "C-\\") 'toggle-russian-input-method)

(provide 'russian-config)
