;;; base-diminish.el --- Base diminish configuration

;;; Commentary:

;;; Code:

(diminish 'abbrev-mode)

(defun flymake--transform-mode-line-format (ret)
  "Change the output of `flymake--mode-line-format'."
  (setf (seq-elt (car ret) 1) " fm") ret)

(advice-add #'flymake--mode-line-format
            :filter-return #'flymake--transform-mode-line-format)

(provide 'base-diminish)
;;; base-diminish.el ends here
