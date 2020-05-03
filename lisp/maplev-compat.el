;;; maplev-compat.el --- Compatibility functions

;;; Commentary:
;; 

;; Define the functions needed for older versions of Emacs.

;;; Code:

;; font-lock-ensure was introduced in Emacs 25

(unless (fboundp 'font-lock-ensure)
  (defalias 'font-lock-ensure 'font-lock-fontify-buffer))

(unless (fboundp 'save-mark-and-excursion)
  (defalias 'save-mark-and-excursion 'save-excursion))


(provide 'maplev-compat)

;;; maplev-compat.el ends here
