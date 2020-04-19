;;; maplev-compat.el --- Compatibility functions

;;; Commentary:
;; 

;; Define the functions needed for older versions of Emacs.

;;; Code:

;; font-lock-ensure introduced in Emacs 25

(unless (fboundp 'font-lock-ensure)
  (defalias 'font-lock-ensure 'font-lock-fontify-buffer))

(provide 'maplev-compat)

;;; maplev-compat.el ends here
