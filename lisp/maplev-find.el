;;; maplev-find.el --- Find files for Maple

;;; Commentary:
;;
;; The function `maplev-find-file' is used
;; by maplev-mint when jumping to the source
;; of a mint error/warning.

;;; Code:
;;

(require 'eieio)
(require 'maplev-config)

(defvar maplev-find-executable "find")

(defun maplev-find-file (file &optional dir)
  "Find location of FILE, starting at DIR.
If DIR is nil, use the :project-root slot of `maplev-config'."
  (let ((dir (or dir (or (oref maplev-config :project-root)
			 (error "no dir or :project-root slot of maplev-config specified"))))
	(cmd (format "%s . -name %s" 
		     maplev-find-executable
		     file))
	files num)
    (cd (file-name-as-directory dir))
    (setq files (mapcar
		 (lambda (file)
		   (concat dir (substring file 1)))
		 (split-string (shell-command-to-string cmd))))
    (setq num (length files))
    (cond 
     ((zerop num) (error "no file found"))
     ((= num 1) (car files))
     (t (error "multiple files found")))))
  
(provide 'maplev-find)

;;; maplev-find.el ends here

