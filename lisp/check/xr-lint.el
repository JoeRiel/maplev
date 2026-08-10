
;; Run xr-lint

;; (load-file "~/github/xr/xr.el")

(cl-loop for re in '(
                     maplev--assignment-re
                     maplev--compile-error-re
                     maplev--declaration-re
                     maplev--defun-begin-re
                     maplev--defun-end-re
                     maplev--deprecated-re
                     maplev--ditto-operators-re
                     maplev--expr-re
                     maplev--help-definition-re
                     maplev--help-section-re
                     maplev--help-subsection-re
                     maplev--include-directive-re
                     maplev--initial-variables-re
                     maplev--link-re
                     maplev--module-export-re
                     maplev--name-re
                     maplev--number-re
                     maplev--operator-re
                     maplev--possibly-typed-assignment-re
                     maplev--preprocessor-directives-re
                     maplev--protected-names-re
                     maplev--quote-re
                     maplev--quoted-name-re
                     maplev--simple-name-re
                     maplev--special-words-re
                     maplev--string-re
                     maplev--symbol-re
                     maplev--top-defun-begin-re
                     maplev--trace-re
                     maplev--undocumented-names-re
                     maplev-indent-grammar-keyword-re
                     maplev-mint-variables-re
                     maplev-partial-end-defun-re
                     maplev-sb-assign-re
                     maplev-sb-keyword-re
                     maplev-wexp-statement-cont-re
                     maplev-wexp-statement-start-re
                     )
         do (if (xr-lint (eval re))
                (insert (format "\n%s = %s" re (xr-lint (eval re))))))
