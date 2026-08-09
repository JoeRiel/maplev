
(require 'xtest)
(require 'maplev)

(ert-test-erts-file "maplev-indent.erts"
                    (lambda ()
                      (maplev-mode)
                      (indent-region (point-min) (point-max))))
