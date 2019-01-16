(require 'f)

(defvar ess-smart-equals-root-path
  (f-parent (f-dirname load-file-name)))

(defvar ess-smart-equals-ess-path
  (f-dirname (locate-library "ess")))

(defvar ess-smart-equals-major-mode #'ess-r-mode
  "Major mode to use in smart-equals ecukes tests.")

(add-to-list 'load-path ess-smart-equals-ess-path)
(add-to-list 'load-path ess-smart-equals-root-path)

(setq ess-smart-equals-extra-ops '(brace paren percent))

(require 'ess)
(require 'ess-smart-equals)

(ess-smart-equals-activate)

(defmacro with-ess-buffer (&rest body)
  (with-temp-buffer
    (funcall ess-smart-equals-major-mode)
    (ess-smart-equals-mode 1)
    ,@body))
