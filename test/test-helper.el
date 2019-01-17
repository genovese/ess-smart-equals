;; test-helper.el -- ert-runner setup   -*- lexical-binding: t; -*-
;;
;; Run automatically
;;
;;    cask exec ert-runner
;; 
;; or manually with
;;    cask emacs -Q
;;    M-x ert
;;    

(require 's)
(require 'f)

(setq debug-on-error nil)

(defvar ess-smart-equals-root-path
  (f-parent (f-dirname load-file-name)))

(defvar ess-smart-equals-major-mode #'ess-r-mode
  "Major mode to use in smart-equals ecukes tests.")

(add-to-list 'load-path ess-smart-equals-root-path)

(setq ess-smart-equals-extra-ops '(brace paren percent))

(require 'ess)
(require 'ess-smart-equals)

(ess-smart-equals-activate)

(defmacro with-ess-buffer (&rest body)
  `(with-temp-buffer
     (funcall ess-smart-equals-major-mode)
     (ess-smart-equals-mode 1)
     ,@body))
