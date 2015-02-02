(require 'f)


(defvar ess-smart-equals-root-path
  (f-parent (f-dirname load-file-name)))

(defvar ess-smart-equals-ess-path
  (f-join (f-dirname (locate-library "ess-autoloads")) "lisp"))

(add-to-list 'load-path ess-smart-equals-ess-path)
(add-to-list 'load-path ess-smart-equals-root-path)

(require 'ess-smart-equals)
