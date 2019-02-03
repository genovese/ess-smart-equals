(require 'f)

(defvar ess-smart-equals-root-path
  (f-parent (f-dirname load-file-name)))

(add-to-list 'load-path ess-smart-equals-root-path)

;; Cask handles this
;;(defvar ess-smart-equals-ess-path
;;  (f-join (f-dirname (locate-library "ess-autoloads")) "lisp"))
;; 
;;(add-to-list 'load-path ess-smart-equals-ess-path)

(require 'ess-smart-equals)
