;; Initialization code for manual tests -*- lexical-binding: t; -*-
;;
;; Use with `cask emacs -Q', so we assume that 
;; cask has already setup the load path except for the package
;; directory.

(add-to-list 'load-path default-directory)
(setq ess-smart-equals-extra-ops '(brace paren percent))
(require 'ess)
(require 'ess-smart-equals)
(ess-smart-equals-activate)
