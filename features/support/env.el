(require 'f)

(defvar ess-smart-equals-support-path
  (f-dirname load-file-name))

(defvar ess-smart-equals-features-path
  (f-parent ess-smart-equals-support-path))

(defvar ess-smart-equals-root-path
  (f-parent ess-smart-equals-features-path))

(defvar ess-smart-equals-ess-path
  (f-join (f-dirname (locate-library "ess-autoloads")) "lisp"))

(add-to-list 'load-path ess-smart-equals-ess-path)
(add-to-list 'load-path ess-smart-equals-root-path)

(require 'ert)
(require 'espuds)
(require 'ess-smart-equals)

(Before
 (switch-to-buffer
  (get-buffer-create "*ess-smart-equals-tests*"))
 (erase-buffer)
 (R-mode)
 (ess-smart-equals-mode 1))

(After
 (kill-buffer "*ess-smart-equals-tests*"))


;; Local Variables:
;; no-byte-compile: t
;; End:
