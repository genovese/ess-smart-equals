(require 'f)
(require 'subr-x)

(defvar ess-smart-equals-root-path
  (thread-first load-file-name
    f-dirname
    f-parent
    f-parent))

(defvar ess-smart-equals-ess-path
  (f-dirname (locate-library "ess")))

(defvar ess-smart-equals-major-mode #'ess-r-mode
  "Major mode to use in smart-equals ecukes tests.")

(add-to-list 'load-path ess-smart-equals-ess-path)
(add-to-list 'load-path ess-smart-equals-root-path)

(require 'ert)
(require 'espuds)

(defun espuds-add-to-chain (v)
  "Add sequence v to the current action chain."
  (setq espuds-action-chain (vconcat espuds-action-chain v)))

(Setup
 (when (boundp 'flymake-diagnostic-functions)
   (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))
 (require 'ess)
 (setq ess-smart-equals-extra-ops '(brace paren percent))
 (require 'ess-smart-equals)
 (ess-smart-equals-activate))

(Setup
 (when (boundp 'flymake-diagnostic-functions)
   (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)))

(Before
 (switch-to-buffer
  (get-buffer-create "*ess-smart-equals-tests*"))
 (erase-buffer)
 (funcall ess-smart-equals-major-mode)
 (ess-smart-equals-mode 1))

(After
 (kill-buffer "*ess-smart-equals-tests*"))


;; Local Variables:
;; no-byte-compile: t
;; End:
