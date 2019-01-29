(require 'f)
(require 'subr-x)

(defvar ess-smart-equals-root-path
  (thread-first load-file-name
    f-dirname
    f-parent
    f-parent))

(add-to-list 'load-path ess-smart-equals-root-path)

(defvar ess-smart-equals-major-mode #'ess-r-mode
  "Major mode to use in smart-equals ecukes tests.")

(require 'ert)
(require 'espuds)

(require 'ess)

(setq ess-smart-equals-extra-ops '(brace paren percent))
(let ((load-prefer-newer t))
  (require 'ess-smart-equals))
(ess-smart-equals-activate)

(defun espuds-add-to-chain (v)
  "Add sequence v to the current action chain."
  (setq espuds-action-chain (vconcat espuds-action-chain v)))

(Setup
 (when (boundp 'flymake-diagnostic-functions)
   (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)))

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
