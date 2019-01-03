;;; ess-smart-equals.el --- better smart-assignment with =-key in R and S  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2015 Christopher R. Genovese, all rights reserved.

;; Author: Christopher R. Genovese <genovese@cmu.edu>
;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;; Keywords: R, S, ESS, convenience
;; URL: https://github.com/genovese/ess-smart-equals
;; Version: 0.2.0
;; Package-Version: 0.2.0
;; Package-Requires: ((emacs "25") (ess "16.10"))


;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


;;; Commentary:
;;
;;  Assignment in R is syntactically complicated by a few features:
;;  1. the historical role of '_' (underscore) as an assignment
;;  character in the S language; 2. the somewhat
;;  inconvenient-to-type, if conceptually pure, '<-' operator as the
;;  preferred assignment operator; 3. the ability to use either an
;;  '=' or an '<-' for assignment; and 4. the multiple roles that
;;  '=' can play, including for setting named arguments in a
;;  function call.
;;
;;  This package gives an alternative smart assignment operator for
;;  R (i.e., S) code that is tied to the '=' key; in fact, it
;;  handles assignment and comparison operators as well as named
;;  argument setting. It uses context in the code to intelligently
;;  guess which operator is intended and then allows very easy
;;  cycling through the possible operators. The contexts and the
;;  operators that are cycled through in each context are
;;  customizable.
;;
;;  The package defines a minor mode `ess-smart-equals-mode',
;;  intended for S-language modes (e.g., ess-r-mode,
;;  inferior-ess-r-mode, and ess-r-transcript-mode), that when
;;  enabled in a buffer activates the '=' key to to handle
;;  context-sensitive completion and cycling of relevant operators.
;;  When the mode is active and an '=' is pressed:
;;
;;   1. In a string or comments, just insert '='.
;; 
;;   2. If an operator relevant to the context lies before point
;;      (with optional whitespace), it is replaced, cyclically, by the
;;      next operator in the configured list for that context.
;;
;;   3. Otherwise, if a prefix of an operator relevant to the
;;      context lies before point, that operator is completed.
;;
;;   4. Otherwise, the highest priority relevant operator is inserted
;;      with surrounding whitespace (see `ess-smart-equals-no-spaces').
;;
;;  Consecutive presses of '=' cycle through the relevant operators.
;;  With a prefix argument, '=' always just inserts an '='.
;;
;;  By default, the minor mode activates the '=' key, but this can
;;  be customized with the option `ess-smart-equals-key'.
;;
;;  The function `ess-smart-equals-activate' arranges for the minor mode
;;  to be activated by mode hooks for any given list of major modes,
;;  defaulting to ESS major modes associated with R (ess-r-mode,
;;  inferior-ess-r-mode, ess-r-transcript-mode, ess-roxy-mode). 
;;
;;  Examples
;;  --------
;;  In the left column below, ^ marks the location at which an '='
;;  key is pressed, the remaining columns show the result of
;;  consecutive presses of '=' using the package's default settings.
;;  position of point.
;;
;;     Before '='         Press '='      Another '='       Another '='
;;     ----------         ---------      -----------       -----------
;;     foo^               foo <- ^       foo <<- ^         foo = ^
;;     foo  ^             foo  <- ^      foo  <<- ^        foo  = ^
;;     foo<^              foo <- ^       foo <<- ^         foo = ^
;;     foo=^              foo = ^        foo -> ^          foo ->> ^
;;     foo(a^             foo(a = ^      foo( a == ^       foo( a != ^
;;     if( foo=^          if( foo == ^   if( foo != ^      if( foo <= ^
;;     if( foo<^          if( foo < ^    if( foo > ^       if( foo >= ^
;;     "foo ^             "foo =^        "foo ==^          "foo ===^
;;     #...foo ^          #...foo =^     #...foo ==^       #...foo ===^
;;
;;
;;   As a bonus, if `ess-smart-equals-extra-ops' is non-nil when
;;   this package is loaded, this package also defines some other
;;   smart operators that may prove useful. If it is set to the
;;   symbol `bind', then `ess-smart-equals-activate' binds the
;;   associated electric keys in mode keymaps. Currently, only
;;   `essmeq-electric-brace' is defined, intended to be bound to
;;   '{'; it configurably places a properly indented and spaced
;;   matching pair of braces at point or around the region if
;;   active.
;;
;;   Finally, the primary user facing functions are named with a
;;   prefix `ess-smart-equals-' to avoid conflicts with other
;;   packages. Because this is long, the internal functions and
;;   objects use a shorter (but still distinctive prefix) `essmeq-'.
;;   
;;
;;  Installation and Initialization
;;  -------------------------------
;;  The package can be loaded from MELPA using `package-install' or another
;;  Emacs package manager. Alternatively, you can clone or download the source
;;  directly from the github repository and put the file `ess-smart-equals.el'
;;  in your Emacs load path.
;;
;;  To activate, you need only do
;;
;;      (require 'ess-smart-equals)
;;      (ess-smart-equals-mode 1)
;;
;;  somewhere in your init file. For those who use the outstanding
;;  `use-package', you can do
;;
;;      (use-package ess-smart-equals
;;        :after (ess-site)
;;        :config (ess-smart-equals-activate))
;;
;;  somewhere in your init file. An equivalent but less concise version
;;  of this is
;;
;;      (use-package ess-smart-equals
;;        :after (ess-site)
;;        :hook ((ess-r-mode . ess-smart-equals-mode)
;;               (inferior-ess-r-mode . ess-smart-equals-mode)
;;               (ess-r-transcript-mode . ess-smart-equals-mode)
;;               (ess-roxy-mode . ess-smart-equals-mode))
;;               
;;  To also activate the smart brace operator and bind it to '{'
;;  automatically, you can replace this with
;;
;;      (use-package ess-smart-equals
;;        :init   (setq ess-smart-equals-extra-ops 'bind)
;;        :after  (ess-site)
;;        :config (ess-smart-equals-activate))
;;
;;  Details on customization are provided in the README file.
;;  

;;; Change Log:
;;
;;  0.2.0 -- Breaking changes in functionality, design, and configuration.
;;           No longer relies on `ess-S-assign' which was deprecated in
;;           ESS. Now provides more powerful context-sensitive, prioritized
;;           operator lists with cycling and completion. The mode is now,
;;           properly, a local minor mode, which can be added automatically
;;           to relevant mode hooks for ESS R modes. Updated required
;;           versions of emacs and ESS.
;;           
;;  0.1.1 -- Initial release with simple insertion and completion, with
;;           space padding for the operators except for a single '=' 
;;           used to specify named arguments in function calls. Relies on
;;           ESS variables `ess-S-assign' and `ess-smart-S-assign-key'
;;           to specify preferred operator for standard assignments.

;;; Code:

(require 'ess-site)

(defvar ess-smart-equals-assign-key "="
  "Cached value of previous smart assignment key.")

(defun ess-smart-equals--strip-leading-space (string)
  "Strip one leading space from STRING, if present."
  (replace-regexp-in-string "\\` " "" string))

(defun ess-smart-equals--restore-leading-space (string)
  "Add one leading space to STRING, if none are present."
  (replace-regexp-in-string "\\`\\(\\S-\\)" " \\1" string))

(defun ess-smart-equals--maybe-narrow ()
  "Narrow to relevant part of buffer in various ess-related modes."
  (ignore-errors
    (when (and (eq major-mode 'inferior-r-ess-mode)
               (> (point) (process-mark (get-buffer-process (current-buffer)))))
      (narrow-to-region (process-mark (ess-get-process)) (point-max)))
    (and (boundp 'ess-noweb-mode)
         ess-noweb-mode
         (ess-noweb-in-code-chunk)
         (ess-noweb-narrow-to-chunk))
    (and (fboundp 'pm/narrow-to-span)
         (boundp 'polymode-mode)
         polymode-mode
         (pm/narrow-to-span))))

(defun ess-smart-equals--after-assign-p ()
  "Are we looking backward at `ess-smart-equals-assign-key'?
If so, return number of characters to its beginning; otherwise, nil."
  (let ((ess-assign-len (length ess-smart-equals-assign-key)))
    (when (and (>= (point) (+ ess-assign-len (point-min))) ; enough room back
               (save-excursion
                 (backward-char ess-assign-len)
                 (looking-at-p ess-smart-equals-assign-key)))
      ess-assign-len)))

;;;###autoload
(defun ess-smart-equals (&optional raw)
  "Insert, or substitute, a properly-spaced R assignment operator at point.
If an assignment operator is already present before point, it is replaced
by the next operator in `ess-smart-equals-operators', taken cyclically.
The order of these operators is somewhat dependent on context. For instance,
in the argument list of a function call, a single `=' is first rather 
than the standard `<-'; spacing in this case is also determined by
the value of `ess-smart-equals-space-named-arguments'.
For equal signs not preceded by spaces, as in argument lists,
just use equals.  This can effectively distinguish the two uses
of equals in every case.  When RAW is non-nil, the equals sign
is always inserted as is."
  (interactive "P")
  (save-restriction
    (ess-smart-equals--maybe-narrow)
    (let ((prev-char (preceding-char)))
      (cond
       ((or raw
            (not (equal ess-language "S"))
            (not (string-match-p "[ \t=<>!]" (string prev-char)))
            (ess-inside-string-or-comment-p (point)))
        (insert "="))
       ((string-match-p "[=<>!]" (string prev-char))
        (when (save-excursion
                (goto-char (- (point) 2)) ; OK if we go past beginning (ignore-errors (backward-char 2))
                (not (looking-at-p "[ \t]")))
          (delete-char -1)
          (insert " " prev-char))
        (insert "= "))
       (t
        (let ((back-by (ess-smart-equals--after-assign-p)))
          (if (not back-by)
              (insert "<- ")
            (delete-char (- back-by))
            (insert "== "))))))))

;;;###autoload
(define-minor-mode ess-smart-equals-mode
     "Minor mode for setting the '=' key to intelligently handle assignment.

When enabled for S-language modes, an '=' key uses the preceding character
to determine the intended construct (assignment, comparison, default argument).
Loosely, an '=' preceded by a space is converted to an assignment, an '='
preceded by a comparison (<>!=) becomes a space-padded comparison operator,
and otherwise just an '=' is inserted. The specific rules are as follows:

  1. In a string or comment or with a non-S language, just insert '='.
  2. If a space (or tab) preceeds the '=', insert a version of `ess-smart-equals-assign-key'
     with no leading space (e.g., '<- ') so that assignment is surrounded
     by at least one space. (Other preceeding spaces are left alone.)
  3. If any of '=<>!' preceed the current '=', insert an '= ', but
     if no space preceeds the preceeding character, insert a space
     so that the resulting binary operator is surrounded by spaces.
  4. If the `ess-smart-equals-assign-key' string (e.g., '<- ') precedes point,
     insert '== ' (a double *not* a single equals).
  5. Otherwise, just insert an '='.

With a prefix argument, '=' always just inserts an '='.

This is a global minor mode that will affect the use of '=' in
all ess-mode and inferior-ess-mode buffers. A local mode
may be included in a future version.

Do not set the variable `ess-smart-equals-mode' directly; use the
function of the same name instead. Also any changes to
`ess-smart-S-assign-key' while this mode is enabled will have no
effect and will be lost when the mode is disabled."
     :lighter nil
     :require 'ess-site
     (if (not ess-smart-equals-mode)
         (progn
           (define-key ess-r-mode-map ess-smart-equals-assign-key 'self-insert-command)
           (define-key inferior-ess-r-mode-map ess-smart-equals-assign-key 'self-insert-command)))
     (define-key ess-r-mode-map ess-smart-equals-assign-key 'ess-smart-equals)
     (define-key inferior-ess-r-mode-map ess-smart-equals-assign-key 'ess-smart-equals))


(provide 'ess-smart-equals)

;;; ess-smart-equals.el ends here
