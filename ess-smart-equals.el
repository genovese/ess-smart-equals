;;; ess-smart-equals.el --- better smart-assignment with =-key in R and S  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2015 Christopher R. Genovese, all rights reserved.

;; Author: Christopher R. Genovese <genovese@cmu.edu>
;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;; Keywords: R, S, ESS, convenience
;; URL: https://github.com/genovese/ess-smart-equals
;; Version: 0.2.1
;; Package-Version: 0.2.1
;; Package-Requires: ((emacs "24") (ess "5.00"))


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
;;  Assignment in R is syntactically complicated by three features: 1. the
;;  historical role of '_' (underscore) as an assignment character in
;;  the S language (SPlus may still allow this); 2. the somewhat
;;  inconvenient-to-type, if conceptually pure, '<-' operator as the
;;  preferred assignment operator; and 3. the ability to use either
;;  an '=' or an '<-' for assignment.
;;
;;  ESS uses '_' as a (default) smart assignment character which expands
;;  to the '<-' with one invokation and gives an underscore on two.
;;  This makes it rather painful to use underscores in variable, field,
;;  and function names. Moreover, _ no longer has any association with
;;  assignment in R, so the mnemonic is strained.
;;
;;  It is possible to reassign the special underscore to another character,
;;  such as '=', but that raises other inconviences because of the multiple
;;  roles that '=' can play (assignment and default argument setting).
;;
;;  This package gives an alternative smart assignment for R and S code
;;  that is tied to the '=' key instead of the '_' key. It intelligently
;;  handles the various ways that '=' is used in R (and S) by examining
;;  the preceding context. It works under the assumption that '=' used
;;  for default arguments to functions *will not* be surrounded by
;;  spaces but that binary operators involving '=' /should be/. When
;;  this is enabled, underscore is completely divorced from assignment
;;  and thus can be used directly in names.
;;
;;  This package defines a global minor mode `ess-smart-equals-mode', that
;;  when enabled for S-language modes causes the '=' key to use the
;;  preceding character to determine the intended construct (assignment,
;;  comparison, default argument). Loosely speaking, an '=' preceded by a
;;  space is converted to an assignment, an '=' preceded by a comparison
;;  character (<>!=) becomes a space-padded comparison operator, and
;;  otherwise just an '=' is inserted. The specific rules are as follows:
;;
;;   1. In a string or comment or with a non-S language, just insert '='.
;;   2. If a space (or tab) preceeds the '=', insert a version of
;;      `ess-smart-equals-assign-key' with no leading space (e.g., "<- ").
;;      (Other preceeding spaces are left alone.)
;;   3. If any of =<>! preceed the current '=', insert an '= ', but
;;      if no space preceeds the preceeding character, insert a space
;;      so that the resulting binary operator is surrounded by spaces.
;;   4. If the `ess-smart-equals-assign-key' string (e.g., "<- ") precedes
;;      point, insert '== ' (a double *not* a single equals).
;;   5. Otherwise, just insert an '='.
;;
;;  With a prefix argument, '=' always just inserts an '='.
;;
;;  These insertions ensure that binary operators have a space on either
;;  end but they do not otherwise adjust spacing on either side. Note that
;;  in #4 above, the second '=' key is assumed to be intended as an equality
;;  comparison because the assignment would have been produced by an '='
;;  following a space.
;;
;;  Examples: In the left column below, ^ marks the location at which an '='
;;  key is pressed, and in the right column it marks the resulting
;;  position of point.
;;
;;     Before '='         After '='
;;     ----------         ---------
;;     foo ^              foo <- ^
;;     foo     ^          foo     <- ^
;;     foo(a^             foo(a=^
;;     foo=^              foo == ^
;;     foo<^              foo <= ^
;;     "foo ^             "foo =^
;;     #...foo ^          #...foo =^
;;     foo <- ^           foo == ^
;;
;;
;;  Installation
;;  ------------
;;  Either put this file on your load path
;;  Disabling the minor mode restores (as well as possible) the previous
;;  ESS assignment setup.
;;

;;; Change Log:
;;
;;  0.2.2 -- Fix for deprecated ESS variables `ess-S-assign' and
;;           `ess-smart-S-assign-key'. Thanks to Daniel Gomez (@dangom).
;;
;;  0.2.1 -- Initial release with simple insertion and completion, with
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
  "Insert an R assignment for equal signs preceded by spaces.
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
