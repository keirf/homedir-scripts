;;; middl-mode.el 
;; Copyright (C) 1991 Free Software Foundation, Inc.
;; Author: Paul Barham
;; Last-Modified: 14 Jul 1992
;; Keywords: tools, languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This mode was written by Eric S. Raymond <esr@snark.thyrsus.com>,
;; inspired by an earlier middl-mode by Martin Neitzel.

;;;
;;; USES DERIVED MODE
;;;

;;  (define-derived-mode middl-mode text-mode "Middl"
;;    "Major mode for middl.\n\n\\{middl-mode-map}"
;;    (setq case-fold-search nil))
;;
;;  (define-key middl-mode-map [down-mouse-3] 'do-hyper-link)
;;
;; will create a function `middl-mode' with its own (sparse)
;; keymap `middl-mode-map.'  The command M-x middl-mode will
;; perform the following actions:
;;
;; - run the command (text-mode) to get its default setup
;; - replace the current keymap with 'middl-mode-map,' which will
;;   inherit from 'text-mode-map'.
;; - replace the current syntax table with
;;   'middl-mode-syntax-table', which will borrow its defaults
;;   from the current text-mode-syntax-table.
;; - replace the current abbrev table with
;;   'middl-mode-abbrev-table', which will borrow its defaults
;;   from the current text-mode-abbrev table
;; - change the mode line to read "Middl"
;; - assign the value 'middl-mode' to the 'major-mode' variable
;; - run the body of commands provided in the macro -- in this case,
;;   set the local variable `case-fold-search' to nil.
;; - **run the command (middl-mode-setup), which is empty by
;;   default, but may be redefined by the user to contain special
;;   commands (ie. setting local variables like 'outline-regexp')
;;   **NOTE: do not use this option -- it will soon be obsolete.
;; - run anything assigned to 'middl-mode-hooks' (obsolete, but
;;   supported for the sake of compatibility).
;;

;;; Code:

(require 'filladapt)

(defvar middl-font-lock-keywords
  '(("\\(--.*\\)$" 1 font-lock-comment-face)
    ("[],:.;={}[]" . font-lock-string-face)
    ("\\<ADDRESS\\|ANNOUNCEMENT\\|ARRAY\\|BEGIN\\|BOOLEAN\\|CARDINAL\\|CHAR\\|CHOICE\\|DANGEROUS\\|END\\|EXCEPTION\\|EXPORTS\\|EXTENDS\\|IMPORTS\\|INTEGER\\|INTERFACE\\|IREF\\|LONG\\|LOCAL\\|REF\\|NEEDS\\|PROC\\|PARAMETERS\\|RAISES\\|REAL\\|RECORD\\|RETURNS\\|SEQUENCE OF\\|SET OF\\|STRING\\|TYPE\\>" . font-lock-keyword-face)
    )
  "Additional expressions to highlight in MIDDL mode.")

(defun middl-untabify () 
  (interactive)
  (untabify (point-min) (point-max)))
(define-derived-mode middl-mode fundamental-mode "MIDDL"
  "Major mode for MIDDL.\n\n\\{middl-mode-map}"
  (auto-fill-mode 1)
  (add-hook 'write-contents-hooks 'middl-untabify)
  (setq tab-stop-list '(2 4 6 8 10 12))
  (setq filladapt-debug t)
  (setq filladapt-mode 'minor)
  (setq filladapt-token-table 
    '(("[ \t]*--[ \t]*" . middl-comment)
      ("[ \t]+" . space)
      ("$" . end-of-line)))
  (setq filladapt-token-match-table 
	'((middl-comment middl-comment)))
  (setq filladapt-token-conversion-table 
	'((middl-comment . exact)))    
  (set (make-local-variable 'font-lock-defaults)
       '(middl-font-lock-keywords t))
)
(define-key middl-mode-map "]"  'middl-close-params)
(define-key middl-mode-map "\t" 'middl-tab)

(defun middl-format-parameters ()
  "Align colons in region."
  (interactive)
  (save-excursion
    (let ((start (search-backward "[" (point-min) t))
	  (beg   (progn (beginning-of-line) (point)))
	  (end (search-forward "]" (point-max) t)))
      (and start end 
	   (save-restriction
	     (narrow-to-region beg end)
	     (goto-char start)
	     (while (re-search-forward "[ \t]+" nil t)
	       (replace-match " "))
	     (align-regexp "\\<\\w+ *:[^[]*$" (point-min) (point-max) 0)
	     (align-regexp ":[^[]*$" (point-min) (point-max) 0)
	     (widen)
	     )
	   )
      )
    )
)
(defun middl-close-params ()
  (interactive)
  (insert-char ?] 1)
  (middl-format-parameters))

(defun middl-tab ()
  (interactive)
  (let ((bound (point))
	(notparams nil)
	col)
    (save-excursion
      (search-backward "[" (point-min) t)
      (setq notparams (search-forward "]" bound t))
      (goto-char bound)
      (forward-line -1) (beginning-of-line) 
      (re-search-forward "\\<\\w+ *:[^[]*$" nil t)
      (goto-char (match-beginning 0))
      (setq col (current-column))
      )
    (if notparams
	(tab-to-tab-stop)
      (indent-line-to col))))

;;; middl-mode.el ends here
				   
