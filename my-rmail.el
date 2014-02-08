;;; MY-RMAIL.EL --- rmail customisation

;; Copyright (C) 1997 Paul Barham, 1999 Keir Fraser.

;; Author: Paul Barham Paul.Barham@cl.cam.ac.uk
;; Maintainer: Keir Fraser kaf24@cl.cam.ac.uk
;; Created: 22 Aug 1997
;; Last modified: 19 October 1999
;; Version: 1.1
;; Keywords:
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to Paul.Barham@cl.cam.ac.uk)
;; or from the Free Software Foundation, Inc., 675 Mass Ave,
;; Cambridge, MA 02139, USA.



;;;
;;; User-configurable options (PRB/KAF)
;;;

; MH-E requires help finding the program directories, as they're hidden away.
(setq mh-progs     "/usr/bin/mh")
(setq mh-lib       "/usr/bin/mh/lib")
(setq mh-lib-progs "/usr/bin/mh/lib")

; Once a message has been yanked to a reply, we no longer need the window.
; Also, don't yank header crap.
(setq mh-delete-yanked-msg-window t)
(setq mh-yank-from-start-of-msg 'body)

; Normal summary window height when showing a message is stupidly small!
(setq mh-summary-height 15)



;;;
;;; PRB's hacked user notation stuff for UNSEEN and URGENT messages.
;;; You can narrow to urgent messages with C-x n (rewiden with C-x w).
;;; Mark a message as urgent with % (unmark with M-%).
;;;
(require 'mh-e) ; force a load so that an internal defun can be rebound
(defun mh-notate-user-sequences ()
  (mapcar '(lambda (x)
             (let* ((name (mh-seq-name x)) 
                    (spec (cdr (assoc name mh-user-seq-notation))))
               (if spec (mh-notate-seq name (car spec) (car (cdr spec)))
                 (if (not (mh-internal-seq name)) 
                     (mh-notate-seq name mh-note-seq (1+ mh-cmd-note))))))
          mh-seq-list)
)
(setq mh-user-seq-notation '((unseen "u" 5) (urgent "*" 5)))
(defun mh-seen () 
  (mh-notate (mh-get-msg-num t) " " 5)
  (mh-update-unseen))
(add-hook 'mh-show-hook 'mh-seen)



;;;
;;; Ensure font locking happens in a timely fashion. (PRB)
;;;
(add-hook 'mh-show-mode-hook 'font-lock-fontify-buffer)
(add-hook 'mh-letter-mode-hook 'font-lock-mode)



;;;
;;; RMIME (PRB/KAF)
;;;
(add-hook 'mh-show-mode-hook 'rmime-format)
(autoload 'rmime-format "rmime" "" nil)


;; If a message contains mhn-style annotations, it must be processed 
;; before sending -- this bit of advice does that automatically (KAF)
(defadvice mh-send-letter (before force-mime activate) 
  (if (save-excursion (goto-char (point-min)) 
                      (re-search-forward "^#[^;\n]*;[^\n]*name=" nil t))
      (mh-edit-mhn)))


;; Redefine the standard "attachment" insertion function to auto-detect some
;; document types (e.g., .ps files are application/postscript) (KAF)
(defvar mh-mime-suffixes '(("ps" . "application/postscript") 
                           ("gif" . "image/gif")
                           ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
                           ("mpg". "video/mpeg") ("mpeg" . "video/mpeg")
                           ("rtf" . "text/richtext")
                           ("txt" . "text/plain"))
  "ALIST containing entries of the form (FILE SUFFIX . MIME TYPE).
This is searched by a hacked mh-mhn-compose-insertion before prompting the user
for a type.")
(require 'mh-mime)
(if (not (fboundp 'file-name-extension))
    (defsubst file-name-extension (f)
      (let ((index (string-match "\\.[^./]+$" f)))
        (if index (substring f (+ 1 index)))))) 
(defun mh-mhn-compose-insertion (filename type description attributes)
  "Add a directive to insert a MIME message part from a file.
This is the typical way to insert non-text parts in a message.
Arguments are FILENAME, which tells where to find the file, TYPE, the
MIME content type, and DESCRIPTION, a line of text for the
Content-description header.  See also \\[mh-edit-mhn]."
  (interactive (let* ((filename (read-file-name "Insert contents of: "))
                      (type (assoc (file-name-extension filename) 
                                   mh-mime-suffixes)))
		 (list
		  filename
		  (if type (cdr type)
                      (completing-read "Content-type: "
                                       mh-mime-content-types nil nil nil))
		  (read-string "Content-description: ")
		  (read-string "Content-Attributes: " 
			       (concat "name=\""
				       (file-name-nondirectory filename)
				       "\"")))))
  (mh-mhn-compose-type filename type description attributes ))


;; When we save messages, we generally want the MIME info decoded first! (KAF)
(defun mh-write-msg-to-file (msg file no-headers)
  "Append MESSAGE to the end of a FILE.
If NO-HEADERS (prefix argument) is provided, write only the message body.
Otherwise send the entire message including the headers."
  (interactive
   (list (mh-get-msg-num t)
	 (let ((default-dir (if (eq 'write (car mh-last-destination))
				(file-name-directory (car (cdr mh-last-destination)))
			      default-directory)))
	   (read-file-name (format "Save message%s in file: "
				   (if current-prefix-arg " body" ""))
			   default-dir
			   (if (eq 'write (car mh-last-destination))
			       (car (cdr mh-last-destination))
			     (expand-file-name "mail.out" default-dir))))
	 current-prefix-arg))
  (let ((output-file (mh-expand-file-name file)))
    (setq mh-last-destination (list 'write file (if no-headers 'no-headers)))
    (save-window-excursion
      (mh-show-msg msg)
      (set-buffer mh-show-buffer)
      (goto-char (point-min))
      (if no-headers (search-forward "\n\n"))
      (append-to-file (point) (point-max) output-file)))
)



;;;
;;; Browse-URL - handy! (PRB)
;;;
(autoload 'browse-url-at-mouse "browse-url.el" t t)
(defvar url-regexp "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-\\wa-zA-Z0-9_=!?#$@~`%&*+|\\/.,]*[-\\wa-zA-Z0-9_=#$@~`%&*+|\\/]"
  "*Regular expression that matches URLs.")
(defun highlight-urls () 
  (interactive)
  (message "Highlighting URLs")
  (save-excursion 
    (goto-char (point-min))
    (while (re-search-forward url-regexp nil t)
      (add-text-properties (match-beginning 0) (match-end 0) 
			   (list 'mouse-face 'highlight)))
    (set-buffer-modified-p nil))
)
(add-hook 'mh-show-mode-hook 
	  '(lambda () 
	     (highlight-urls)
	     (local-unset-key [mouse-2])
	     (local-set-key [mouse-2] 'browse-url-at-mouse)))



(provide 'my-rmail)


;;;
;;; Mail abbrevs (KAF)
;;;
(autoload 'mail-abbrevs-setup "mailabbrev" nil t)
(add-hook 'mh-letter-mode-hook 'mail-abbrevs-setup)
(add-hook 'mh-show-mode-hook   'mail-abbrevs-setup)
(load-file "~/.mail_abbrevs")
;; Functions to automate creation of new aliases used to be here, until I
;; deleted it accidentally :-(  Not worth redoing I suspect...

;;; MY-RMAIL.EL ends here
