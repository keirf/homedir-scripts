;;; trace-mode.el: A trace-monitoring mode for Emacs

;; Copyright (C) 1999, K A Fraser


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPATIBILITY WITH OLD EMACS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and (boundp 'emacs-major-version) (= emacs-major-version 20))
    (defsubst tm-win-end (win) (window-end win t))
  (defmacro when (cond &rest body) 
    (list 'if cond (cons 'progn body)))
  (defmacro with-current-buffer (buffer &rest body)
    (` (save-excursion (set-buffer (, buffer)) (,@ body))))
  (defsubst caar (x) (car (car x)))
  (defsubst cadr (x) (car (cdr x)))
  (defsubst cdar (x) (cdr (car x)))
  (defsubst cddr (x) (cdr (cdr x)))
  (defsubst tm-win-end (win) (window-end win)))


;;;;;;;;;;
;;; KEYMAP
;;;;;;;;;;

(defvar trace-mode-map (make-sparse-keymap)
  "Keymap used in Trace mode.")
(define-key trace-mode-map "h"    'trace-mode-hide-lines-int)
(define-key trace-mode-map "s"    'trace-mode-show-lines-int)
(define-key trace-mode-map "a"    'trace-mode-all-visible) ;; XXX doesn't work!
(define-key trace-mode-map "c"    'trace-mode-colour-lines-int)
(define-key trace-mode-map "u"    'trace-mode-uncolour-lines-int)
(define-key trace-mode-map "\C-s" 'trace-mode-resize-window)
(define-key trace-mode-map "\C-r" 'trace-mode-reset-window)


;;;;;;;;;;;;;;;;
;;; SYNTAX TABLE
;;;;;;;;;;;;;;;;

(defvar trace-mode-syntax-table nil
  "Syntax table in use in trace-mode buffers.")
(unless trace-mode-syntax-table
  (setq trace-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" trace-mode-syntax-table)
  (modify-syntax-entry ?- "w" trace-mode-syntax-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLES CUSTOMISABLE BY USER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Standard list of callback hooks. Run when trace-mode is started.
(defvar trace-mode-hook        nil)

;; The new frame width, in characters, to be set by `trace-mode-resize-window'
(defvar trace-mode-frame-width nil)

;; The character width to reset frames to with `trace-mode-reset-window'
(defvar trace-mode-reset-width nil)

;; Remaining customisable variables are buffer-local. This allows multiple
;; trace types to be used at the same time, in separate buffers.
(defun trace-mode-create-vars ()
  (make-local-variable 'trace-mode-format-list)
  (setq trace-mode-format-list (list  8 nil "time"
                                     16   t "file"
                                     16   t "function"
                                      4 nil "line"
                                      3 nil "level"))

  ;; The set of colours we can highlight with.
  (make-local-variable 'trace-mode-colour-list)
  (setq trace-mode-colour-list (list "yellow"
                                     "purple"
                                     "Goldenrod"
                                     "cyan"
                                     "orange"
                                     "green")))

(defun trace-mode-create-symbol-tables ()
  ;; we add all file and function names to a symbol table, which provides
  ;; a unique symbol for each distinct name we add to it.
  (make-local-variable 'sym-table)
  (setq sym-table (make-vector 20 0))

  (let ((format-list trace-mode-format-list))
    (while format-list
      (setq format-list (cdr format-list))
      (when (car format-list)
        (let ((new-table (intern (cadr format-list) sym-table)))
          (make-local-variable new-table)
          (set new-table (make-vector 20 0))))
      (setq format-list (cddr format-list))))

  ;; we also have tables for minibuffer completion of selected subsets of
  ;; the above.
  (make-local-variable 'highlight-table)
  (make-local-variable 'invisible-table)
  (setq highlight-table (make-vector 20 0))
  (setq invisible-table (make-vector 20 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *** MAIN MODE FUNCTION ***
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trace-mode ()
  "Major mode for monitoring trace (.trc) files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map trace-mode-map)
  (setq major-mode 'trace-mode)
  (setq mode-name "Trace")
  (set-syntax-table trace-mode-syntax-table)
  (setq require-final-newline t)

  (trace-mode-create-vars)
  (trace-mode-all-visible)
  (run-hooks 'trace-mode-hook)
  (trace-mode-create-symbol-tables)
  
  ;; scan existing windows
  (let ((windows (get-buffer-window-list (current-buffer) 'nomini t)))
    (while windows
      (scan-trace-buffer-conservatively (car windows))
      (setq windows (cdr windows))))

  ;; hook function to detect when window configs change
  (when (boundp 'window-configuration-change-hook)
    (make-local-variable 'trace-in-frame)    
    (add-hook 'window-configuration-change-hook 'check-resize-windows))

  ;; install hook functions to do the lazy scanning
  (make-local-hook 'window-scroll-functions)
  (add-hook 'window-scroll-functions 'scan-after-scroll nil t)
  (add-hook 'window-size-change-functions 'scan-after-resize)
  (add-hook 'redisplay-end-trigger-functions 'scan-after-redisplay nil t))


;;;;;;;;;;;;;;;;;;
;;; HOOK FUNCTIONS
;;;;;;;;;;;;;;;;;;

(defun scan-after-scroll (window window-start)
  ;; Called from `window-scroll-functions'.
  ;; Scan WINDOW from WINDOW-START following the scroll.
  ;; Will only ever be called for our own buffers, as we added as a local hook.
  (scan-trace-buffer window-start (tm-win-end window)))

(defun scan-after-resize (frame)
  ;; Called from `window-size-change-functions'.
  ;; Scan windows in FRAME conservatively, since window bounds unusable.
  ;; We must also check that the buffer is ours! (Hook added globally)
  (save-excursion
    (save-selected-window
      (select-frame frame)
      (walk-windows (function (lambda (window)
                                (set-buffer (window-buffer window))
                                (if (boundp 'sym-table)
                                    (scan-trace-buffer-conservatively window))))
                    'nomini frame))))

(defun scan-after-redisplay (window trigger-point)
  ;; Called from `redisplay-end-trigger-functions'.
  ;; Scan WINDOW from TRIGGER-POINT to WINDOW_END.
  ;; We must also check that the buffer is ours! (Hook added globally)
  (when (boundp 'sym-table)
    (scan-trace-buffer trigger-point (tm-win-end window))))

;;;;;;;;;;;;;;;;;;
;;; SCAN FUNCTIONS
;;;;;;;;;;;;;;;;;;

(defun scan-trace-buffer-conservatively (window)
  ;; Scan in WINDOW conservatively around point.
  ;; Doing `window-height' lines above and below point guarantees enough done.
  (with-current-buffer (window-buffer window)
    (scan-trace-buffer 
     (save-excursion
       (goto-char (window-point window))
       (vertical-motion (- (window-height window)) window) (point))
     (save-excursion 
       (goto-char (window-point window))
       (vertical-motion (window-height window) window) (point)))))

(defun n-trace-buffer (s e))
(defun scan-trace-buffer (start-orig end-orig)
  (let ((start (make-marker)) (end (make-marker)) (le (make-marker)))
    ;; Scan between START and END, where necessary, in the current buffer.
    (set-marker start (text-property-any start-orig end-orig 'invisible nil))
    (set-marker end end-orig)
    (save-excursion
      (while (marker-position start)
        (goto-char (marker-position start))
        (beginning-of-line)
        (set-marker start (point))

        (let ((format-list trace-mode-format-list) (pos 0) sym-val)
          (while format-list
            ;; For each item in the format list, do what is necessary...
            (set-marker le (save-excursion (forward-line) (point)))
            (when (cadr format-list)
              ;; This one needs processing - get the symbol via symbol table.
              (let ((wrd (buffer-substring-no-properties 
                          (point) 
                          (save-excursion
                            (re-search-forward "\\w+" (marker-position le) t)
                            (point)))))
              (setq sym-val (eval (intern (car (cddr format-list)) sym-table)))
              (setq sym-val (intern wrd sym-val)))
              ;; Face and property stuff.
              (make-face sym-val)
              (add-text-properties (marker-position start) (marker-position le)
                                   (list 'face sym-val))
              (add-text-properties (marker-position start) (marker-position le)
                                   (list 'invisible sym-val)))
            ;; After each field, we search ahead for the next and align it.
            (re-search-forward "[ \t]+" (marker-position le) t)
            (indent-to (setq pos (+ 1 pos (car format-list))))
            (move-to-column pos) 
            (setq format-list (nthcdr 3 format-list))))
        
          (set-marker start (text-property-any 
                             (marker-position le) 
                             (marker-position end) 'invisible nil)))))
  (set-buffer-modified-p nil))


;;;;;;;;;;;;;;;;;;;;;;;
;;; HIGHLIGHT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;

(defun select-column ()
  (eval (intern-soft (completing-read "Column to select from: " sym-table nil t)
                     sym-table)))

(defun trace-mode-colour-lines-int ()
  "Highlight a function or file."
  (interactive)
  (let ((table (select-column)))
    (trace-mode-colour-lines 
     (completing-read "Column item to highlight: " table nil t) table)))
  
(defun trace-mode-uncolour-lines-int ()
  "Unhighlight a function or file."
  (interactive)
  (let ((table (select-column)))
    (trace-mode-uncolour-lines 
     (completing-read "Column item to un-highlight: " 
                      highlight-table 
                      '(lambda (sym) (intern-soft (symbol-name sym) table)) 
                      t) 
     table)))

(defun trace-mode-colour-lines (to-colour table)
  (if trace-mode-colour-list
      (when (intern-soft to-colour table)
        (if (not (intern-soft to-colour highlight-table))
            (progn
              (intern to-colour highlight-table)
              (set-face-foreground (intern to-colour table) 
                                   (car trace-mode-colour-list))
              (setq trace-mode-colour-list (cdr trace-mode-colour-list)))
          (error "Already highlighted!")))
    (error "No more colours to highlight with!")))

(defun trace-mode-uncolour-lines (to-uncolour table)
  (when (intern-soft to-uncolour highlight-table)
    (setq trace-mode-colour-list 
          (cons (face-foreground (intern to-uncolour table)) 
                trace-mode-colour-list))
    (copy-face 'default (intern to-uncolour table))
    (unintern to-uncolour highlight-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INVISIBILTY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trace-mode-show-lines-int ()
  "Make a function or file visible."
  (interactive)
  (let ((table (select-column)))
    (trace-mode-show-lines 
     (completing-read "Column item to show: " 
                      invisible-table 
                      '(lambda (sym) (intern-soft (symbol-name sym) table))
                      t)
     table)))

(defun trace-mode-hide-lines-int ()
  "Make a function or file invisible."
  (interactive)
  (let ((table (select-column)))
    (trace-mode-hide-lines 
     (completing-read "Column item to hide: " table nil t) table)))

(defun trace-mode-all-visible ()
  "Make all trace lines visible"
  (interactive)
  (setq buffer-invisibility-spec nil))

(defun trace-mode-show-lines (to-add table)
  (when (intern-soft to-add invisible-table)
    (remove-from-invisibility-spec (intern to-add table))
    (unintern to-add invisible-table)))

(defun trace-mode-hide-lines (to-remove table)
  (when (intern-soft to-remove table)
    (if (not (intern-soft to-remove invisible-table))
        (progn
          (add-to-invisibility-spec (intern to-remove table))
          (intern to-remove invisible-table))
      (error "Already hidden!"))))


;;;;;;;;;;;;;;;;;;;
;;; WINDOW RESIZING
;;;;;;;;;;;;;;;;;;;

(defun trace-mode-resize-window ()
  "Resize all trace frames to TRACE-MODE-FRAME-WIDTH chars wide."
  (interactive)
  (trace-mode-set-widths trace-mode-frame-width))

(defun trace-mode-reset-window ()
  "Reset all trace frames to the default width."
  (interactive)
  (trace-mode-set-widths trace-mode-reset-width))
  
(defun trace-mode-set-widths (width)
  ;; Set to WIDTH characters every frame we are currently monitoring trace in.
  (when width
    (let ((windows (get-buffer-window-list (current-buffer) 'nomini t)))
      (while windows
        (let ((wf (window-frame (car windows))))
          (set-frame-size wf width (frame-height wf)))
        (setq windows (cdr windows))))))

(defun check-resize-windows ()
  ;; Called by `window-configuration-change-hook' whenever a window is
  ;; created, deleted, split, and so on. Called with the affected frame 
  ;; selected. We simply walk all the windows in that frame, and see if any
  ;; belong to us. If so, we keep the frame wide, otherwise we reset it.
  (setq trace-in-frame nil)
  (save-excursion
    (walk-windows (function (lambda (window)
                              (set-buffer (window-buffer window))
                              (if (boundp 'sym-table) (setq trace-in-frame t))))
                  'nomini (selected-frame)))
  (if trace-in-frame (trace-mode-resize-window) (trace-mode-reset-window)))


(provide 'trace-mode)

;;; trace-mode.el ends here
