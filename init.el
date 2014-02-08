;;;;;;;;;;;;;;;;;;;;;;;
;;;; make emacs sane
;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'blink-cursor-mode) (blink-cursor-mode nil))
(setq inhibit-startup-message t)
(setq make-backup-files nil)              ; Don't make annoying backups
(setq auto-save-list-file-prefix nil)     ; \
(setq auto-save-list-file-name nil)       ;  why save?!
(setq auto-save-interval 0)               ; /
(setq nmenonic-buffer-names t)            ;  sensible buffer names
(setq minimum-buffer-name-dir-content 0)  ; /
(setq next-line-add-newlines nil)         ; no blank lines
(line-number-mode 42)                     ; turn line numbers on
(setq vc-follow-symlinks nil)             ; none of this nasty following stuff
(require 'desktop)                        ; Snapshoting of state of all open emacs buffers
(setq-default indent-tabs-mode nil)       ; prevent extraneous tabs
(setq search-highlight t)
(setq visual-bell t)
(setq line-move-visual nil)
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X settings

(cond (window-system
       (set-background-color "black")
       (set-foreground-color "white")
       ;; tasteful highlighting...
       (set-face-background (quote region) "grey32")
       (set-face-foreground (quote region) "white")
       (set-face-background (quote highlight) "grey32")
       (set-face-foreground (quote highlight) "white")
       (set-cursor-color "white")
       (load "my-font-lock")
       (require 'caml-font)
       (cond ((eq (x-display-pixel-width) 1364) (load "my-frames-1364.el"))
             (t (load "my-frames-fvwm.el")))
       ))

(menu-bar-mode -1)
;Toolbar turned off in .Xdefaults
(tool-bar-mode -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;(make-frame)

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-<tab>") 'auto-complete)
(define-key ac-complete-mode-map (kbd "<tab>") 'ac-next)
(define-key ac-complete-mode-map (kbd "M-<tab>") 'ac-next)
(ac-set-trigger-key "TAB")

;;;;;;;;;;;;;;;;;;;;;;;
;;;; the feel of the thing
;;;;;;;;;;;;;;;;;;;;;;;

;;; so that marked region is highlighted...
(setq transient-mark-mode t)

;;; M-x wc word count facility
(defun wc ()
  "Counts the number of words in the region"
  (interactive)
  (shell-command-on-region (point) (mark) "wc")
)

(defun reread-dot-emacs () "Re-read ~/.emacs"
  (interactive) (load-file "~/.emacs"))   ; re-read .emacs configa
(global-set-key [M-C-down-mouse-1] (function imenu))
(global-unset-key [M-C-s])
(global-set-key [M-C-s] (function imenu))

;;; prb's ispell stuff
(make-face 'spelling-mistake)
(set-face-background 'spelling-mistake "darkslategrey")
(setq ispell-highlight-face 'spelling-mistake)

;;; Paren matching
(make-face 'paren-mismatch)
(make-face 'paren-match)
(set-face-background 'paren-match "grey50")
(show-paren-mode 1)
(setq show-paren-face 'paren-match)

;;; Cyclic buffers
(autoload 'cyclebuffer-forward "cyclebuffer" "cycle forward" t)
(autoload 'cyclebuffer-backward "cyclebuffer" "cycle backward" t)
(global-set-key "\M-n" 'cyclebuffer-forward)
(global-set-key "\M-p" 'cyclebuffer-backward)

;;; Window switching: (C-x o) selects next window, (C-x p) selects prev window
(defun real-next-window ()
  "A frame-aware replacement for (other-window 1)"
  (interactive)
  (let ((next (next-window nil nil t)))
    (if (not (eq (selected-frame) (window-frame next))) (other-frame 1))
    (select-window next)))
(defun real-prev-window ()
  "A frame-aware replacement for (other-window -1)"
  (interactive)
  (let ((prev (previous-window nil nil t)))
    (if (not (eq (selected-frame) (window-frame prev))) (other-frame -1))
    (select-window prev)))
(global-set-key "\C-o" '(lambda () (interactive) (real-next-window)))
(global-set-key "\C-p" '(lambda () (interactive) (real-prev-window)))

(global-set-key "\^z" 'nil) ; C-z don't iconify

(fset 'simple-cite
      [(control x) r t > space return])
(fset 'simple-cite-and-fill
      [(control x) r t > space return (meta q)])
(fset 'simple-indent
      [(control x) r t space space return])

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end]  'end-of-buffer)

(global-set-key [f5] 'simple-cite)           ;F5 cite selected region with "> "
(global-set-key [f6] 'simple-cite-and-fill)  ;F6 cite like f5, then fill
(global-set-key [f7] 'simple-indent)         ;F7 indent region 2 spaces

(global-set-key "\C-\\" 'c-backslash-region)

(cond ((not window-system)
       (keyboard-translate ?\C-? ?\C-h)
       (global-set-key "\C-h" 'backward-delete-char)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; different file mode type things
;;;;;;;;;;;;;;;;;;;;;;;

;;; Turn on paragraph filling when going into text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Editing a file ending in .ml puts emacs into ML mode
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(setq auto-mode-alist (cons '("\\.ml$" . sml-mode) auto-mode-alist))

;;; Shell scripts
(autoload 'sh-mode "sh-mode" "Major mode for editing shell scripts" t)
(setq auto-mode-alist (cons '("\\.sh$" . sh-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("sh" . sh-mode) interpreter-mode-alist))
(setq interpreter-mode-alist (cons '("bash" . sh-mode) interpreter-mode-alist))

;; CAML
(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

;;; .S is the same as .s
(setq auto-mode-alist (cons '("\\.S$" . asm-mode) auto-mode-alist))

;;; Autoload the comment minor mode when comment-mode is called.
(autoload 'comment-mode "comment-mode" "Minor mode for sane C commenting." t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C mode shenanigans

(autoload 'c-mode   "cc-mode" "C Editing Mode"   t)
(autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)

(add-hook 'c-mode-common-hook '(lambda () 
 	(c-set-style "BSD")
 	(setq c-basic-offset 4)

        ;; Case sensitive expansion/completion
        (set (make-local-variable 'dabbrev-case-fold-search) nil)
        (set (make-local-variable 'dabbrev-case-replace) nil)

        ;; this starts up comment-mode.
        (comment-mode 1)
        ;; some key bindings may be annoying, but can be changed/removed
        ;; as follows... (see comment-mode.el for more details):
        ;(define-key comment-mode-map <key sequence> nil)
        ;(define-key comment-mode-map <key sequence> '<new-binding>)
        (define-key comment-mode-map "\C-cr"    nil)
        (define-key comment-mode-map "\C-c\C-c" nil)))

(defun switch-c-mode ()
  "Counts the number of words in the region"
  (interactive)
  (if indent-tabs-mode
      (progn
        (c-set-style "BSD")
        (setq indent-tabs-mode nil))
    (c-set-style "Linux")
    (setq indent-tabs-mode t))
)

(global-set-key "\C-cl" 'switch-c-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abbrevs

(global-unset-key [M-tab])
(global-set-key [M-tab] 'dabbrev-expand)
