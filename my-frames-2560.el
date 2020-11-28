
; emacs*font: 6x13
; emacs*geometry: 80x68+20+0
; emacs*toolBar: 0

(defvar kaf::left-frm  nil "Left hand frame")
(defvar kaf::right-frm nil "Right hand frame")
(defvar kaf::third-frm nil "Third frame")
(defvar kaf::fourth-frm nil "Fourth frame")

(let* ((kaf::frame-height 109)
       (kaf::left-gap 210)
       (kaf::top-gap 0)
       (kaf::left-frm-props
	(list (cons 'top kaf::top-gap)
	      (cons 'left kaf::left-gap)
	      (cons 'width 80)
	      (cons 'height kaf::frame-height)
	      (cons 'user-position 't)))
       (kaf::right-frm-props
        (list (cons 'width 80)
              (cons 'height kaf::frame-height)
              (cons 'user-position 't)))
       (kaf::third-frm-props
	(list (cons 'width 80)
	      (cons 'height kaf::frame-height)
	      (cons 'user-position 't)))
       (kaf::fourth-frm-props
	(list (cons 'width 80)
	      (cons 'height kaf::frame-height)
	      (cons 'user-position 't)))
       )
  (setq initial-frame-alist    kaf::left-frm-props)
  (setq kaf::right-frm (make-frame kaf::right-frm-props))
  (setq kaf::third-frm (make-frame kaf::third-frm-props))
  (setq kaf::fourth-frm (make-frame kaf::fourth-frm-props))
  (set-frame-position kaf::right-frm 720 kaf::top-gap)
  (set-frame-position kaf::third-frm 1230 kaf::top-gap)
  (set-frame-position kaf::fourth-frm 1740 kaf::top-gap)
)
