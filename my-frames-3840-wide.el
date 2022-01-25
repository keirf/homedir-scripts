
; emacs*font: neep-alt-18
; emacs*toolBar: 0

(defvar kaf::left-frm  nil "Left hand frame")
(defvar kaf::right-frm nil "Right hand frame")

(let* ((kaf::frame-height 107)
       (kaf::left-gap 80)
       (kaf::top-gap 0)
       (kaf::left-frm-props
	(list (cons 'top kaf::top-gap)
	      (cons 'left kaf::left-gap)
	      (cons 'width 180)
	      (cons 'height kaf::frame-height)
	      (cons 'user-position 't)))
       (kaf::right-frm-props
        (list (cons 'width 180)
              (cons 'height kaf::frame-height)
              (cons 'user-position 't)))
       )
  (setq initial-frame-alist    kaf::left-frm-props)
  (setq kaf::right-frm (make-frame kaf::right-frm-props))
  (set-frame-position kaf::right-frm 1910 kaf::top-gap)
)
