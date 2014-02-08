
(defvar kaf::left-frm  nil "Left hand frame")
(defvar kaf::right-frm nil "Right hand frame")
(defvar kaf::third-frm nil "Third frame")

(let* ((kaf::frame-height 85)
       (kaf::left-gap 150)
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
       )
  (setq initial-frame-alist    kaf::left-frm-props)
  (setq kaf::right-frm (make-frame kaf::right-frm-props))
  (setq kaf::third-frm (make-frame kaf::third-frm-props))
  (set-frame-position kaf::right-frm 650 kaf::top-gap)
  (set-frame-position kaf::third-frm 1150 kaf::top-gap)
)
