
(defvar kaf::left-frm  nil "Left hand frame")
(defvar kaf::right-frm nil "Right hand frame")

(let* ((kaf::frame-height 57)
       (kaf::left-gap 70)
       (kaf::top-gap 0)
       (kaf::minibuffer-props
        (list (cons 'top 738)
              (cons 'left kaf::left-gap)
              (cons 'width 165)
              (cons 'height 1)
              (cons 'user-position 't)))
       (kaf::left-frm-props
	(list (cons 'top kaf::top-gap)
	      (cons 'left kaf::left-gap)
	      (cons 'width 80)
	      (cons 'height kaf::frame-height)
;	      (cons 'minibuffer nil)
	      (cons 'user-position 't)))
       (kaf::right-frm-props
        (list (cons 'width 80)
              (cons 'height kaf::frame-height)
;              (cons 'minibuffer nil)
              (cons 'user-position 't)))
       )
  (setq initial-frame-alist    kaf::left-frm-props)
  (setq minibuffer-frame-alist kaf::minibuffer-props)
  (setq kaf::right-frm (make-frame kaf::right-frm-props))
  (set-frame-position kaf::right-frm 580 kaf::top-gap)
)