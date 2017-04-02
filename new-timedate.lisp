
(defmvar $time_zone_offset nil
  "The offset of the local time zone from GMT. This offset does
not account for daylight savings.")

(defun $timedate (&optional (tz-offset $time_zone_offset))
  (multiple-value-bind
    (second minute hour date month year day-of-week dst-p tz)
    (if tz-offset (decode-universal-time (get-universal-time) tz-offset)
      (decode-universal-time (get-universal-time)))
    (declare (ignore day-of-week))
    (let
      ((tz-offset (if dst-p (- 1 tz) (- tz))))
      (multiple-value-bind
        (tz-hours tz-hour-fraction)
        (floor tz-offset)
        (let
          ((tz-sign (if (< 0 tz-hours) #\+ #\-)))
          (format nil "~4,'0d-~2,'0d-~2,'0d
~2,'0d:~2,'0d:~2,'0d~a~2,'0d:~2,'0d"
              year month date hour minute second tz-sign (abs tz-hours)
(floor (* 60 tz-hour-fraction))))))))
