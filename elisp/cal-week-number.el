;;; cal-week-number.el

;;display ISO week numbers in CalendarMode
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil)
(setq calendar-week-start-day 1)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))
(copy-face 'default 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil)
(setq calendar-intermonth-header
      (propertize "WW"
                  'font-lock-face 'calendar-iso-week-header-face))
(set-face-attribute 'calendar-iso-week-face nil
                    :foreground "salmon")


(provide 'cal-week-number)

;;; cal-week-number.el end here
