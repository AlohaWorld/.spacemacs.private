;;
;; <my-commons> layer config file
;; main configuration: miscellaneous configuration
;; Cui Yidong
;; Rev: 20160730
;;

;; In order to load el files in current directory, we need add current dir to
;; load-path
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;; Setup personal information
(setq user-full-name "Cui Yidong")
(setq user-mail-address "cyd@bupt.edu.cn")

;; -----------------------------------------------------------------------------
;; Calendar & Diary
;; Setup altitude and logitude. Then we can get sunrise/sunset time in calendar
(setq calendar-latitude 39.55)          ;; Altitude, positive number ==> north
(setq calendar-longitude 116.25)        ;; Longitude, positive number ==> east
(setq calendar-location-name "Beijing") ;; City name
;; Diary entries using non-gregorian calendars
(add-hook 'diary-nongregorian-listing-hook 'diary-chinese-list-entries)
(add-hook 'diary-nongregorian-marking-hook 'diary-chinese-mark-entries)

;; cyd@20190902
(setq calendar-week-start-day 1)        ; Make Monday the first day of a week

;; GnuEmacs 23.1 introduced the variable calendar-intermonth-text that can be
;; used for displaying ISO week numbers in the Calendar window
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 1.0)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

;; add a header for the week numbers:
(copy-face 'default 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
                    :height 1.0)
(setq calendar-intermonth-header
      (propertize "Wk"                  ; or e.g. "KW" in Germany
                  'font-lock-face 'calendar-iso-week-header-face))

;; using a different color for the week number:
(set-face-attribute 'calendar-iso-week-face nil
                    :height 1.0 :foreground "salmon")


;; Setup Coding page
;; To solve "Some unexpected code \241\256 in Interactive-Haskell mode"
;; https://github.com/haskell/haskell-mode/issues/400
(setq default-process-coding-system '(cp936-dos . utf-8-unix))

;; Setup the general paths
(load "config-path.el")



;; Setup the shell (cygwin zsh)
(load "config-shell.el")


;; Configuration about editing/viewing environment
;; cyd@20160739
(load "config-editview.el")
