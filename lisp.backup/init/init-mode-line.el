;; ========================================================
;; 在状态栏显示行号、列号、日期和时间信息
(display-time-mode 1)
(setq display-time-24hr-format t)  ;;使用24小时制
(setq display-time-day-and-date t) ;;显示时间和日期
(setq display-time-interval 10)    ;;时间变化频率
;; (format-time-string "%m/%d %A %H:%M")
;;时间栏旁边启用邮件设置
(setq display-time-use-mail-icon t)

(setq column-number-mode t)
(setq default-indicate-empty-lines t)

(setq display-time-string-forms
      '(
		(if (and (not display-time-format) display-time-day-and-date)
			(concat
             "["
             (let (month)
               (setq month (format-time-string "%m"))
               (if (string= (substring month 0 1) "0")
                   (substring month 1 2)
                 month))
             (format-time-string "/%-d %a] " now))
          "")
        (format-time-string
         (or display-time-format
             (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
         now)
        load
        (if mail
            ;; Build the string every time to act on customization.
            (concat " "
                    (propertize
                     "Mail"
                     'display `(when (and display-time-use-mail-icon
                                          (display-graphic-p))
                                 ,@display-time-mail-icon
                                 ,@(list :background (face-attribute
                                                      display-time-mail-face
                                                      :background)))
                     'help-echo "mouse-2: Read mail"
                     'local-map (make-mode-line-mouse-map 'mouse-2
                                                          read-mail-command)))
          "")))

(delete 'win:mode-string global-mode-string) ;在 `global-mode-string' 中去掉窗口数字

;;; ### Scroll-mode-line ###
;;; http://www.emacswiki.org/emacs/scroll-mode-line-mode.el
;;; --- 滚动 Mode-line 的信息
;; (require 'scroll-mode-line-mode)
;; (scroll-mode-line-mode 1)

;;; --- 在 Mode-line 显示当前Buffer的大小
(size-indication-mode 0)
