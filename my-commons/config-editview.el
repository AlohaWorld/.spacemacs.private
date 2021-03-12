;;
;; <my-commons> layer config file
;; Edit/View configuration: setting up the editing and viewing environment
;; Cui Yidong <nathan.cui@gmail.com>
;; Rev 20160426,20160730
;;

;; Setup Emacs title text
(setq frame-title-format
      `("%b  " (:eval (if (buffer-modified-p) "※")) " → "
        (dired-directory dired-directory " %f → ")
        (:eval user-login-name) "@" (:eval system-name)  ))

;; auto-fill-mode: When the length of a line exceeds a limit, the text will
;; auto wrap
;; Automatically turn on auto-fill-mode when editing text files
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; If you want to change the word wrap column, change this number
(setq-default fill-column 80)

;; Recognize chinese punctuation
;; We do not need to insert two space characters after a period punctuation
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; How do I control Emacs's case-sensitivity when searching/replacing?
;; For searching, the value of the variable case-fold-search determines
;; whether they are case sensitive:
;; (setq case-fold-search nil) ; make searches case sensitive
(setq case-fold-search t)   ; make searches case insensitive

;; 输入左括号时，自动输入右括号
;; python 存在'''模式，做特殊处理
(add-hook 'python-mode-hook
           '(lambda ()
               (setq autopair-handle-action-fns
                     (list 'autopair-default-handle-action
                           'autopair-python-triple-quote-action))))

;;
;; 打开time-stamp可以记录最后运行time-stamp的时间
;;		缺省的情况下, 在所编辑文件的前八行内插入如下标记
;;			Time-stamp: <>   或者
;;			Time-stamp: " "
;;		Emacs在保存时就会运行write-file-hooks中的time-stamp, 从而加 入修改时间,
;;		结果类似下面所示
;;			Time-stamp: <jerry 12/17/2003 12:00:54 (unidevel.com)>
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-format "%:u %04y/%02m/%02d %02H:%02M:%02S")

;; 要使用中文表示, 可以如下设置，"最后更新时间:"行后所有字符都将
;; 无条件被替换为"XXXX年XX月XX日" 格式的时间
(setq time-stamp-start "Last Update:[     ]+\\\\?")
(setq time-stamp-end: "\n")
;; (setq time-stamp-format: "%:y年%:m月%:d日")
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)


;; ===================================================================
;; Set tab width to 4 spaces, replacing the original 2 spaces
(setq default-tab-width 4)


;; ===================================================================
;; Settings for SpeedBar
;; Display speedbar in current frame, not in a new one
;; http://www.emacswiki.org/emacs/SpeedBar
;; http://www.emacswiki.org/emacs/SrSpeedbar
;; http://www.emacswiki.org/emacs/sr-speedbar.el
;; (require 'sr-speedbar)
;; (require 'speedbar-extension)
;; (global-set-key (kbd "C-c C-'") 'sr-speedbar-toggle) ;;sr-speedbar按键绑定
;; (setq speedbar-show-unknown-files t) ; Show all dirs and files
;; (setq dframe-update-speed nil)       ; Refresh manually with key "g"
;; (setq speedbar-update-flag nil)
;; (setq speedbar-use-images nil)       ; Do NOT use image
;; (setq speedbar-verbosity-level 0)

;; Suppress "Yes/No" confirmation messages
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;; Remember the position before turn page
;;  (setq scroll-preserve-screen-position t)

;; Keep cursor at the end of a line when moving up and down
(setq track-eol t)

;; Enable the following default-disabled functions
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)

;; Set default mode to text-mode ,not fundamental
(setq default-major-mode 'text-mode)

;; =========================================================
;; 查找和替换
;; query-replace-regexp is bound by default to C-M-%, although
;;    some people prefer using an alias like M-x qrr. Put the
;;    following in your InitFile to create such alias.
(defalias 'qrr 'query-replace-regexp)
