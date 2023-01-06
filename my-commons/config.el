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
(setq user-full-name "Nathan Cui")
(setq user-mail-address "cyd@bupt.edu.cn")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; The variable calendar-intermonth-text can be used to display ISO week numbers
;; in the Calendar window
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
      (propertize "Wk"
                  'font-lock-face 'calendar-iso-week-header-face))

;; using a different color for the week number:
(set-face-attribute 'calendar-iso-week-face nil
                    :height 1.0 :foreground "salmon")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Coding page
;; To solve "Some unexpected code \241\256 in Interactive-Haskell mode"
;; https://github.com/haskell/haskell-mode/issues/400
(setq default-process-coding-system '(cp936-dos . utf-8-unix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup the shell (msys zsh)
;; !!!!!!!! BE AWARE !!!!!!!!
;; When using lsp-haskell, hie-wrapper is invoked with the following shell
;; One must check if hie-wrapper is running smoothly under emacs shell
;;    TO TEST
;; M-x shell
;; hie-wrapper --lsp -d -l C:/temp/hie.log
;;    Then see c:/temp/hie.log

;; !!!!!!!! BE AWARE !!!!!!!!
;; When updating OS path, delete ~/.spacemacs.env
;; OR you will not see the path in emacs being updated!!!

;; (when (eq system-type 'windows-nt)
;;   (if (file-exists-p "C:/msys64/usr/bin/zsh.exe")
;;     (progn
;;       (setq explicit-shell-file-name "C:/msys64/usr/bin/zsh.exe")
;;       (setq shell-file-name "zsh")
;;       ;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
;;       ;; (setq explicit-zsh.exe-args '("--emacs" "--login"))
;;       (setenv "SHELL" shell-file-name)
;;       ;; remove the input Ctrl-M character
;;       (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
;;       (add-to-list 'process-coding-system-alist
;;         '("zsh" . (undecided-dos . undecided-unix)))
;;     )
;;     (message "Can not find C:/msys64/usr/bin/zsh.exe. Keep the shell as-is(cmd.exe)")
;;   )
;; )

;; (when (eq system-type 'windows-nt)
;;   (if (file-exists-p "C:/Windows/System32/cmd.exe")
;;     (progn
;;       (setq explicit-shell-file-name "C:/Windows/System32/cmd.exe")
;;       (setq shell-file-name "cmd")
;;       ;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
;;       ;; (setq explicit-zsh.exe-args '("--emacs" "--login"))
;;       (setenv "SHELL" shell-file-name)
;;       ;; remove the input Ctrl-M character
;;       (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
;;       (add-to-list 'process-coding-system-alist
;;         '("cmd" . (undecided-dos . undecided-unix)))
;;     )
;;     (message "Can not find C:/Windows/System32/cmd.exe")
;;   )
;; )


(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(defun powershell (&optional buffer)
  "Launches a powershell in buffer *powershell* and switches to it."
  (interactive)
  (let ((buffer (or buffer "*powershell*"))
        (powershell-prog "c:\\Program Files\\PowerShell\\7\\pwsh.exe"))
    (make-comint-in-buffer "shell" "*powershell*" powershell-prog)
    (switch-to-buffer buffer)))

(defun zsh (&optional buffer)
  "Launches a msys2/zsh in buffer *zsh* and switches to it."
  (interactive)
  (let ((buffer (or buffer "*zsh*"))
        (zsh-prog "c:\\msys64\\usr\\bin\\zsh.exe")
       )
    (make-comint-in-buffer "shell" "*zsh*" zsh-prog)
    (switch-to-buffer buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration about editing/viewing environment

;; Setup Emacs title text
(setq frame-title-format
      `("%b  " (:eval (if (buffer-modified-p) "※")) " → "
        (dired-directory dired-directory " %f → ")
        (:eval user-login-name) "@" (:eval system-name)  ))

;; Recognize chinese punctuation
;; We do not need to insert two space characters after a period punctuation
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; How do I control Emacs's case-sensitivity when searching/replacing?
;; For searching, the value of the variable case-fold-search determines
;; whether they are case sensitive. non-nil to make searches case insensitive
(setq case-fold-search t)


;; 打开time-stamp可以记录最后运行time-stamp的时间
;; 缺省的情况下, 在所编辑文件的前八行内插入如下标记
;;			Time-stamp: <>   或者
;;			Time-stamp: " "
;; Emacs在保存时就会运行write-file-hooks中的time-stamp, 从而加 入修改时间,
;; 结果类似下面所示
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

;; Set tab width to 4 spaces, replacing the original 2 spaces
(setq default-tab-width 4)

;; Suppress "Yes/No" confirmation messages
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;; Keep cursor at the end of a line when moving up and down
(setq track-eol t)

;; Set default mode to text-mode ,not fundamental
(setq default-major-mode 'text-mode)
;; auto-fill-mode: When the length of a line exceeds a limit, the text will
;; auto wrap
;; We don't use auto-fill 'cause it will introduce extra return carriage 
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; The word wrap column
(setq-default fill-column 80)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 查找和替换
;; query-replace-regexp is bound by default to C-M-%, although
;;    some people prefer using an alias like M-x qrr. Put the
;;    following in your InitFile to create such alias.
(defalias 'qrr 'query-replace-regexp)

;;
;; Tools to reinstall packages
(defun reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))
