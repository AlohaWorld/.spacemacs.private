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

;; In windows system, Ctrl+Space is usually bound to input method.
;; In emacs, Ctrl+Space is bound to set-mark-command
;; To avoid conflicts, we need to change key sequence of C-SPC
(global-unset-key (kbd "C-SPC")) 
;; We then have two options:
;; 1. If you do NOT use "Sogou input method", then Rebind "Mark Set" command to
;;    shift-space
;;   (global-set-key (kbd "S-SPC") 'set-mark-command)
;; 2. Rebind to Alt+Space to avoid conflict with Sogou (Win8/Win10)
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; Rebind CapsLock key to Ctrl key
(define-key function-key-map [(capslock)] 'event-apply-control-modifier)


;; Rebind line-number key. Show line number with \C-cn
(global-set-key "\C-cn" 'spacemacs/toggle-line-numbers)

;;跳到某一行 系统默认是 \M-gg 或者\M-g\M-g
;; (global-set-key "\M-gg" 'goto-line) ; 默认就是\M-gg，无需设置
;;跳到某一列
(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))
(global-set-key "\M-g\M-g" 'go-to-column)

;; Jump to a specific character
;; Press C-c f x (x is an arbitrary character) and the cursor will go to the next
;; "x". Press "x" again, the cursor will go to the next "x"
;; e.g. C-c f w w w w ..., C-c f b b b b b b ...
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
					 char)
	(search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(global-set-key "\C-cf" 'wy-go-to-char)


;; Rebind F1 as the global help key and search the word at cursor.
(global-set-key [f1] (lambda () (interactive) (manual-entry (current-word))))

;;
;; Paren match:  C-M-f 和 C-M-b。
;; vi使用 % 很方便。当 % 在括号上按下时，匹配括号，否则输入 % 
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
;; end [] match


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
;; 启用文本补全
;; (if (eq system-type 'windows-nt)
;; 	(setq ispell-program-name (expand-file-name "bin/aspell.exe" cygwin-root-path)))

;; (if (eq system-type 'cygwin)
;; 	(setq ispell-program-name "/usr/bin/aspell.exe"))

;; (if (eq system-type 'windows-nt)
;; 	(global-set-key "\M-/" 'ispell-complete-word)
;; )

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

  ;; Kill current buffer without reconfirmation
  (global-set-key "\C-xk" 'kill-current-buffer)
  (defun kill-current-buffer ()
    "Kill the current buffer, without confirmation."
    (interactive)
    (kill-buffer (current-buffer)))


  ;; To switch window, use Ctrl-o instead of Ctrl-x o
  (global-set-key [(control o)] 'other-window)


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

