;; ==================================================
;; Filename: init-edit.el
;; Emacs initialization file, included in init.el
;; Cui Yidong
;; Rev 20150426
;; ==================================================


;; ===================================================================
;;  auto-fill-mode: 当输入的文本过宽时，会自动换行
;;  Automatically turn on auto-fill-mode when editing text files
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; If you want to change the word wrap column, change this number
(setq-default fill-column 80)

;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插
;;入两个空格
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;++以下内容与搜索替换有关++++++++++++++++++++++++++++++++++++++++++++

;; ===================================================================
;; How do I control Emacs's case-sensitivity when searching/replacing?
;; For searching, the value of the variable case-fold-search determines
;;    whether they are case sensitive:
;  (setq case-fold-search nil) ; make searches case sensitive
(setq case-fold-search t)   ; make searches case insensitive


;;++以下内容与快捷键有关++++++++++++++++++++++++++++++++++++++++++++++

;; C-Space被系统输入法使用
(global-unset-key (kbd "C-SPC")) 

;; 将shift-space改为Mark Set
;; (global-set-key (kbd "S-SPC") 'set-mark-command)
;; 在win8 搜狗输入法之下，S-SPC是全角半角切换
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; 将capslock键改为ctrl键
(define-key function-key-map [(capslock)] 'event-apply-control-modifier)



;;跳到某一行 系统默认是 \M-gg 或者\M-g\M-g
;; (global-set-key "\M-gg" 'goto-line) ; 默认就是\M-gg，无需设置
;;跳到某一列
(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))
(global-set-key "\M-g\M-g" 'go-to-column)

;; 全选当前文本
; (global-set-key "\C-a" 'mark-whole-buffer)

;;=========== 快速跳转到某个字符 =====================================
;;按 C-c f x (x 是任意一个字符) 时，光标就会到下一个 x 处。再次按 x，
;;光标就到下一个 x。比如 C-c f w w w w ..., C-c f b b b b b b ...
;;这个方式类似 vi 的 "f" 。
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
;; (define-key global-map (kbd "C-c f") 'wy-go-to-char)
(global-set-key "\C-cf" 'wy-go-to-char)


;; F1键改为全局帮助键，查找光标所在的单字
;; (global-set-key [f1] (lambda () (interactive) (manual-entry (current-word))))
  
;; ===================================================================
;; 括号匹配：Emacs 在匹配的括号间跳转时按 C-M-f 和 C-M-b。
;; vi使用 % 很方便。当 % 在括号上按下时，匹配括号，否则输入 % 
(global-set-key "%" 'match-paren)
          
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
;; end [] match


;; ===================================================================
;; 输入左括号时，自动输入右括号
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers
;; python 存在'''模式，做特殊处理
(add-hook 'python-mode-hook
           '(lambda ()
               (setq autopair-handle-action-fns
                     (list 'autopair-default-handle-action
                           'autopair-python-triple-quote-action))))



;;++以下和自动文本有关++++++++++++++++++++++++++++++++++++++++++++++++

;; ==================================================
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
(setq time-stamp-start "最后更新时间:[     ]+\\\\?")
(setq time-stamp-end: "\n")
;; (setq time-stamp-format: "%:y年%:m月%:d日")
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)


;; ===================================================================
;;设置tab为4个空格的宽度，而不是原来的2 
(setq default-tab-width 4) 


;; ===================================================================
;; 启用文本补全
(if (eq system-type 'windows-nt)
	(setq ispell-program-name (expand-file-name "bin/aspell.exe" cygwin-root-path)))

(if (eq system-type 'cygwin)
	(setq ispell-program-name "/usr/bin/aspell.exe"))

(if (eq system-type 'windows-nt)
	(global-set-key "\M-/" 'ispell-complete-word)
)
