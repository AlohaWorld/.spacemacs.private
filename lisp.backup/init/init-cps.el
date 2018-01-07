;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 剪切 | 粘贴 | 搜索 | 替换

;; ==================================================
;;emacs cut（C-w, C-k）命令会把你所有cut掉的部分都存起来， 
;; 之后可以用M-y 一个一个的调出来。set 这个值为最多存档两百个这样的命令 
(setq kill-ring-max 50)

;; ==================================================
;; 让拷贝和粘贴变得充满乐趣
;; 这段代码定义了三个 Emacs Lisp 函数。这三个函数提供了可以在 Emacs 编辑器
;; 中执行的三个操作。分别用来拷贝当前光标所在的 行 ，当前光标所在的 单词 以及当前光标所在的 段落 。
(defun copy-line (&optional arg)
 "Save current line into Kill-Ring without mark the line"
 (interactive "P")
 (let ((beg (line-beginning-position)) 
	(end (line-end-position arg)))
 (copy-region-as-kill beg end))
)


(defun copy-word (&optional arg)
 "Copy words at point"
 (interactive "P")
 (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point))) 
	(end (progn (forward-word arg) (point))))
 (copy-region-as-kill beg end))
)


(defun copy-paragraph (&optional arg)
 "Copy paragraphes at point"
 (interactive "P")
 (let ((beg (progn (backward-paragraph 1) (point))) 
	(end (progn (forward-paragraph arg) (point))))
 (copy-region-as-kill beg end))
)

;; ===================================================================
;; cua mode会导致ctrl-v变成粘贴
;; CUA-MODE 矩形块复制粘贴操作 http://www.emacswiki.org/emacs/CuaMode
;; (cua-mode t)
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands 矩形操作后不TAB对齐
;; (transient-mark-mode 1) ;; No region when it is not highlighted
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; =========================================================
;; 查找和替换
;; query-replace-regexp is bound by default to C-M-%, although some people prefer using an alias, 
;;    like M-x qrr. Put the following in your InitFile to create such alias.
(defalias 'qrr 'query-replace-regexp)

;; 结束
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
