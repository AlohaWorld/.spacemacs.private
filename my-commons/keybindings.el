;;; keybindings.el --- General Keybindings
;;
;; Copyright (c) 2014-2016 Yidong Cui & Contributors
;;
;; Author: Yidong Cui <nathan.cui@gmail.com>
;; URL: 
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

; (global-set-key (kbd "C-x C-f") 'spacemacs/helm-find-files)

;; In windows system, Ctrl+Space is usually bound to input method.
;; In emacs, Ctrl+Space is bound to set-mark-command
;; To avoid conflicts, we need to change key sequence of C-SPC
;; cuiyidong@20180626 : We don't need to unset this key combination in Win 10
;;   Because under windows 10, Ctrl-SPC does NOT work as windows 7
;;   We can leave C-SPC unused in Win10
;; (global-unset-key (kbd "C-SPC")) 
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
(defun continue-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `continue-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
					 char)
	(search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(global-set-key "\C-cf" 'continue-go-to-char)


;; Rebind F1 as the global help key and search the word at cursor.
(global-set-key [f1] (lambda () (interactive) (manual-entry (current-word))))


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


;; Kill current buffer without reconfirmation
(global-set-key "\C-xk" 'kill-current-buffer)
(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))


;; To switch window, use Ctrl-o instead of Ctrl-x o
(global-set-key [(control o)] 'other-window)