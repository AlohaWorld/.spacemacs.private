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

;; 用拼音首字母搜索汉字
(global-set-key "\C-cs" 'ace-pinyin-jump-char-2)


;; An initial C-w/M-w will grab only the word at point, while calling it a
;; second time will grab the entire line.
;; https://emacs.stackexchange.com/questions/2347/kill-or-copy-current-line-with-minimal-keystrokes

;; *** Copy word/line without selecting
(defadvice kill-ring-save (before slick-copy-line activate compile)
  "When called interactively with no region, copy the word or line

Calling it once without a region will copy the current word.
Calling it a second time will copy the current line."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (if (eq last-command 'kill-ring-save)
           (progn
             ;; Uncomment to only keep the line in the kill ring
             ;; (kill-new "" t)
             (message "Copied line")
             (list (line-beginning-position)
                   (line-beginning-position 2)))
         (save-excursion
           (forward-char)
           (backward-word)
           (mark-word)
           (message "Copied word")
           (list (mark) (point)))))))

;; *** Kill word/line without selecting
(defadvice kill-region (before slick-cut-line first activate compile)
  "When called interactively kill the current word or line.

Calling it once without a region will kill the current word.
Calling it a second time will kill the current line."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
    (if (eq last-command 'kill-region)
        (progn
          ;; Return the previous kill to rebuild the line
          (yank)
          ;; Add a blank kill, otherwise the word gets appended.
          ;; Change to (kill-new "" t) to remove the word and only
          ;; keep the whole line.
          (kill-new "")
          (message "Killed Line")
          (list (line-beginning-position)
                (line-beginning-position 2)))
      (save-excursion
        (forward-char)
        (backward-word)
        (mark-word)
        (message "Killed Word")
        (list (mark) (point)))))))



;; https://emacs.stackexchange.com/questions/14231/modification-of-kill-ring-save-to-copy-current-word-line-whole-buffer-if-no-reg
;; on the first execution behaves like the regular kill-ring-save if some region is selected
;; on the first execution copies the current word if no region is selected,
;; on the second execution copies the current line
;; on the third execution copies the current paragraph
;; on the fourth execution and so on copies the whole current buffer

;; expand-region or easy-kill or hydra might provide better idea on how to do one thing continuously
(defvar my-kill-ring-save--counter)

(defun my-kill-ring-save ()
  (interactive)
  (if (eq last-command this-command)
      (cl-incf my-kill-ring-save--counter)
    (setq my-kill-ring-save--counter 1))
  (cond ((eq my-kill-ring-save--counter 1)
         (if (use-region-p)
             (kill-ring-save (region-beginning) (region-end))
           (kill-new (current-word))))
        ((eq my-kill-ring-save--counter 2)
         (kill-ring-save (line-beginning-position) (line-end-position)))
        ((eq my-kill-ring-save--counter 3)
         (let (page-beginning-pos page-end-pos)
           (save-excursion              ; I am not really sure about them nowadays
            ; (save-restriction
               (forward-page)
               (setq page-end-pos (point))
               (forward-page -1)
               (setq page-beginning-pos (point))) ;)
           (kill-ring-save page-beginning-pos page-end-pos)))
        ((eq my-kill-ring-save--counter 4)
         (kill-new (buffer-string)))))

;; For testing
(global-set-key (kbd "<f6>") #'my-kill-ring-save)
