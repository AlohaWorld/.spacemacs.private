;;; keybindings.el --- General Keybindings for my-org layer
;;
;; Copyright (c) 2014-2016 Yidong Cui & Contributors
;;
;; Author: Yidong Cui <nathan.cui@gmail.com>
;; URL: 
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Standard Key Bindings
;; C-c l 在当前位置加入一个超链接
(define-key global-map "\C-cl" 'org-store-link)
;; C-c a 进入日程表
(define-key global-map "\C-ca" 'org-agenda) 


;; ======= 4.6 Key bindings ====================================================
; Key		|	For			|	Used
; F12		|	Agenda (1 key less than C-c a)			|	Very Often
; C-c b		|	Switch to org file			|	Very Often
; F11		|	Goto currently clocked item			|	Very Often
; C-c c		|	Capture a task			|	Very Often
; C-F11		|	Clock in a task (show menu with prefix)			|	Often
; f9 g		|	Gnus - I check mail regularly			|	Often
; f5		|	Show todo items for this subtree			|	Often
; S-f5		|	Widen			|	Often
; f9 b		|	Quick access to bbdb data			|	Often
; f9 c		|	Calendar access			|	Often
; C-S-f12	|	Save buffers and publish current project			|	Often
; C-c l		|	Store a link for retrieval with C-c C-l			|	Often
; f8		|	Go to next org file in org-agenda-files			|	Sometimes
; f9 r		|	Boxquote selected region			|	Sometimes
; f9 t		|	Insert inactive timestamp			|	Sometimes
; f9 v		|	Toggle visible mode (for showing/editing links)			|	Sometimes
; C-f9		|	Previous buffer			|	Sometimes
; C-f10		|	Next buffer			|	Sometimes
; C-x n r	|	Narrow to region			|	Sometimes
; f9 f		|	Boxquote insert a file			|	Sometimes
; f9 i		|	Info manual			|	Sometimes
; f9 I		|	Punch Clock In			|	Sometimes
; f9 O		|	Punch Clock Out			|	Sometimes
; f9 o		|	Switch to org scratch buffer			|	Sometimes
; f9 s		|	Switch to scratch buffer			|	Sometimes
; f9 h		|	Hide other tasks			|	Rare
; f7		|	Toggle line truncation/wrap			|	Rare
; f9 T		|	Toggle insert inactive timestamp			|	Rare
; C-c a		|	Enter Agenda (minimal emacs testing)			|	Rare

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
;; (global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;; (global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)
(global-set-key (kbd "<f9> w") 'widen)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)            ;; I'm using org-mode's original keybindings C-c C-x C-i
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c") 'org-capture)               ;; I'm using org-mode's original keybindings C-c r
(global-set-key (kbd "C-c r") 'org-capture)
