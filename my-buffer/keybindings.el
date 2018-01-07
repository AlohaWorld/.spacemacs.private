;;; keybindings.el --- Buffer Management keybindings
;;
;; Copyright (c) 2014-2016 Yidong Cui & Contributors
;;
;; Author: Yidong Cui <nathan.cui@gmail.com>
;; URL: 
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Buffer Management: Switch buffer; List buffers 
;; Bind Ctrl-x b to helm-mini
;; Bind M-m b b to ido-switch-buffer
(global-set-key (kbd "C-x b") 'helm-mini)
;; Because helm buffer management is stronger than ido, I bind ido buffer to
;; spacemacs key sequences (M-m ......)
;; (global-set-key (kbd "M-m b b") 'ido-switch-buffer)
(spacemacs/set-leader-keys
  "bb" 'ido-switch-buffer)
;; Buffer 管理，将 C-x C-b 从Buffer Menu切换为 iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Move buffer to another window
(global-set-key (kbd "C-x <up>")     'buf-move-up)
(global-set-key (kbd "C-x <down>")   'buf-move-down)
(global-set-key (kbd "C-x <left>")   'buf-move-left)
(global-set-key (kbd "C-x <right>")  'buf-move-right)
