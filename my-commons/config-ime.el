;;
;; <my-commons> layer config file
;; Shell configuration: setting up the shell environment
;; Cui Yidong <nathan.cui@gmail.com>
;; Rev: 20160730, 20180609

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

;; ==================================================
;; Change the default shell from cmd.exe to msys2
;; (setq shell-file-name "C:/msys64/usr/bin/zsh.exe")

(when (eq system-type 'windows-nt) 
)

;; ## 激活 Chinese-pyim ##
(require 'chinese-pyim) 
(setq default-input-method "chinese-pyim") 
(global-set-key (kbd "C-<SPC>") 'toggle-input-method) 
                                        ;(global-set-key (kbd ";") 'pyim-insert-ascii)
;; pos-tip 这个包有问题，暂时禁用；拼音直接在下面的minibuffer显示
(setq pyim-use-tooltip nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(pyim-dicts
   (quote
	  ((:name "pyim-bigdict" :file "d:/MyDocument/60.Applications/emacs/.emacs.d/pyim/dicts/pyim-bigdict.pyim" :coding utf-8-unix)))))
