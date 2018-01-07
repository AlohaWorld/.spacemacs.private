;;++以下和编程模式有关++++++++++++++++++++++++++++++++++++++++++++++++++

;; ==================================================
;; 启用 projectile
;; git 目录默认为一个project
;; 其它情况下，需要在目录下面创建一个空的 .projectile 文件，表明当前目录下面有一个project
(require 'projectile)
(projectile-global-mode)

;; ==================================================
;; 设置语法自动加亮
(setq font-lock-maximum-decoration t)

;; ==================================================
;; 设置自动完成功能的扩展，只针对：
;; `ac-source-gtags'   ;;Provide a completion for C or C++.
;;      need install `gtags' first.
;; `ac-source-c++'     ;;Provide a completion keyword for C++.
;; `ac-source-haskell' ;;Provide a completion for Haskell.
;;      need install `GHC' and `hoogle' first.
(require 'auto-complete-extension)

;; ==================================================
;; txt2tags
; (autoload 't2t-mode "txt2tags-mode" "Syntax Highlight para o txt2tags" t)

;; ==CSS=============================================
;; CSS
;(autoload 'css-mode "css-mode")

;; ==C================================================
;; C Programming Language, 仅指定快捷键
(defun my-c-mode-common-hook ()
	(define-key c-mode-base-map [f9] 'recompile)
	(define-key c-mode-base-map "\C-m" 'newline-and-indent)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(load "init-cpp")
;; ==================================================
;; Haskell
;;(require 'init-haskell)
(load "init-haskell")


;; ==================================================
;; 代码折叠 
(load-library "hideshow") 
(add-hook 'java-mode-hook 'hs-minor-mode) 
(add-hook 'python-mode-hook 'hs-minor-mode) 
(add-hook 'php-mode-hook 'hs-minor-mode) 
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'haskell-mod-hook 'hs-minor-mode)


;; ==================================================
;; Python

;; python-mode settings
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq interpreter-mode-alist(cons '("python" . python-mode)
								  interpreter-mode-alist))

;; Pymacs
;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-exec "pymacs" nil t)
;(autoload 'pymacs-load "pymacs" nil t)

;; Ropemacs
;(pymacs-load "ropemacs" "rope-")
;(setq ropemacs-enable-autoimport t)

;; PyComplete
;(require 'pycomplete)

;; pdb setup
;; pdb setup, note the python version
(setq pdb-path 'c:/Python33/Lib/pdb.py
       gud-pdb-command-name (symbol-name pdb-path))
 (defadvice pdb (before gud-query-cmdline activate)
   "Provide a better default command line when called interactively."
   (interactive
    (list (gud-query-cmdline pdb-path
                 (file-name-nondirectory buffer-file-name)))))



;;====================如果没有使用python-mode.el，则需要添加如下代码============
; (defvar python-mode-map ()
;   "Keymap used in `python-mode' buffers.")
;(if python-mode-map
;     nil
;  (setq python-mode-map (make-sparse-keymap)))
;;====================代码补充完毕============================================

;; (define-key py-mode-map "\t" 'ryan-python-tab)


;;======================			拷贝代码自动格式化		  =====================
;;Emacs 里对代码的格式化支持的非常好，不但可以在编辑的时候自动帮你格式化，还可以选中一块代码，
;;按 Ctrl-Alt-\ 对这块代码重新进行格式化.如果要粘贴一块代码的话，粘贴完了紧接着按 Ctrl-Alt-\,
;;就可以把新加入的代码格式化好。可是，对于这种粘贴加上重新格式化的机械操作，Emacs 应该可以将
;;它自动化才能配得上它的名气，把下面的代码加到配置文件里，你的 Emacs 就会拥有这种能力了
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
	  (and (not current-prefix-arg)
		   (member major-mode
				   '(
					 c-mode
					 c++-mode
					 clojure-mode
					 emacs-lisp-mode
					 haskell-mode
					 js-mode
					 latex-mode
						lisp-mode
					 objc-mode
					 perl-mode
					 cperl-mode
					 plain-tex-mode
					 python-mode
					 rspec-mode
						ruby-mode
					 scheme-mode))
		   (let ((mark-even-if-inactive transient-mark-mode))
			 (indent-region (region-beginning) (region-end) nil))))))
 
;;----------------------			End 拷贝代码自动格式化		---------------------


;;++编程模式结束+++++++++++++++++++++++++++++++++++++++++++++++++++++++
