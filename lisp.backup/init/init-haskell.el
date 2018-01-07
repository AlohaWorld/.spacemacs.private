;;; init-haskell.el --- Haskell configuration

;;
;; Put init-haskell.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-haskell)
;;
;; No need more.

;; The below setting is refered from haskell-mode manual, which can be invoked by:
;;       M-x info-display-manual [RET] haskell-mode

;;; Code:

;; Added by cuiyidong@20140127
(require 'haskell-mode)
;; =========================================================
;; 5 Unicode support
;; When the haskell-unicode input method is active, you can simply type ‘->’ and it is 
;;    immediately replaced with ‘→’. Use C-\ to toggle the input method. To see a table
;;    of all key sequences use M-x describe-input-method RET haskell-unicode. A sequence
;;    like ‘<=’ is ambiguous and can mean either ‘?’ or ‘≤’. Typing it presents you with
;;    a choice. Type 1 or 2 to select an option or keep typing to use the default option.
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)

;; =========================================================
;; 6 Indent
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)   ;简单缩进模式
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)          ;智能缩进模式
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)      ;增强的智能缩进模式

;; =========================================================
;; 6.1 Interactive Block Indentation
;; By inserting the key bindings for C-, and C-. (these bindings are convenient on
;;    keyboard layouts where , and . are adjacent keys) as shown below you can
;;    interactively de/indent either the following nested block or, if a region is 
;;    active while in Transient Mark Mode (see (emacs)Disabled Transient Mark), 
;;    de/indent the active region.
;; By using a numeric prefix argument (see (elisp)Prefix Command Arguments) you can
;;    modify the shift-amount; for instance, C-u C-, increases indentation by 4 characters 
;;    at once.
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

;; =========================================================
;; 6.2 Rectangle Commands
;; ======================
;;
;; GNU Emacs provides so-called "rectangle commands" which operate on
;; rectangular areas of text, which are particularly useful for languages
;; with a layout rule such as Haskell. *Note Rectangles:
;; (emacs)Rectangles, to learn more about rectangle commands.
;;  
;; Moreover, CUA mode (*note CUA Bindings: (emacs)CUA Bindings.)
;; provides enhanced rectangle support with visible rectangle
;; highlighting. When CUA mode is active, you can initiate a rectangle
;; selection by `C-RET' and extend it simply by movement commands. You
;; don't have to enable full CUA mode to benefit from these enhanced
;; rectangle commands; you can activate CUA selection mode (without
;; redefining  `C-x',`C-c',`C-v', and `C-z') by calling `M-x
;; cua-selection-mode' (or adding `(cua-selection-mode nil)' to your
;; `haskell-mode-hook').
;; 

;; =========================================================
;; 8 Compilation
;; Haskell mode comes equipped with a specialized Compilation mode tailored to GHC’s 
;;    compiler messages with optional support for Cabal projects. See (emacs)Compilation
;;    Mode, for more information about the basic commands provided by the Compilation mode
;;    which are available in the Haskell compilation sub-mode as well. The additional 
;;    features provided compared to Emacs’ basic Compilation mode are:
;;
;;        * DWIM-style auto-detection of compile command (including support for CABAL projects)
;;        * Support for GHC’s compile messages and recognizing error, warning and info
;;                 source locations (including -ferror-spans syntax)
;;        * Support for filtering out GHC’s uninteresting ‘Loading package...’ linker messages
;; In order to use it, invoke the `haskell-compile' command instead of
;; `compile' as you would for the ordinary Compilation mode. It's
;; recommended to bind `haskell-compile' to a convenient key binding. For
;; instance, you can add the following to your Emacs initialization to
;; bind `haskell-compile' to `C-c C-c'.
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; =========================================================
;; haskell-cabal-mode
;; haskell-cabal-mode is a major mode for editing Cabal package description files and
;;    is automatically associated with files having a .cabal extension.
;;    cuiyidong@20140127:  This gives ERROR messages when turn into haskell mode for .hs file
;;(eval-after-load "haskell-mode"
;;  (define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file))

;; =========================================================
;; Rebind C-x C-s
;; To do specific actions on save, you need haskell-mode-save-buffer. It is recommended
;;    to rebind C-x C-s in haskell-mode to this. Add the following in your haskell-mode-hook.
(add-hook
 'haskell-mode-hook
 '(lambda ()
	(define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)))

;; ======= END cuiyidong@20140127 =============

;; =========================================================
;; 7 `haskell-decl-scan-mode'
;; ADDED by cuiyidong@20140501
;; haskell-decl-scan-mode enables the use of features that build upon imenu support such as 
;;    Speedbar Frames (see (emacs)Speedbar) or the global “Which Function” minor mode 
;;    (see (emacs)Which Function).
;;
;; In order to enable which-function-mode for Haskell buffers you need to add the following
;;     to your Emacs initialization:
(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'haskell-mode))


;; (load "haskell-site-file")
(autoload 'haskell-refac-mode "haskell-refac"
  "Minor mode for refactoring Haskell programs" t)
(add-hook 'haskell-mode-hook 'font-lock-mode)        ;高亮模式

(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)     ;GHCi 交互模式
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode) ;文档模式
(add-hook 'haskell-mode-hook 'haskell-refac-mode)       ;重构
;;(add-hook 'haskell-mode-hook 'hs-lint-mode-hook)        ;代码建议
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)  ; 扫描代码，将关键字加入menu中
(add-hook 'haskell-mode-hook 'turn-on-eldoc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(global-set-key "\C-ch" 'haskell-mode)
(setq haskell-indent-offset 4)

;;By default, Emacs' commands acting on words treat camelCaseIdentifiers as a 
;;single unit. If you want to make Emacs recognise the subwords inside (so that, 
;;say,  M-f will stop at "Case" in the previous example"), you can activate capitalized-
;;words-mode. Note that this only works in Emacs 23 and the version in Emacs 
;;23.1 is broken; get the fixed version of cap-words.el. Activate it either by hand 
;;(M-x capitalized-words-mode) or by adding to haskell-mode-hook and/or haskell-
;;literate-mode-hook

(add-hook 'haskell-mode-hook '(lambda () (capitalized-words-mode t)))

;; ==================================================
;; haskell 的flymake模式（一边编写代码，一边编译检查）
;; 为了使用该模式，在 hakell-mode.el 中注释掉了 三行代码，
;; 参见 http://www.emacswiki.org/emacs/FlymakeHaskell

(defun flymake-Haskell-init ()
	  (flymake-simple-make-init-impl
		'flymake-create-temp-with-folder-structure nil nil
		(file-name-nondirectory buffer-file-name)
		'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "ghc"
	(list "--make" "-fbyte-code"
		  (concat "-i"base-dir)  ;;; can be expanded for additional -i options as in the Perl script
		  source)))

(defvar multiline-flymake-mode nil)
(defvar flymake-split-output-multiline nil)

;; this needs to be advised as flymake-split-string is used in other places and I don't know of a better way to get at the caller's details
(defadvice flymake-split-output
  (around flymake-split-output-multiline activate protect)
  (if multiline-flymake-mode
	  (let ((flymake-split-output-multiline t))
		ad-do-it)
	ad-do-it))

(defadvice flymake-split-string
  (before flymake-split-string-multiline activate)
  (when flymake-split-output-multiline
	(ad-set-arg 1 "^\\s *$")))

;; Why did nobody tell me about eval-after-load - very useful
(eval-after-load "flymake"
  '(progn
	 (add-to-list 'flymake-allowed-file-name-masks
			  '("\\.l?hs$" flymake-Haskell-init flymake-simple-java-cleanup))
	 (add-to-list 'flymake-err-line-patterns
			  '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
				1 2 3 4))))

(add-hook
 'haskell-mode-hook
 '(lambda ()
	(set (make-local-variable 'multiline-flymake-mode) t)))

;; ==================================================
;; haskell 的关键字处理


;; ==================================================
;; haskell 的自动补全功能
(require 'auto-complete-config)
(ac-config-default)
(require 'init-auto-complete)
(require 'auto-complete-extension)



(provide 'init-haskell)

;;; init-haskell.el ends here


;; Haskell installed packages
;(require 'haskell-installed-packages)
;(setq haskell-ghc-pkg-bin-path "C:/Program Files (x86)/Haskell Platform/2010.2.0.0/bin/ghc-pkg")
;(haskell-installed-packages-refresh-all)

;(require 'haskell-align-imports)
;(define-key haskell-mode-map (kbd "C-c .") 'haskell-align-imports)
;(require 'haskell-sort-imports)
;(define-key haskell-mode-map (kbd "C-c ,") 'haskell-sort-imports)


;(require 'haskell-hoogle-database)
;(haskell-hoogle-database-update)

;(require 'haskell-navigate-imports)
;(define-key haskell-mode-map [f8] 'haskell-navigate-imports)

;(require 'haskell-package-exports)
;(haskell-package-exports-refresh)
