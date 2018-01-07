;; ==================================================================
;; Yasnippet
;; Basic steps to setup:
;;   1. In your .emacs file:
;;        (add-to-list 'load-path "/dir/to/yasnippet.el")
;; done in .emacs
										;(require 'yasnippet)
;;   2. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   3. In your .emacs file
;;   4. To enable the YASnippet menu and tab-trigger expansion
;;        M-x yas/minor-mode
;;   5. To globally enable the minor mode in *all* buffers
;;        M-x yas/global-mode
;;
;;   Steps 4. and 5. are optional, you don't have to use the minor
;;   mode to use YASnippet.
(require 'yasnippet)

;; yas-snippet-dirs 默认包含两个目录
;;   1. ~/.emacs.d/snippets
;;   2. yas-installed-snippets-dir
;; 所以在 yas-snippet-dirs 中增加新目录时，不要将原来的两个覆盖了

;; (yas-load-directory yas-snippet-dirs)
(yas-global-mode 1)

(require 'dropdown-list) ;; 需要从elpa单独安装
(setq yas-prompt-functions '(yas-dropdown-prompt
							 yas-ido-prompt
							 yas/completing-prompt))
