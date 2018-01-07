;;==============================================================================
;; In this init file, The attributes of fonts/colors are set

;; ==================================================
;; 设置emacs窗口的颜色方案COLOR-THEME
; (require 'color-theme)
; (eval-after-load "color-theme"
;   '(progn
;      (color-theme-initialize)
; ;;(color-theme-comidia)
; ;; (color-theme-hober)
; ;;(color-theme-clarity)
; ;; (color-theme-charcoal-black)
; (color-theme-ld-dark)
; 
; )) ;; end (progn

;; Modified by cyd@20160226 to add theme cycle (F4 key)
;; http://orgmode.org/worg/org-color-themes.html
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)

(load "color-theme-colorful-obsolescence")
(load "color-theme-tangotango")
;; (load "color-theme-folio")
;; (load "color-theme-zenash")
;; (load "color-theme-manoj")

(setq my-color-themes (list
					   'color-theme-colorful-obsolescence
					   'color-theme-tangotango
;;					   'color-theme-folio
;;					   'color-theme-zenash
;;					   'color-theme-manoj-dark
					   'color-theme-comidia
					   'color-theme-hober
					   'color-theme-clarity
					   'color-theme-charcoal-black
					   'color-theme-xp
					   'color-theme-wheat
					   ))

(defun my-theme-set-default () ; Set the first row
  (interactive)
  (setq theme-current my-color-themes)
  (funcall (car theme-current)))

(defun my-describe-theme () ; Show the current theme
  (interactive)
  (message "%s" (car theme-current)))

; Set the next theme (fixed by Chris Webber - tanks)
(defun my-theme-cycle ()            
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (funcall (car theme-current))
  (message "%S" (car theme-current)))

(setq theme-current my-color-themes)
(setq color-theme-is-global nil) ; Initialization
(my-theme-set-default)
(global-set-key [f4] 'my-theme-cycle)


;; 高亮行的背景
;; '(highline-face ((t (:background "#006600"))))
'(highline-face ((t (:background "blue"))))
'(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t)

;; customize the "delimiter lines" of the code blocks in Org files
;; --> http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
'(org-block-begin-line
  ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
'(org-block-background
  ((t (:background "#FFFFEA"))))
'(org-block-end-line
  ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))



;; 设置指针颜色
(set-cursor-color "yellow")

;; 设置光标在不同状态下的样式
;; package: cursor-chg
(require 'cursor-chg)  ; Load this library
(change-cursor-mode 1) ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 1) ; On when idle

;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
;;(set-face-foreground 'highlight "white")
;;(set-face-background 'highlight "blue")
;;(set-face-foreground 'secondary-selection "skyblue")
;;(set-face-background 'secondary-selection "darkblue")

;;设置日历的一些颜色
(setq calendar-load-hook
'(lambda ()
(set-face-foreground 'diary-face "skyblue")
(set-face-background 'holiday-face "slate blue")
(set-face-foreground 'holiday-face "white")))
 

;;------------------------------------------------------------------------------
;; Note: 
;;    1.  For the number in (set-face-attribute 'default nil :height 100)
;;        The value is in 1/10pt, so 100 will give you 10pt, etc.
;;    ;; Setting English Font 
    (set-face-attribute 
      'default nil :font "Consolas") 
;;     'default nil :font "Lucida Sans Typewriter") 
;;    ;; 'default nil :font "Lucida Console") 
;;    
;;    ;; Chinese Font 
;;    (dolist (charset '(kana han symbol cjk-misc bopomofo)) 
;;      (set-fontset-font (frame-parameter nil 'font) 
;;    					charset 
;;    					(font-spec :family "Yahei Mono" :size 16))) 
;;					(font-spec :family "楷体" :size 20))) 
(custom-set-faces
 '(default (
     (t 
	   (:stipple nil 
	    :background "#000000" 
		:foreground "#ccccff" 
		:inverse-video nil 
		:box nil 
		:strike-through nil 
		:overline nil 
		:underline nil 
		:slant normal
		:weight light 
;; weight can be: light, medium, demibold, bold, and black
		:pixelsize 16
;; Specifies the font size in pixels. This can be used instead of the point size specified after the family name
;;		:height normal
;;		:height 143
;;		:height normal
;;;		:width normal
		:foundry "outline" 
		:antialias natural
;;		:family "Courier New"))
;;        :family "微软雅黑"))
;;		:family "Lucida Bright"))
;;		:family "Lucida Console"))
;;		:family "Yahei Mono"))
;;		:family "YaHei Consolas Hybrid"))
;;		:family "楷体"))
		))
     )
  )
)
; here are 20 hanzi and 40 english chars, see if they are the same width
; 你你你你你你你你你你你你你你你你你你你你
; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/

 ;;-------------------------Shell 使用 ansi color-------------
 (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
 (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
