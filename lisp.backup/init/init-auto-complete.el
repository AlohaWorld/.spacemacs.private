;;; init-auto-complete.el --- Configuration for auto-complete mode

;; Filename: init-auto-complete.el
;; Description: Configuration for auto-complete mode
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-02 11:08:12
;; Version: 0.1
;; Last-Updated: 2008-12-02 11:08:15
;;           By: Andy Stewart
;; URL:
;; Keywords: auto-complete
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `auto-complete' `auto-complete-extension'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Configuration for auto-complete mode
;;

;;; Installation:
;;
;; Put init-auto-complete.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-complete)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/02
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'auto-complete)
(require 'auto-complete-config)

(require 'auto-complete-extension nil t) ;optional
(require 'auto-complete-yasnippet nil t) ;optional
(require 'auto-complete-semantic nil t)  ;optional
(require 'auto-complete-gtags nil t)     ;optional

;;; Code:

;; Generic setup.
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

(global-auto-complete-mode t)           ;enable global-mode
(setq ac-show-menu-immediately-on-auto-complete t)
                                        ;使用M-x auto-complete指令时，展开一个菜单

;;(setq ac-auto-start t)                ;automatically start complete
(setq ac-auto-start 2)                  ;want to start completion automatically 
										; only when you has inserted 3 or more 
										; characters

;;(setq ac-auto-start nil)				;If you are being annoyed with 
										; displaying completion menu, you can 
										; disable automatic starting completion 
										; by setting `ac-auto-start` to `nil`.

;;(define-key ac-mode-map (kbd "M-\\") 'auto-complete)
										;If "ac-auto-start" is nil, then we should
										; bind some key to `auto-complete` command,
										; e.g. bind to `ac-mode-map`, which is a 
										; key map for `auto-complete-mode` enabled
										; buffer
;;(global-set-key "\M-/" 'auto-complete);Or bind to global key map
(ac-set-trigger-key "TAB")              ;Or use trigger-key 

;;(setq ac-auto-show-menu nil)          ; do NOT show candidates menu
(setq ac-auto-show-menu 0.5)            ; Show 0.5 second later
;;(setq ac-auto-show-menu t)              ; Show menu

(add-to-list 'ac-user-dictionary-files
			 "~/.emacs.d/dict/user.dict")
                                        ;user's dict for AC, one word per line

(setq ac-dwim t)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
(setq ac-use-fuzzy t)                   ;Whether or not to use fuzzy matching

;; The mode that automatically startup.
(setq-default ac-sources '(
				   ac-source-abbrev 
				   ac-source-dictionary 
				   ac-source-filename
				   ac-source-files-in-current-dir
				   ac-source-functions
				   ac-source-imenu
				   ac-source-semantic
				   ac-source-words-in-same-mode-buffers
				   ;;ac-source-words-in-buffer
;;				   ac-source-yasnippet
))

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(add-hook 'text-mode-hook 'ac-common-setup)


;; auto-complete-mode won't be enabled automatically for modes that are not 
;; in ac-modes. So you need to set if necessary:
; (add-to-list 'ac-modes 'some-mode)
(setq ac-modes
	  '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode
						c-mode cc-mode c++-mode java-mode perl-mode
						cperl-mode python-mode ruby-mode
						ecmascript-mode javascript-mode php-mode css-mode
						makefile-mode sh-mode fortran-mode f90-mode ada-mode
						xml-mode sgml-mode
						haskell-mode literate-haskell-mode
						emms-tag-editor-mode
						asm-mode
						org-mode))

(add-to-list 'ac-trigger-commands 'org-self-insert-command) 
										;If you want enable auto-complete at 
										; org-mode, uncomment this line

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook (list
		'emacs-lisp-mode-hook
		'lisp-interaction-mode
		))
  (add-hook hook '(lambda ()
		(add-to-list 'ac-sources 'ac-source-symbols)
		(add-to-list 'ac-sources 'ac-source-variables)
		(add-to-list 'ac-sources 'ac-source-features) ; for lisp (require '
		)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C-common-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enables omnicompletion with `c-mode-common'.
(add-hook 'c-mode-common-hook
	   '(lambda ()
		  (add-to-list 'ac-omni-completion-sources
					   (cons "\\." '(ac-source-semantic)))
		  (add-to-list 'ac-omni-completion-sources
					   (cons "->" '(ac-source-semantic)))
		  (add-to-list 'ac-sources 'ac-source-gtags)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C++-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords.
(add-hook 'c++-mode-hook '(lambda ()
						 (add-to-list 'ac-sources 'ac-c++-sources)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Haskell mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords.
(add-hook 'haskell-mode-hook '(lambda ()
							 (add-to-list 'ac-sources 'ac-source-haskell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CSS mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Because there is NO "hook" in emacs 24 build-in css-mode;
;;   so we just disable it
;(add-hook 'css-mode-hook '(lambda()
;							(add-to-list 'ac-source-css-property)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion for python using rope.

;; (ac-ropemacs-initialize)
(add-hook 'python-mode-hook '(lambda ()
							   (add-to-list 'ac-sources 'ac-source-ropemacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                         
;	(defun prefix-list-elements (list prefix)
;	  (let (value)
;		(nreverse
;		 (dolist (element list value)
;		   (setq value (cons (format "%s%s" prefix element) value))))))
;	(defvar ac-source-rope
;	  '((candidates
;		 . (lambda ()
;			 (prefix-list-elements (rope-completions) ac-target))))
;	  "Source for Rope")
;	(defun ac-python-find ()
;	  "Python `ac-find-function'."
;	  (require 'thingatpt)
;	  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;		(if (null symbol)
;			(if (string= "." (buffer-substring (- (point) 1) (point)))
;				(point)
;			  nil)
;		  symbol)))
;	(defun ac-python-candidate ()
;	  "Python `ac-candidates-function'"
;	  (let (candidates)
;		(dolist (source ac-sources)
;		  (if (symbolp source)
;			  (setq source (symbol-value source)))
;		  (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;				 (requires (cdr-safe (assq 'requires source)))
;				 cand)
;			(if (or (null requires)
;					(>= (length ac-target) requires))
;				(setq cand
;					  (delq nil
;							(mapcar (lambda (candidate)
;									  (propertize candidate 'source source))
;									(funcall (cdr (assq 'candidates source)))))))
;			(if (and (> ac-limit 1)
;					 (> (length cand) ac-limit))
;				(setcdr (nthcdr (1- ac-limit) cand) nil))
;			(setq candidates (append candidates cand))))
;		(delete-dups candidates)))
;	(add-hook 'python-mode-hook
;			  (lambda ()
;				(auto-complete-mode 1)
;				(set (make-local-variable 'ac-sources)
;					 (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
;				(set (make-local-variable 'ac-find-function) 'ac-python-find)
;				(set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
;				(set (make-local-variable 'ac-auto-start) nil)))
;	 
;	;;Ryan's python specific tab completion
;	(defun ryan-python-tab ()
;	; Try the following:
;	; 1) Do a yasnippet expansion
;	; 2) Do a Rope code completion
;	; 3) Do an indent                                                                                            
;	  (interactive)
;	  (if (eql (ac-start) 0)
;		  (indent-for-tab-command)))
;	 
;	(defadvice ac-start (before advice-turn-on-auto-start activate)
;	  (set (make-local-variable 'ac-auto-start) t))
;	(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;	  (set (make-local-variable 'ac-auto-start) nil))


(provide 'init-auto-complete)

;;; init-auto-complete.el ends here

