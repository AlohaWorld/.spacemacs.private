;;; rotate-windows.el --- Rotate windows package
;; 
;; Copyright (C) 2012, Rocky Zhang
;; 
;; Author: Rocky Zhang <rockyzhz@163.com>
;; Last-Updated: Fri Jan 06 21:27:16 2012
;; Keywords: rotate-windows
;; 
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; Use command `rotate-window-clockwise' and `rotate-window-anticlockwise'
;;     to rotate windows in any splitted manner in the current frame.
;; 
;; If there are too many windows in horizontal or vertical direction,
;; it would lose some windows after rotation. At this situation,
;; program would ask you for your choice:
;;   `y' stands for going on any way;
;;       At this situation, you can roll back the layout simply by rotate
;;       back immediately, ie. `rotate-window-clockwise' command to rotate
;;       the `rotate-window-anticlockwise' back, and vice versa.
;;   `n' stands for quitting;
;;   `s' stands for saving current window configuration and then going on.
;;       Here this saved current window configuration can be restore by
;;       the command `load-layout'.
;; 
;; Use command `save-layout' to save the current window configuration,
;;     and use `load-layout' to load this window configuration.
;; 
;; Copy rotate-windows.el to your load-path and add to your .emacs:
;; 
;;    (require 'rotate-windows)
;; 
;; Example setup:
;; 
;;    (add-to-list 'load-path "~/Emacs/rotate-windows/")
;;    (require 'rotate-windows)
;;    (global-set-key (kbd "C-x <up>") 'rotate-window-clockwise)
;;    (global-set-key (kbd "C-x <down>") 'rotate-window-anticlockwise)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; History:
;; 
;; Jan 06, 2012 -- rotate-windows version 1.0 by Rocky Zhang
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; -------------------------------------------------------------------
;; Some useful macros and utilities
;; -------------------------------------------------------------------
(defmacro are-all(list-or-cons predicate)
  "Return t if all of LIST-OR-CONS is satisfied predicate function PREDICATE."
  (cond ((listp (cdr (eval list-or-cons)))
	 `(and ,@(eval `(mapcar ',predicate ,list-or-cons))))
	(t
	 `(and (eval (,predicate (car ,list-or-cons)))
	       (eval (,predicate (cdr ,list-or-cons)))))))

(defmacro inc(var &optional step minus)
  "Increase the value of VAR by step STEP (1 by default),
and set this value back to VAR."
  (cond ((and (eval `(symbolp ',var)) (numberp (eval var)))
	 (if step
	     (if (numberp (eval step))
		 (if minus
		     `(setq ,var (- ,var ,step))
		   `(setq ,var (+ ,var ,step)))
	       (error "The \`step\' arg \"%s\" should be a number." step))
	   (if minus
	       `(setq ,var (1- ,var))
	     `(setq ,var (1+ ,var)))))
	((and (eval `(symbolp ',var))
	      (consp (eval var))
	      (numberp (car (eval var)))
	      (numberp (cdr (eval var))))
	 (if step
	     (if (and (consp (eval step))
		      (numberp (car (eval step)))
		      (numberp (cdr (eval step))))
		 (if minus
		     `(progn (setf (car ,var) (- (car ,var) (car (eval ',step)))
				   (cdr ,var) (- (cdr ,var) (cdr (eval ',step))))
			     ,var)
		   `(progn (setf (car ,var) (+ (car ,var) (car (eval ',step)))
				 (cdr ,var) (+ (cdr ,var) (cdr (eval ',step))))
			   ,var))
	       (error "The \`step\' arg \"%s\" should be a cons type variable." step))
	   (if minus
	       `(progn (setf (car ,var) (1- (car ,var))
			     (cdr ,var) (1- (cdr ,var)))
		       ,var)
	     `(progn (setf (car ,var) (1+ (car ,var))
			   (cdr ,var) (1+ (cdr ,var)))
		     ,var))))
	(t (error "The \`var\' arg \"%s\" should be a number type symbol or a cons type symbol." var))))

(defmacro tail(list)
  ""
  `(car (reverse ,list)))

(defmacro notail(list &optional N)
  "Return the a new list which is the list LIST
without the tail N elements.
If N is omit, then it takes the default value of 1."
  (let ((n (or N 1)))
    `(reverse (nthcdr ,n (reverse ,list)))))

(defmacro applies(func &rest sequence)
  "This is almost the same with `apply', except that
this can use the special form as the function to be
called."
  (let (it
	(tail (eval (tail sequence)))
	(notail (notail sequence)))
    (if (not (listp tail))
	(error "The last arg \"%s\" of \`sequence\' must be a list." (car (last sequence)))
      (dolist (elt notail)
	(add-to-list 'it elt t))
      (dolist (elt tail)
	(add-to-list 'it elt t))
      (add-to-list 'it (eval func))
      it)))

(defmacro copylist (newvar var)
  "Just act as `setq' for list type variable."
  `(setq ,newvar (copy-tree ,var)))

(defalias 'setl 'copylist)

(defun set-nth(listn n object)
  "Set the N-th element of the list LIST to object OBJECT.
When n is 0, it is equivalent to `setcar'"
  (if (and (listp listn)
	   (numberp n)
	   (< n (length listn))
	   (>= n 0))
      (let ((idx 0)
	    (tlist listn))
	(while (< idx n)
	  (setq tlist (cdr tlist))
	  (inc idx))
	(setcar tlist object)
	listn)
    nil))

;; -------------------------------------------------------------------
;; Functions and commands begin:
;; -------------------------------------------------------------------
(defun save-window-state(&optional window)
  "Save the state of window WINDOW."
  (if (windowp window)
      (let ((width (- (nth 2 (window-edges window)) (nth 0 (window-edges window))))
	    (height (- (nth 3 (window-edges window)) (nth 1 (window-edges window))))
	    it)
	(push (eq window (selected-window)) it)
	(push (window-point window) it)
	(push (window-buffer window) it)
	(push (cons width height) it)
	it)))

(defun restore-window-state(content &optional window)
  "Restore the CONTENT of window WINDOW in a state."
  (let ((win (or window (selected-window))))
    (set-window-buffer win (nth 1 content))
    (set-window-point  win (nth 2 content))))

(defun frame-state(tree &optional frame)
  "Return a window-tree with respect to the function of `window-tree'.
In this returned value, each sub-window object will substitute by a list,
which is with its size property and corresponding buffers."
  (let ((tmp-tree tree))
    (cond ((windowp tmp-tree)
	   (setq tmp-tree (save-window-state tmp-tree)))
	  ((listp tmp-tree)
	   (let* ((win-edges (nth 1 tmp-tree))
		  (width (- (nth 2 win-edges) (nth 0 win-edges)))
		  (height (- (nth 3 win-edges) (nth 1 win-edges)))
		  (idx 2)
		  (idx-max (length tmp-tree))
		  child)
	     (set-nth tmp-tree 1 (cons width height))
	     (while (< idx idx-max)
	       (setq child (nth idx tmp-tree))
	       (set-nth tmp-tree idx (frame-state child frame))
	       (inc idx)))))
    tmp-tree))

(defun win-size-p(win-size)
  "Return t if win-size is a valid window size with the form \(width . height\)."
  (and (consp win-size)
       (are-all win-size integerp)
       (<= (car win-size) (+ (frame-parameter nil 'width) 2))
       (<= (cdr win-size) (1- (frame-parameter nil 'height)))
       (>= (car win-size) window-min-width)
       (>= (cdr win-size) window-min-height)))

(defun content-p(content)
  "Return t if (car content) is a valid return value of `window-edges'
and (nth 1 content) ia a buffer."
  (and (= (length content) 4)
       (win-size-p (nth 0 content))
       (bufferp    (nth 1 content))
       (integerp   (nth 2 content))
       (booleanp   (nth 3 content))))

(defun rotate-size(win-size)
  "Rotate the window size WIN-SIZE for 90бу."
  (if (win-size-p win-size)
      (let* ((full-width (+ (frame-parameter nil 'width) 2))
	     (full-height (1- (frame-parameter nil 'height)))
	     (h-2-w (/ (float full-width) (float full-height)))
	     (w-2-h (/ (float full-height) (float full-width)))
	     (width (max (truncate (fround (* (cdr win-size) h-2-w))) window-min-width))
	     (height (max (truncate (fround (* (car win-size) w-2-h))) window-min-height))
	     (tmp-edges (cons width height)))
	(if (win-size-p tmp-edges)
	    (setq win-size tmp-edges)
	  (error "Some window is too small to be created.")))))

(defun rotate-content(content)
  "Rotate the window content CONTENT for 90бу."
  (if (content-p content)
      (set-nth content 0 (rotate-size (car content)))))

(defun rotate-state(state &optional clockwise)
  "Rotate the state for 90бу. If CLOCKWISE is non-nil, rotate clockwise.
Otherwise rotate anticlockwise."
  (if (content-p state)
      (rotate-content state)
    (let ((contents (cdr (cdr state)))
	  (no-reverse (or (and (not clockwise) (car state))
			  (and clockwise (not (car state)))))
	  tmp-content-copy
	  tmp-contents)
      (dolist (state-or-content contents)
	(setq tmp-content-copy (copy-tree state-or-content))
	(add-to-list 'tmp-contents (rotate-state tmp-content-copy clockwise) no-reverse 'eq))
      (setcdr (cdr state) tmp-contents)
      (set-nth state 0 (not (car state)))
      (set-nth state 1 (rotate-size (nth 1 state))))))

(defun number-of-contents(state &optional size)
  "If size is omitted or nil, the return value is a cons
cell (horizon . vertical), in which the `horizon' is the
max number of contents in horizontal direction, so as the
`vertical'. Otherwise, the return value is a cons cell
as the same of the above, but it would count all the size
in amount in both directions.
This function is designed for `adjust-state'."
  (let* (horizon
	 vertical
	 value)
    (if (not (content-p state))
	(let ((idx 2)
	      (idx-max (length state))
	      countlist)
	  (while (< idx idx-max)
	    (push (number-of-contents (nth idx state) size) countlist)
	    (inc idx))
	  (if (car state)
	      (setf hfunc 'max vfunc '+)
	    (setf hfunc '+ vfunc 'max))
	  (setq horizon  (apply hfunc (mapcar 'car countlist)))
	  (setq vertical (apply vfunc (mapcar 'cdr countlist))))
      (if size
	  (setf horizon (caar state) vertical (cdar state))
	(setf horizon 1 vertical 1)))
    (setq value (cons horizon vertical))))

(defun need-adjust(state)
  "Return t if state need to be adjusted, else return nil.
The criterion that the state STATE need to be adjusted is
that all the child state size in the (car state) direction
(which when (car state) is t stands for vertical splitted.)
is greater than the size of state itself, or any one of its
child state need to be adjusted, this is a recursive process."
  (let ((total-size (if (content-p state)
			(car state)
		      (car (cdr state))))
	size
	need)
    (if (content-p state)
	(setq need nil)
      (setq need (applies 'or (mapcar 'need-adjust (cdr (cdr state)))))
      (let ((idx 2)
	    (idx-max (length state))
	    child)
	(setf size (cons 0 0) (car size) 0 (cdr size) 0)
	(while (< idx idx-max)
	  (setq child (nth idx state))
	  (inc size (if (content-p child) (car child) (car (cdr child))))
	  (inc idx)))
      (setq need (or need
		     (if (car state)
			 (< (cdr total-size) (cdr size))
		       (< (car total-size) (car size))))))
    need))

(defun adjust-state(state)
  "Return t if state STATE is adjusted successfully,
else return h(or v) if there are too many windows in
the horizontal(or vertical) direction."
  (let (value)
    (if (or (not (need-adjust state))
	    (content-p state))
	(setq value t)
      (cond ((> (* (car (number-of-contents state))
		   window-min-width)
		(car (nth 1 state)))
	     (setq value 'h))
	    ((> (* (cdr (number-of-contents state))
		   window-min-height)
		(cdr (nth 1 state)))
	     (setq value 'v))
	    ((need-adjust state)
	     ;; step 1: satisfy all the request size of its children;
	     (let ((idx 2)
		   (idx-max (length state))
		   n-nth)
	       (while (< idx idx-max)
		 (setq n-nth (if (content-p (nth idx state)) 0 1))
		 (set-nth n-nth (nth idx state)
			  (number-of-contents (nth idx state) t))
		 (inc idx)))
	     ;; step 2: adjust.
	     (let* ((request-size (number-of-contents state t))
		    (dfunc (if (< (car (nth 1 state)) (car request-size))
			       'car
			     'cdr))
		    (remained-size (- (funcall dfunc request-size)
				      (funcall dfunc (cadr state))))
		    (min-size (cons window-min-width window-min-height))
		    (idx 0)
		    (idx-max (- (length state) 2))
		    child
		    size
		    dfunc-size
		    could)
	       (while (> remained-size 0)
		 (setq child (nth (+ idx 2) state))
		 (setq size (if (content-p child) (car child) (cadr child)))
		 (setq could (> (funcall dfunc size)
				(* (funcall dfunc (number-of-contents child))
				   (funcall dfunc min-size))))
		 (if could
		     (cond ((eq dfunc 'car)
			    (setf (car size)  (1- (car size)))
			    (inc remained-size -1))
			   (t
			    (setf (cdr size)  (1- (cdr size)))
			    (inc remained-size -1))))
		 (if (need-adjust child)
		     (adjust-state child))
		 (setq idx (mod (inc idx) idx-max))))
	     (setq value t))
	    (t
	     (setq value t))))
    value))

(defun activate-state(state &optional init)
  "Activate the state STATE. This is a reconstruction process of
state STATE."
  (if init (delete-other-windows))
  (let (focus-window)
    (if (not (content-p state))
	(let ((idx 2)
	      (idx-max (length state))
	      win-other)
	  (while (< idx idx-max)
	    (let* ((child (nth idx state))
		   (size (if (content-p child)
			     (if (car state) (cdr (car child)) (car (car child)))
			   (if (car state) (cdr (nth 1 child)) (car (nth 1 child)))))
		   tmp-window)
	      (if (< idx (1- idx-max))
		  (setq win-other (split-window (selected-window) size (not (car state)))))
	      (if (not (content-p child))
                  (progn (setq tmp-window (activate-state child))
			 (setq focus-window (or focus-window tmp-window)))
		(restore-window-state child)
		(if (nth 3 child) (setq focus-window (selected-window))))
	      (select-window win-other))
	    (inc idx)))
      (setq focus-window (or focus-window
			     (if (nth 3 state)
				 (get-buffer-window (nth 1 state))))))
    focus-window))

(defvar last-window-configuration
  (current-window-configuration)
  "It's the configuration of last window.")

(defvar some-state
  (current-window-configuration)
  "It's the configuration of some state.")

(defun rotate-window-clockwise()
  "Rotate windows clockwise. If there are too many windows
in horizontal or vertical direction, it would lose some windows
after rotation. At this situation, program would ask you for
your choice:
  `y' stands for going on any way;
      At this situation, you can roll back the layout simply by rotate
      back immediately, ie. `rotate-window-clockwise' command to rotate
      the `rotate-window-anticlockwise' back, and vice versa.
  `n' stands for quitting;
  `s' stands for saving current window configuration and then going on.
      Here this saved current window configuration can be restore by
      the command `load-layout'."
  (interactive)
  (let ((state (rotate-state (frame-state (car (window-tree))) t))
	(current-configuration (current-window-configuration))
	(keep-going t))
    (if (eq last-command 'rotate-window-anticlockwise)
	(set-window-configuration last-window-configuration)
      (if (need-adjust state)
	  (let ((result (adjust-state state))
		dir
		choice)
	    (cond ((eq result 'h)
		   (setq dir "vertical"))
		  ((eq result 'v)
		   (setq dir "horizontal"))
		  ((eq result t)
		   (setq keep-going t)))
	    (when (not (eq result t))
	      (setq choice
		    (read-char
		     (format "There is too many window in the %s direction. Please input you choice
\(y for going on, n for quitting, s for saving current window configuration and going on\):"
			     dir)))
	      (cond ((eq choice ?y)
		     (setq keep-going t))
		    ((eq choice ?n)
		     (setq keep-going nil))
		    ((eq choice ?s)
		     (setq some-state current-configuration)
		     (setq keep-going t))
		    (t
		     (error "Invalid choice!"))))))
      (when keep-going
	(select-window (activate-state state t))))
    (setq last-window-configuration current-configuration)))

(defun rotate-window-anticlockwise()
  "Rotate windows clockwise. If there are too many windows
in horizontal or vertical direction, it would lose some windows
after rotation. At this situation, program would ask you for
your choice:
  `y' stands for going on any way;
      At this situation, you can roll back the layout simply by rotate
      back immediately, ie. `rotate-window-clockwise' command to rotate
      the `rotate-window-anticlockwise' back, and vice versa.
  `n' stands for quitting;
  `s' stands for saving current window configuration and then going on.
      Here this saved current window configuration can be restore by
      the command `load-layout'."
  (interactive)
  (let ((state (rotate-state (frame-state (car (window-tree)))))
	(current-configuration (current-window-configuration))
	(keep-going t))
    (if (eq last-command 'rotate-window-clockwise)
	(set-window-configuration last-window-configuration)
      (if (need-adjust state)
	  (let ((result (adjust-state state))
		dir
		choice)
	    (cond ((eq result 'h)
		   (setq dir "vertical"))
		  ((eq result 'v)
		   (setq dir "horizontal"))
		  ((eq result t)
		   (setq keep-going t)))
	    (when (not (eq result t))
	      (setq choice
		    (read-char
		     (format "There is too many window in the %s direction. Please input you choice
\(y for going on, n for quitting, s for saving current window configuration and going on\):"
			     dir)))
	      (cond ((eq choice ?y)
		     (setq keep-going t))
		    ((eq choice ?n)
		     (setq keep-going nil))
		    ((eq choice ?s)
		     (save-layout current-configuration)
		     (setq keep-going t))
		    (t
		     (error "Invalid choice!"))))))
      (when keep-going
	(select-window (activate-state state t))))
    (setq last-window-configuration current-configuration)))

(defun save-layout(&optional configuration)
  "Save the current window configuration, and can be load by
`load-layout' command."
  (interactive)
  (setq configuration (or configuration (current-window-configuration)))
  (setq some-state configuration))

(defun load-layout()
  "Load the window configuration that saved by `save-layout' command."
  (interactive)
  (set-window-configuration some-state))

;; -------------------------------------------------------------------
;; provide
;; -------------------------------------------------------------------
(provide 'rotate-window)
;;; rotate-window.el ends here
