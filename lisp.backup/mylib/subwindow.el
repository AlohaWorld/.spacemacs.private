;; ==================================================
;; The following codes are collected from internet,
;; http://www.emacswiki.org/emacs-es/ThreeWindows
;; and I modified some of them to adapt to my own 
;; customs.
;; ==================================================
;; 窗口分隔、窗口旋转、Buffer旋转

;; 将窗口分隔为4个部分
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;;             window layout related               ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+
;  |                       |                      |
;  |                       |                      |
;  |                       |                      |
;  +-----------------------+----------------------+

(defun split-window-4()
 "Splite window into 4 sub-window"
 (interactive)
 (if (= 1 (length (window-list)))
     (progn (split-window-vertically)
	    (split-window-horizontally)
	    (other-window 2)
	    (split-window-horizontally)
	    )
   )
)

;; 水平切分转垂直切分
;  +----------------------+                 +----------- +-----------+ 
;  |                      |           \     |            |           | 
;  |                      |   +-------+\    |            |           | 
;  +----------------------+   +-------+/    |            |           |
;  |                      |           /     |            |           | 
;  |                      |                 |            |           | 
;  +----------------------+                 +----------- +-----------+ 

(defun split-v ()
  (interactive)
  (if (= 2 (length (window-list)))
    (let (( thisBuf (window-buffer))
	  ( nextBuf (progn (other-window 1) (buffer-name))))
	  (progn   (delete-other-windows)
		   (split-window-horizontally)
		   (set-window-buffer nil thisBuf)
		   (set-window-buffer (next-window) nextBuf)
		   ))
    )
)

;; 垂直切分转水平切分

;  +----------- +-----------+                  +----------------------+ 
;  |            |           |            \     |                      | 
;  |            |           |    +-------+\    |                      | 
;  |            |           |    +-------+/    +----------------------+ 
;  |            |           |            /     |                      | 
;  |            |           |                  |                      | 
;  +----------- +-----------+                  +----------------------+ 

(defun split-h ()
  (interactive)
  (if (= 2 (length (window-list)))
    (let (( thisBuf (window-buffer))
	  ( nextBuf (progn (other-window 1) (buffer-name))))
	  (progn   (delete-other-windows)
		   (split-window-vertically)
		   (set-window-buffer nil thisBuf)
		   (set-window-buffer (next-window) nextBuf)
		   ))
    )
)

;;  +----------------------+                +---------- +----------+
;;  |                      |          \     |           |          |
;;  |                      |  +-------+\    |           |          |
;;  +----------------------+  +-------+/    |           |          |
;;  |                      |          /     |           |          |
;;  |                      |                |           |          |
;;  +----------------------+                +---------- +----------+
;;
;;  +--------- +-----------+                +----------------------+
;;  |          |           |          \     |                      |
;;  |          |           |  +-------+\    |                      |
;;  |          |           |  +-------+/    +----------------------+
;;  |          |           |          /     |                      |
;;  |          |           |                |                      |
;;  +--------- +-----------+                +----------------------+

(defun change-split-type-2 ()
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive)
  (if (= 2 (length (window-list)))
      (let ((thisBuf (window-buffer))
            (nextBuf (progn (other-window 1) (buffer-name)))
            (split-type (if (window-full-width-p)
                            'split-window-horizontally
                            'split-window-vertically)))
        (progn
          (delete-other-windows)
	  (funcall split-type)
          (set-window-buffer nil thisBuf)
          (set-window-buffer (next-window) nextBuf)))))


;; 水平三窗口转垂直三窗口
;  +----------------------+                 +----------- +-----------+ 
;  |                      |           \     |            |           | 
;  |                      |   +-------+\    |            |           | 
;  +----------------------+   +-------+/    |            |-----------|
;  |          |           |           /     |            |           | 
;  |          |           |                 |            |           | 
;  +----------------------+                 +----------- +-----------+ 


(defun split-v-3 ()
  "Change 3 window style from horizontal to vertical"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
	    (let ((1stBuf (window-buffer (car winList)))
		  (2ndBuf (window-buffer (car (cdr winList))))
		  (3rdBuf (window-buffer (car (cdr (cdr winList))))))
	      (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
	      (delete-other-windows)
	      (split-window-horizontally)
	      (set-window-buffer nil 1stBuf)
	      (other-window 1)
	      (set-window-buffer nil 2ndBuf)
	      (split-window-vertically)
	      (set-window-buffer (next-window) 3rdBuf)
	      (select-window (get-largest-window))
	    )
	  )
    )
)

;; 垂直三窗口转水平三窗口
;  +----------- +-----------+                  +----------------------+ 
;  |            |           |            \     |                      | 
;  |            |           |    +-------+\    |                      | 
;  |            |-----------|    +-------+/    +----------------------+ 
;  |            |           |            /     |           |          | 
;  |            |           |                  |           |          | 
;  +----------- +-----------+                  +----------------------+ 


(defun split-h-3 ()
  "Change 3 window style from vertical to horizontal"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
	    (let ((1stBuf (window-buffer (car winList)))
		  (2ndBuf (window-buffer (car (cdr winList))))
		  (3rdBuf (window-buffer (car (cdr (cdr winList))))))
		(message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
		(delete-other-windows)
		(split-window-vertically)
		(set-window-buffer nil 1stBuf)
		(other-window 1)
		(set-window-buffer nil 2ndBuf)
		(split-window-horizontally)
		(set-window-buffer (next-window) 3rdBuf)
		(select-window (get-largest-window))
	      )
	    )
    )
)


;; 垂直三窗口中的buffer转换
;  +----------- +-----------+                    +----------- +-----------+ 
;  |            |     C     |            \       |            |     A     | 
;  |            |           |    +-------+\      |            |           | 
;  |     A      |-----------|    +-------+/      |     B      |-----------| 
;  |            |     B     |            /       |            |     C     | 
;  |            |           |                    |            |           | 
;  +----------- +-----------+                    +----------- +-----------+ 
;
;  +------------------------+                     +------------------------+ 
;  |           A            |           \         |           B            | 
;  |                        |   +-------+\        |                        | 
;  +------------------------+   +-------+/        +------------------------+ 
;  |     B     |     C      |           /         |     C     |     A      | 
;  |           |            |                     |           |            | 
;  +------------------------+                     +------------------------+ 


(defun roll-v-3 ()
  "Rolling 3 window buffers clockwise"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
	    (let ((1stWin (car winList))
		  (2ndWin (car (cdr winList)))
		  (3rdWin (car (cdr (cdr winList)))))
	      (let ((1stBuf (window-buffer 1stWin))
		    (2ndBuf (window-buffer 2ndWin))
		    (3rdBuf (window-buffer 3rdWin))
		    )
		    (set-window-buffer 1stWin 3rdBuf)
		    (set-window-buffer 2ndWin 1stBuf)
		    (set-window-buffer 3rdWin 2ndBuf)
		    )
	      )
	    )
    )
)


;; 三窗口切分时，垂直与水平的互相转换
;; ‘split-v-3’ and ‘split-h-3’ may also be merged into one
;;  function, that automatically detects split type and changes
;;  vertical to horizontal or vice-versa, like this:

;  +----------------------+                 +----------- +-----------+ 
;  |                      |           \     |            |           | 
;  |                      |   +-------+\    |            |           | 
;  +----------------------+   +-------+/    |            |-----------|
;  |          |           |           /     |            |           | 
;  |          |           |                 |            |           | 
;  +----------------------+                 +----------- +-----------+ 

;  +----------- +-----------+                  +----------------------+ 
;  |            |           |            \     |                      | 
;  |            |           |    +-------+\    |                      | 
;  |            |-----------|    +-------+/    +----------------------+ 
;  |            |           |            /     |           |          | 
;  |            |           |                  |           |          | 
;  +----------- +-----------+                  +----------------------+ 

(defun change-split-type-3 ()
  "Change 3 window style from horizontal to vertical and vice-versa"
  (interactive)
  (select-window (get-largest-window))
  (if (= 3 (length (window-list)))
      (let ((winList (window-list)))
	    (let ((1stBuf (window-buffer (car winList)))
		  (2ndBuf (window-buffer (car (cdr winList))))
		  (3rdBuf (window-buffer (car (cdr (cdr winList)))))
		  (split-3 
		   (lambda(1stBuf 2ndBuf 3rdBuf split-1 split-2)
		     "change 3 window from horizontal to vertical and vice-versa"
		     (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf)
		     (delete-other-windows)
		     (funcall split-1)
		     (set-window-buffer nil 1stBuf)
		     (other-window 1)
		     (set-window-buffer nil 2ndBuf)
		     (funcall split-2)
		     (set-window-buffer (next-window) 3rdBuf)
		     (select-window (get-largest-window))
		     ))
		  (split-type-1 nil)
		  (split-type-2 nil)
		  )
	      (if (window-full-width-p)
		  (setq split-type-1 'split-window-horizontally split-type-2 'split-window-vertically)
		(setq split-type-1 'split-window-vertically  split-type-2 'split-window-horizontally))
	      (funcall split-3 1stBuf 2ndBuf 3rdBuf split-type-1 split-type-2)

))))

;; 将窗口分隔为3个部分
; +----------+-----------+
; |          |           |
; |          |           |
; +          +-----------+
; |          |           |
; |          |           |
; +----------+-----------+

(defun remove-non-editor-buf (buflist)
  "remove all the non-editor-buffer from the list"
;;  (debug)
  (let ( (newbuflist nil)
		 (tmplist nil)
		 (editorBufNum 1)
		 (firstBuf nil)
		 (restBuf nil) )
	(setq firstBuf (car buflist))
	(setq restBuf  (cdr buflist))
	(while (and (not (null restBuf)) (< editorBufNum 4))
	  (if (not (or (minibufferp)
			  (equal (string-match " \\*.*\\ *" (buffer-name firstBuf)) 0)
			  ) )
		  ( progn
			(push firstBuf tmplist)
			(setq editorBufNum (1+ editorBufNum))
			)
		)
	  (setq firstBuf (car restBuf))
	  (setq restBuf (cdr restBuf))
	  )
	(setq newbuflist (reverse tmplist))
	)
)


 (defun split-window-3 () 
 "Splite window into 3 sub-window"
 (interactive) 
;; (debug)
 (let* ((templist (buffer-list))
		(bufList (remove-non-editor-buf templist) )
		(1stBuf (car bufList)) 
		(2ndBuf (car (cdr bufList))) 
		(3rdBuf (car (cdr (cdr bufList))))
		)
	 (delete-other-windows)
	 (if (= 1 (length (window-list))) 
		 (progn (split-window-horizontally) 
				(other-window 1)
				(set-window-buffer nil 2ndBuf) 
				(split-window-vertically)
				(other-window 1)
				(set-window-buffer nil 3rdBuf)
				(other-window 1)
				) 
	   ) 
	 )
 )

;; 检测当前窗口的数量，执行相应的窗口切换方法

(defun change-split-type ()
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive)
  (if (= 2 (length (window-list)))
	  (change-split-type-2)
	(change-split-type-3)
	)
)

(provide 'subwindow)
