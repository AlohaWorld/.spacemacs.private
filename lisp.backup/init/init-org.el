;; =================================================
;; 这个文件用来初始化Emacs中的Org模式设置
;; 本文件将在 .emacs 或者 init.el 中调用
;; Rev: cyd@20140516
;; =================================================
;;;;(eval-after-load "org-mode" ; <-- "abcd-mode", not 'abcd-mode
;;;;      '(progn

;; 由于本文件中所用“orgPath” 变量是在 .emacs 中定义的，
;; 因此使用时要特别小心

;; ======= 4.2 Org-Mode Setup ==================================================
;; org模式初始化
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

;; Standard Key Bindings
;; C-c l 在当前位置加入一个超链接
(define-key global-map "\C-cl" 'org-store-link)
;; C-c a 进入日程表
(define-key global-map "\C-ca" 'org-agenda) 
;; 切换 org文件 buffer
(global-set-key "\C-cb" 'org-iswitchb)


;; ======= My extra setup ======================================================
;; fontify code in code blocks
;; where there is a #+begin_src xxx
(setq org-src-fontify-natively t)


;; ======= 4.4 Agenda Setup ====================================================
;; 设置变量org-agenda-files，以便让Org-Mode知道在哪些文
;; 件里搜寻TODO和计划项目 
;(setq org-agenda-files (list (concat orgPath "Enterprise.org")
;                             (concat orgPath "Campus.org")
;                             (concat orgPath "Personal.org")
;							 (concat orgPath "Todo.org")
;							 ))
;;我们不再指定具体的org文件，而是使用目录形式，所有目录下的org文件都在搜索范围内
;; cuiyidong@20140502
(setq org-agenda-files (list orgPath))


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

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file (concat orgPath "publish/scratch.org") )
  (gnus-make-directory (concat orgPath "publish")))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


;; ======= 5 Tasks and States ==================================================
;; ======= 5.1 TODO keywords ===================================================
;; 设置默认的todo关键字
;; #+SEQ_TODO: TODO(t) STARTED(s) WAITING(w) ASSIGNED(a) MAYBE(m)| DONE(d) CANCELLED(c) DEFERRED(f)
;; | 分隔完成与未完成两种状态，完成状态会打上 CLOSED 时间戳
;; ! 打上时间戳；@ 要求说明

(setq org-todo-keywords
	  (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
			  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
	  (quote (("TODO" :foreground "red" :weight normal)
			  ("NEXT" :foreground "blue" :background "grey" :weight bold)
			  ("DONE" :foreground "dark gray" :weight normal)
			  ("WAITING" :foreground "orange" :weight bold)
			  ("HOLD" :foreground "magenta" :weight normal)
			  ("CANCELLED" :foreground "forest green" :weight normal)
			  ("PHONE" :foreground "forest green" :weight normal)
			  ("MEETING" :foreground "forest green" :weight bold))))

;; ======= 5.2 Fast Todo Selection =============================================
;; 快速选择标签(tag)
;;   Changing a task state is done with C-c C-t KEY
;;   where KEY is the appropriate fast todo state selection key as defined in 
;;     org-todo-keywords.
(setq org-use-fast-todo-selection t)

;; 用Shift+左右箭头改变TODO状态，而避免状态转换过程中输入时间戳或者注释
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; ======= 5.3 TODO state triggers==============================================
;; The tags are used to filter tasks in the agenda views conveniently.
;; Moving a task to CANCELLED adds a CANCELLED tag
;; Moving a task to WAITING adds a WAITING tag
;; Moving a task to HOLD adds a WAITING tag
;; Moving a task to a done state removes a WAITING tag
;; Moving a task to TODO removes WAITING and CANCELLED tags
;; Moving a task to NEXT removes a WAITING tag
;; Moving a task to DONE removes WAITING and CANCELLED tags
(setq org-todo-state-tags-triggers
	  (quote (
			  ("CANCELLED" ("CANCELLED" . t))
			  ("WAITING"   ("WAITING" . t))
			  ("HOLD"      ("WAITING" . t) ("HOLD" . t))
			  (done        ("WAITING") ("HOLD"))
			  ("TODO"      ("WAITING") ("CANCELLED") ("HOLD"))
			  ("NEXT"      ("WAITING") ("CANCELLED") ("HOLD"))
			  ("DONE"      ("WAITING") ("CANCELLED") ("HOLD")))))

;; My Emacs setup saves all org buffers at 1 minute before 
;; the hour using the following code in my .emacs
(run-at-time "00:59" 3600 'org-save-all-org-buffers)


;; Set to the location of your Org files on your local system
(setq org-directory orgPath )
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat orgPath "Note.org") )
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory (concat dropboxPath "MobileOrg") )

;; Set to my diary file (added by cuiyidong@20130714)
(setq org-my-diary-file (concat orgPath "Diary.org"))


(require 'org-habit) ;; org-habit is not default loaded.
;; 给已完成事项打上时间戳。可选 note，附加注释
(setq org-log-done 'time)

;; 在org模式中自动检测table
(add-hook 'org-mode-hook 'table-recognize)

;; 在 agenda-view 中显示所有的TODO list，包括未schedule的TODO
(setq org-agenda-custom-commands 
	  '(("x" "Agenda and todos" 
		 ((agenda "") 
		  (todo))))) 

;; ==================================================
;; 设置默认的tags
(setq org-tag-alist '((:startgroup . nil)
                        ("@OFFICE" . ?o) ("@HOME" . ?h)
                      (:endgroup . nil)
                        ("PROJECT" . ?p) ("DIR" . ?d)
						(:newline . nil)
						("READING" . ?r) ("NOTES" . ?n)
						("IDEA" . ?i) ("ACTION" . ?a)
					  ))


;;======== 6 Adding New Tasks Quickly with Org Capture =========================

;;======== 6.1 Capture Templates ===============================================
;; 我们使用 capture-mode 代替 remember-mode
;; 设定 Capture templates
;; template的格式如下
;; a shortcut key (a letter)
;; a description of the template
;; a type of entry. These can be an org-mode entry (node) with a headline, an 
;;    item in a list, a checkbox item (checkitem) , a line in a table (table-line)
;;    or plain text.
;; a target specifying where the captured item should be placed
;; a template - a strng of text with special escape codes for date stamps, 
;;    prompt strings, etc
;; additional behaviour properties (I am still stumped on how to set up 
;;    "property lists")

;; 添加新任务时，要将任务加上分类：
;;  A phone call (p)
;;  A meeting (m)
;;  An email I need to respond to (r)
;;  A new task (t)
;;  A new note (n)
;;  An interruption (j)
;;  A new habit (h)

;; Set the default location to store the captured ideas 
(setq org-default-notes-file (concat orgPath "refile.org") )

(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree (concat orgPath "Diary.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file org-default-notes-file)
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/git/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file org-default-notes-file)
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file org-default-notes-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))


;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
	;; cyd@20160223
    ;; In order to solve the misbehaving issue (gibberish) of
	;; org-capture and time clocking, we modify the following
	;; line according to http://stackoverflow.com/questions/21767471
;;    (org-remove-empty-drawer-at "LOGBOOK" (point))))
	(org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;;======== 6.2 Separate file for Capture Tasks =================================
;; 仅使用一个refile.org文件来存储针对多个类别的任务
;; 在refile.org文件中，第一行永久保留为：
;;  #+FILETAGS: REFILE

										
;; ==================================================
;; 为org模式提供额外小窗口显示大纲，并且避免行的自动截短
(require 'org-toc)
(defun my-org-hook()
  (interactive)
  (local-set-key (kbd "M-o") 'org-toc-show)
  (setq truncate-lines nil) 
  )
(add-hook 'org-mode-hook 'my-org-hook)


;;===================================================
;; 记录到 refile.org 中的 captured ideas 需要重新放置到
;; 合适的org文件的合适的位置，这叫做 refile。以下是refile
;; 的配置

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
								 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
; (setq org-completion-use-ido t)

;;;; Below added on 2013/07/13
;(setq ido-everywhere t)
;(setq ido-max-directory-size 100000)
;(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
;(setq ido-default-file-method 'selected-window)
;(setq ido-default-buffer-method 'selected-window)
;;;; 2013/07/13 over

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


;;===================================================
;; Setup the Agenda View

;; Dim blocked tasks
(setq org-agenda-dim-blocked-tasks t)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-agenda-sorting-strategy
                             '(priority-down category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Project Next Tasks")
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(priority-down todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING/!"
                           ((org-agenda-overriding-header (if (marker-buffer org-agenda-restrict-begin) "Project Subtasks" "Standalone Tasks"))
                            (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING/!"
                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                            (org-agenda-skip-function 'bh/skip-stuck-projects)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
              ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
              ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")
                (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-tags-match-list-sublevels nil))
              ("A" "Tasks to Archive" tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                (org-tags-match-list-sublevels nil))))))


;;===================================================
;; Filter the agenda view
;; setup to allow / RET to filter tasks
(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "ignore")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)



;;===================================================
;; 9.1 Clock Setup (Setup the Clock functionalities)
;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
										; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;;================================================
;; Clock in tasks by ID is obsoleted.
;; f9-SPC calls bh/clock-in-last-task which switches the clock back to the 
;; previously clocked task
;;
(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

;; 9.5 Editing Clock Entries
;; makes time editing use discrete minute intervals (no rounding) increments:
(setq org-time-stamp-rounding-minutes (quote (1 1)))

;; shows 1 minute clocking gaps.
(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
							:min-duration 0
							:max-gap 0
							:gap-ok-around ("4:00"))))


;;==================================================
;; 10 Time Reporting And Tracking
;; 10.1.1 Verify That The Clock Data Is Complete And Correct
;; Remove clock entries with a zero duration.
;; This removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; 10.1.2 Using Clock Reports To Summarize Time Spent
;; show 5 levels of detail with links to the tasks. Set wider reports than the 
;; default compact setting so I override the :narrow value.
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; 10.2 Task Estimates And Column View
;;   10.2.1 Creating A Task Estimate With Column Mode
;;Creating A Task Estimate With Column Mode
										; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

;; A property called Effort records the estimated amount of time a given task
;; will take to complete.
										; Effort estimate values
										; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; 10.3 Providing Progress Reports To Others
;; Agenda log mode items to display (closed and state changes by default)
;; To generate the report I pull up the agenda for the appropriate time frame
;;     (today, yesterday, this week, or last week) and hit the key sequence l R to 
;;     add the log report (without clocking data lines) and the agenda clock 
;;     report at the end.
(setq org-agenda-log-mode-items (quote (closed state)))


;;==================================================
;; 11 Tags (use tags mostly for filtering in the agenda)
;; 11.1 Tags(tag definitions with associated keys for filtering in the agenda views.)
;; The startgroup - endgroup (@XXX) tags are mutually exclusive - selecting one removes 
;;       a similar tag already on the task. 

										; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@company" . ?c)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@hongfu" . ?f)
                            (:endgroup)
                            ("PHONE" . ?p)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("MARK" . ?M)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)


;        ;;===================================================
;        ;; 13 Handling Phone Calls
;        ;;
;        ;;  !! I can't find suitable bbdb packages !! cuiyidong@20130714
;        (require 'bbdb)
;        (require 'bbdb-com)
;        
;        (global-set-key (kbd "<f9> p") 'bh/phone-call)
;        
;        ;;
;        ;; Phone capture template handling with BBDB lookup
;        ;; Adapted from code by Gregory J. Grubbs
;        (defun bh/phone-call ()
;          "Return name and company info for caller from bbdb lookup"
;          (interactive)
;          (let* (name rec caller)
;            (setq name (completing-read "Who is calling? "
;                                        (bbdb-hashtable)
;                                        'bbdb-completion-predicate
;                                        'confirm))
;            (when (> (length name) 0)
;        										; Something was supplied - look it up in bbdb
;              (setq rec
;                    (or (first
;                         (or (bbdb-search (bbdb-records) name nil nil)
;                             (bbdb-search (bbdb-records) nil name nil)))
;                        name)))
;        
;        										; Build the bbdb link if we have a bbdb record, otherwise just return the name
;            (setq caller (cond ((and rec (vectorp rec))
;                                (let ((name (bbdb-record-name rec))
;                                      (company (bbdb-record-company rec)))
;                                  (concat "[[bbdb:"
;                                          name "]["
;                                          name "]]"
;                                          (when company
;                                            (concat " - " company)))))
;                               (rec)
;                               (t "NameOfCaller")))
;            (insert caller)))
;        



;;===================================================
;; 14 GTD Stuff
;;    14.1 Weekly Review Process
;;    To keep the agenda fast, only today's date is shown by default. 
;;    I only need the weekly view during my weekly review and this keeps 
;;    my agenda generation fast.
(setq org-agenda-span 'day)

;;    14.2 Project Definition And Finding Stuck Projects
;;    Any task with a subtask using a todo keyword is a project
;;    Projects are 'stuck' if they have no subtask with a NEXT todo keyword task defined.
;;    The org-mode stuck projects agenda view lists projects that have no NEXT task defined. 
;;    Stuck projects show up on my block agenda and I tend to assign a NEXT task so the 
;;        list remains empty. This helps to keep projects moving forward.
;;    I disable the default org-mode stuck projects agenda view with the following setting.
(setq org-stuck-projects (quote ("" nil nil "")))


;; helper functions defined for projects which are used by agenda views
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((and (bh/is-project-p)
                 (marker-buffer org-agenda-restrict-begin))
            nil)
           ((and (bh/is-project-p)
                 (not (marker-buffer org-agenda-restrict-begin))
                 (not (bh/is-project-subtree-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))



;;===================================================
;; 15 Archiving
;; Setup Archive
;; Tasks can just archive normally to the Archived Tasks heading in the archive file
(setq org-archive-mark-done nil)
(setq org-archive-location "Archive/%s.archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))


;;===================================================
;; 16 Publishing And Exporting
;;
;; 16.1 New Exporter Setup
;;   Alphabetical listing options need to be set before the exporters are loaded for filling to work correctly.
(setq org-alphabetical-lists t)
;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)

;;
;; 16.2 Org-Babel Setup
;;
;; Org-babel makes it easy to generate decent graphics using external packages like ditaa, graphviz, PlantUML, and others.
(setq org-ditaa-jar-path (concat basicPath "java/ditaa0_9.jar") )
(setq org-plantuml-jar-path (concat basicPath "java/plantuml.jar") )

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
; 在org-mode 8.3中，将ob-sh改名为ob-shell了。
; 所以这里不能用(sh . t)，而要改为：
		 (shell . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))


;;
;; 16.7 Publishing Projects
;;
; experimenting with docbook exports - not finished
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
(setq org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")
;
; Inline images in HTML instead of producting links to the image
(setq org-html-inline-images t)
; Do not use sub or superscripts - I currently don't need this functionality in my documents
(setq org-export-with-sub-superscripts nil)
; Use org.css from the norang website for export document stylesheets
(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
(setq org-html-head-include-default-style nil)
; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))
; Export with LaTeX fragments
(setq org-export-with-LaTeX-fragments t)
; Increase default number of headings to export
(setq org-export-headline-levels 6)

; List of projects
; norang       - http://www.norang.ca/
; doc          - http://doc.norang.ca/
; org-mode-doc - http://doc.norang.ca/org-mode.html and associated files
; org          - miscellaneous todo lists for publishing
(setq org-publish-project-alist
;
; http://www.norang.ca/  (norang website)
; norang-org are the org-files that generate the content
; norang-extra are images and css files that need to be included
; norang is the top-level project that gets published
      (quote (("norang-org"
               :base-directory "~/git/www.norang.ca"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
               :recursive t
               :table-of-contents nil
               :base-extension "org"
               :publishing-function org-html-publish-to-html
               :style-include-default nil
               :section-numbers nil
               :table-of-contents nil
               :html-head "<link rel=\"stylesheet\" href=\"norang.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("norang-extra"
               :base-directory "~/git/www.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("norang"
               :components ("norang-org" "norang-extra"))
;
; http://doc.norang.ca/  (norang website)
; doc-org are the org-files that generate the content
; doc-extra are images and css files that need to be included
; doc is the top-level project that gets published
              ("doc-org"
               :base-directory "~/git/doc.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :recursive nil
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html org-org-publish-to-org)
               :style-include-default nil
               :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("doc-extra"
               :base-directory "~/git/doc.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive nil
               :author nil)
              ("doc"
               :components ("doc-org" "doc-extra"))
              ("doc-private-org"
               :base-directory "~/git/doc.norang.ca/private"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
               :recursive nil
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html org-org-publish-to-org)
               :style-include-default nil
               :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :auto-sitemap t
               :sitemap-filename "index.html"
               :sitemap-title "Norang Private Documents"
               :sitemap-style "tree"
               :author-info nil
               :creator-info nil)
              ("doc-private-extra"
               :base-directory "~/git/doc.norang.ca/private"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive nil
               :author nil)
              ("doc-private"
               :components ("doc-private-org" "doc-private-extra"))
;
; Miscellaneous pages for other websites
; org are the org-files that generate the content
              ("org-org"
               :base-directory "~/git/org/"
               :publishing-directory "/ssh:www-data@www:~/org"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function org-html-publish-to-html
               :style-include-default nil
               :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
;
; http://doc.norang.ca/  (norang website)
; org-mode-doc-org this document
; org-mode-doc-extra are images and css files that need to be included
; org-mode-doc is the top-level project that gets published
; This uses the same target directory as the 'doc' project
              ("org-mode-doc-org"
               :base-directory "~/git/org-mode-doc/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html org-org-publish-to-org)
               :plain-source t
               :htmlized-source t
               :style-include-default nil
               :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("org-mode-doc-extra"
               :base-directory "~/git/org-mode-doc/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("org-mode-doc"
               :components ("org-mode-doc-org" "org-mode-doc-extra"))
;
; http://doc.norang.ca/  (norang website)
; org-mode-doc-org this document
; org-mode-doc-extra are images and css files that need to be included
; org-mode-doc is the top-level project that gets published
; This uses the same target directory as the 'doc' project
              ("tmp-org"
               :base-directory "/tmp/publish/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html org-org-publish-to-org)
               :html-head "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />"
               :plain-source t
               :htmlized-source t
               :style-include-default nil
               :auto-sitemap t
               :sitemap-filename "index.html"
               :sitemap-title "Test Publishing Area"
               :sitemap-style "tree"
               :author-info t
               :creator-info t)
              ("tmp-extra"
               :base-directory "/tmp/publish/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("tmp"
               :components ("tmp-org" "tmp-extra")))))

; I'm lazy and don't want to remember the name of the project to publish when I modify
; a file that is part of a project.  So this function saves the file, and publishes
; the project that includes this file
;
; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
(defun bh/save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let ((org-html-head-extra)
        (org-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"))
    (org-publish-current-project force)))

(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

;;
;; 16.8.1 Fontify Latex Listings For Source Blocks
;;
;; For export to latex I use the following setting to get fontified listings from source blocks:
(setq org-latex-listings t)

;;
;; 16.8.2 Export HTML Without XML Header
;;
;; I use the following setting to remove the xml header line for HTML exports. 
;;    This xml line was confusing Open Office when opening the HTML to convert to ODT.

(setq org-html-xml-declaration (quote (("html" . "")
                                       ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                                       ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

;;
;; 16.8.3 Allow Binding Variables On Export Without Confirmation
;;
;; The following setting allows #+BIND: variables to be set on export without confirmation. 
;;    In rare situations where I want to override some org-mode variable for export this
;;     allows exporting the document without a prompt.

(setq org-export-allow-BIND t)


;;===================================================
;; 17 Reminders
;  
;  You can specify exactly how Emacs reminds you of an appointment, 
;  and how far in advance it begins doing so, by setting these variables:
;    
;    appt-message-warning-time
;    The time in minutes before an appointment that the reminder begins. The default is 10 minutes.
;    appt-audible
;    If this is non-nil, Emacs rings the terminal bell for appointment reminders. The default is t.
;    appt-visible
;    If this is non-nil, Emacs displays the appointment message in the echo area. The default is t.
;    appt-display-mode-line
;    If this is non-nil, Emacs displays the number of minutes to the appointment on the mode line. The default is t.
;    appt-msg-window
;    If this is non-nil, Emacs displays the appointment message in another window. The default is t.
;    appt-disp-window-function
;    This variable holds a function to use to create the other window for the appointment message.
;    appt-delete-window-function
;    This variable holds a function to use to get rid of the appointment message window, when its time is up.
;    appt-display-duration
;    The number of seconds to display an appointment message. The default is 5 seconds.

;; 17.1 Reminder Setup
; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (setq appt-display-mode-line t)
  (setq appt-msg-countdown-list '(5 1))
  (setq appt-msg-window nil)
  (org-agenda-to-appt)
)

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)


;;===================================================
;; 18 Productivity Tools
;;
;; 18.1 Abbrev-Mode And Skeletons
;;
;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; Skeletons
;;
;; sblk - Generic block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; splantuml - PlantUML Source block
(define-skeleton skel-org-block-plantuml
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

(define-skeleton skel-org-block-plantuml-activity
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str "-act.png :cache yes :tangle " str "-act.txt\n"
  "@startuml\n"
  "skinparam activity {\n"
  "BackgroundColor<<New>> Cyan\n"
  "}\n\n"
  "title " str " - \n"
  "note left: " str "\n"
  "(*) --> (*)\n"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sact" "" 'skel-org-block-plantuml-activity)

(define-skeleton skel-org-block-plantuml-activity-if
  "Insert a org plantuml block activity if statement"
  "" 
  "if \"\" then\n"
  "  -> [] \"" - _ "\"\n"
  "  --> ==M1==\n"
  "  -left-> ==M2==\n"
  "else\n"
  "end if\n"
  "--> ==M2==")

(define-abbrev org-mode-abbrev-table "sif" "" 'skel-org-block-plantuml-activity-if)

(define-skeleton skel-org-block-plantuml-activity-for
  "Insert a org plantuml block activity for statement"
  "" 
  "--> ==LOOP1==\n"
  "note left: Loop1: For each\n"
  "--> ==ENDLOOP1==\n"
  "note left: Loop1: End for each")

(define-abbrev org-mode-abbrev-table "sfor" "" 'skel-org-block-plantuml-activity-for)

(define-skeleton skel-org-block-plantuml-sequence
  "Insert a org plantuml activity diagram block, querying for filename."
  "File appends (no extension): "
  "#+begin_src plantuml :file " str "-seq.png :cache yes :tangle " str "-seq.txt\n"
  "@startuml\n"
  "title " str " - \n"
  "actor CSR as \"Customer Service Representative\"\n"
  "participant CSMO as \"CSM Online\"\n"
  "participant CSMU as \"CSM Unix\"\n"
  "participant NRIS\n"
  "actor Customer"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sseq" "" 'skel-org-block-plantuml-sequence)

;; sdot - Graphviz DOT block
(define-skeleton skel-org-block-dot
  "Insert a org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
  "graph G {\n"
  _ - \n
  "}\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; sditaa - Ditaa source block
(define-skeleton skel-org-block-ditaa
  "Insert a org ditaa block, querying for filename."
  "File (no extension): "
  "#+begin_src ditaa :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; selisp - Emacs Lisp source block
(define-skeleton skel-org-block-elisp
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)



;;
;; 18.2 Focusing on current work
;;
;; 18.2.1 Narrowing to a subtree with Bh/Org-Todo
;;
;;  f5 and S-f5 are bound the functions for narrowing and widening the emacs 
;;    buffer as defined below.
;;  We now use:
;; 
;; T (tasks) for C-c / t on the current buffer
;; N (narrow) narrows to this task subtree
;; U (up) narrows to the immediate parent task subtree without moving
;; P (project) narrows to the parent project subtree without moving
;; F (file) narrows to the current file or file of the existing restriction
;; 
(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree)
          (save-restriction
            (org-agenda-set-restriction-lock)))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
        (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/current-view-project nil)

(defun bh/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (re-search-forward "Tasks to Refile")
    (setq bh/current-view-project (point)))
  (bh/widen)
  (when org-agenda-sticky
    (org-agenda-redo))
  (goto-char bh/current-view-project)
  (forward-visible-line 1)
  (while (and (< (point) (point-max))
              (or (not (org-get-at-bol 'org-hd-marker))
                  (org-with-point-at (org-get-at-bol 'org-hd-marker)
                    (or (not (bh/is-project-p))
                        (bh/is-project-subtree-p)))))
    (forward-visible-line 1))
  (setq bh/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
      (progn
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project)
        (org-agenda-redo)
        (beginning-of-buffer))
    (beginning-of-buffer)
    (setq bh/hide-scheduled-and-waiting-next-tasks t)
    (error "All projects viewed.")))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

;;
;; the following setting to force showing the next headline.
;;
(setq org-show-entry-below (quote ((default))))


;;
;; 18.2.2 Limiting The Agenda To A Subtree
;;
;; C-c C-x < turns on the agenda restriction lock for the current subtree. This 
;;   keeps your agenda focused on only this subtree. Alarms and notifications are 
;;   still active outside the agenda restriction. C-c C-x > turns off the agenda 
;;   restriction lock returning your agenda view back to normal.
(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;;
;; the following setting to keep the highlight on the heading only (as was the 
;;   case for pre-8.0 versions of org-mode)
;;
;; Limit restriction lock highlighting to the headline only
(setq org-agenda-restriction-lock-highlight-subtree nil)



;;
;; 18.3.1 Highlight The Current Agenda Line
;;
;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; The following custom-set-faces create the highlights
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))


;;
;; 18.3.2 Keep Tasks With Timestamps Visible On The Global Todo Lists
;;
;; The block agenda prevents display of tasks with deadlines or scheduled dates 
;;   in the future so you can safely ignore these until the appropriate time.

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;;
;; 18.3.3 Use The Diary For Holidays And Appointments
;;
;; I don't use the emacs Diary for anything but I like seeing the holidays on my
;;   agenda. This helps with planning for those days when you're not supposed to be working.
(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file org-my-diary-file)

;; I use the following setting so any time strings in the heading are shown in the agenda.
(setq org-agenda-insert-diary-extract-time t)

;;
;; 18.3.4 Searches Include Archive Files
;;
;; I keep a single archive file for each of my org-mode project files. This allows 
;;   me to search the current file and the archive when I need to dig up old information 
;;   from the archives.
;; I don't need this often but it sure is handy on the occasions that I do need it.
;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))


;;
;; 18.3.5 Agenda View Tweaks
;;
;; The following agenda customizations control
;;   display of repeating tasks
;;   display of empty dates on the agenda
;;   task sort order
;;   start the agenda weekly view with Sunday
;;   display of the grid
;;   habits at the bottom
;;
;; I use a custom sorting function so that my daily agenda lists tasks in order 
;;   of importance. Tasks on the daily agenda are listed in the following order:
;;
;;   tasks with times at the top so they are hard to miss
;;   entries for today (active timestamp headlines that are not scheduled or deadline tasks)
;;   deadlines due today
;;   late deadline tasks
;;   scheduled items for today
;;   pending deadlines (due soon)
;;   late scheduled items
;;   habits

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
              (todo category-up priority-down effort-up)
              (tags category-up priority-down effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
										; time specific items are already sorted first by org-agenda-sorting-strategy

										; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

										; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

										; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

										; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

										; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

										; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

										; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
										; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
										; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
										; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
										; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))


;;
;; 18.3.6 Sticky Agendas
;;
;; Sticky agendas allow you to have more than one agenda view created simultaneously. 
;;   You can quickly switch to the view without incurring an agenda rebuild by 
;;   invoking the agenda custom command key that normally generates the agenda. 
;;   If it already exists it will display the existing view. g forces regeneration 
;;   of the agenda view.
;;
;; I normally have two views displayed (F12 a for the daily/weekly agenda and 
;;   F12 SPC for my project management view)

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)

;;
;; 18.3.7 Q Buries The Agenda View Buffer
;;
;; I change the q key in the agenda so instead of killing the agenda buffer it 
;;   merely buries it to the end of the buffer list. This allows me to pull it
;;   back up quickly with the q speed key or f9 f9 and regenerate the results with g.
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map "q" 'bury-buffer))
          'append)

;;
;; 18.4 Checklist Handling
;; 
;; (add-to-list 'load-path (expand-file-name "~/git/org-mode/contrib/lisp"))
(require 'org-checklist)
;; then to use it in a task you simply set the property RESET_CHECK_BOXES to t
;;   * TODO Invoicing and Archive Tasks [0/7]
;;   DEADLINE: <2009-07-01 Wed +1m -0d> 
;;   PROPERTIES:
;;   RESET_CHECK_BOXES: t
;;   END:
;;   
;;   - [ ] Do task 1
;;   - [ ] Do task 2
;;   ...
;;   - [ ] Do task 7


;;
;; 18.6 Handling Blocked Tasks
;;
;; Blocked tasks are tasks that have subtasks which are not in a done todo state. 
;;   Blocked tasks show up in a grayed font by default in the agenda.
;;
;; To enable task blocking set the following variable:
(setq org-enforce-todo-dependencies t)
;; This setting prevents tasks from changing to DONE if any subtasks are still
;;   open. This works pretty well except for repeating tasks. I find I'm regularly
;;   adding TODO tasks under repeating tasks and not all of the subtasks need to
;;   be complete before the next repeat cycle.
;;
;; You can override the setting temporarily by changing the task with C-u C-u C-u C-c C-t 
;;   but I never remember that. I set a permanent property on the repeated tasks as follows:
;;      
;;      * TODO New Repeating Task
;;      SCHEDULED: <2009-06-16 Tue +1w>
;;      PROPERTIES:
;;      NOBLOCKING: t
;;      END:
;;      ...
;;      ** TODO Subtask
;;
;; This prevents the New Repeating Task from being blocked if some of the items 
;;   under it are not complete.
;;
;; Occassionally I need to complete tasks in a given order. Org-mode has a property
;;   ORDERED that enforces this for subtasks.
;;      
;;      * TODO Some Task
;;      PROPERTY:
;;      ORDERED: t
;;      END:
;;      ** TODO Step 1
;;      ** TODO Step 2
;;      ** TODO Step 3
;; In this case you need to complete Step 1 before you can complete Step 2, etc. 
;;   and org-mode prevents the state change to a done task until the preceding
;;   tasks are complete.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 18.7 Org Task Structure And Presentation
;; 只高亮显示最后一个代表层级的 *
;; 高亮显示所有的 *
(setq org-hide-leading-stars nil)

;;
;; 18.7.2 Org-Indent Mode
;;
;; It removes the indentation in the org-file but displays it as if it was indented while you are working on the org file buffer.
;;
(setq org-startup-indented t)

;;
;; 18.7.3 Handling Blank Lines
;;
;; The following setting prevents creating blank lines before headings but allows list items to adapt to existing blank lines around the items:
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

;;
;; 18.7.4 Adding New Tasks Quickly Without Disturbing The Current Task Content
;;
;; To create new headings in a project file it is really convenient to use C-RET, C-S-RET, M-RET, and M-S-RET. This inserts a new headline possibly with a TODO keyword. With the following setting

(setq org-insert-heading-respect-content nil)

;;
;; 18.7.5 Notes At The Top
;;
;; I enter notes for tasks with C-c C-z (or just z in the agenda). Changing tasks states also sometimes prompt for a note (e.g. moving to WAITING prompts for a note and I enter a reason for why it is waiting). These notes are saved at the top of the task so unfolding the task shows the note first.

(setq org-reverse-note-order nil)


;;
;; 18.7.6 Searching And Showing Results
;;
;; Org-mode's searching capabilities are really effective at finding data in your org files. C-c / / does a regular expression search on the current file and shows matching results in a collapsed view of the org-file.
;;
;; I have org-mode show the hierarchy of tasks above the matched entries and also the immediately following sibling task (but not all siblings) with the following settings:

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

;;
;; 18.7.7 Editing And Special Key Handling
;; 
;; Org-mode allows special handling of the C-a, C-e, and C-k keys while editing headlines. I also use the setting that pastes (yanks) subtrees and adjusts the levels to match the task I am pasting to. See the docstring (C-h v org-yank-adjust-subtrees) for more details on each variable and what it does.
;; 
;; I have org-special-ctrl-a/e set to enable easy access to the beginning and end of headlines. I use M-m or C-a C-a to get to the beginning of the line so the speed commands work and C-a to give easy access to the beginning of the heading text when I need that.

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

;;
;; 18.9 Deadlines And Agenda Visibility
;;
;; Deadlines and due dates are a fact or life. By default I want to see deadlines in the agenda 30 days before the due date.
(setq org-deadline-warning-days 30)
;; This gives me plenty of time to deal with the task so that it is completed on or before the due date.
;; I also use deadlines for repeating tasks. If the task repeats more often than once per month it would be always bugging me on the agenda view. For these types of tasks I set an explicit deadline warning date as follows:
;;
;;    * TODO Pay Wages
;;    DEADLINE: <2009-07-01 Wed +1m -0d>
;;
;; This example repeats monthly and shows up in the agenda on the day it is due (with no prior warning). You can set any number of lead days you want on DEADLINES using -Nd where N is the number of days in advance the task should show up in the agenda. If no value is specified the default org-deadline-warning-days is used.

;;
;; 18.10 Exporting Tables To CSV
;;
;; Org-mode can export tables as TAB or comma delimited formats. I set the default format to CSV with:
(setq org-table-export-default-format "orgtbl-to-csv")
;; hit M-x org-table-export which prompts for a filename and the format which defaults to orgtbl-to-csv from the setting above.

;;
;; 18.11 Minimize Emacs Frames
;;
;; Links to emails, web pages, and other files are sprinkled all over my org files. The following setting control how org-mode handles opening the link.
(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))

; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

;;
;;18.12 Logging Stuff
;;
;; Most of my logging is controlled by the global org-todo-keywords
(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)


;;
;;18.13 Limiting Time Spent On Tasks
;;
;;Org-mode has this great new feature for signalling alarms when the estimated time for a task is reached. I use this to limit the amount of time I spend on a task during the day.
;;
;;As an example, I've been working on this document for over two months now. I want to get it finished but I can't just work on it solely until it's done because then nothing else gets done. I want to do a little bit every day but limit the total amount of time I spend documenting org-mode to an hour a day.
;;
;; To this end I have a task
;;      
;;      * NEXT Document my use of org-mode
;;      LOGBOOK:...
;;      PROPERTIES:
;;      CLOCK_MODELINE_TOTAL: today
;;      Effort:   1:00
;;      END:
;; 
;; The task has an estimated effort of 1 hour and when I clock in the task it gives me a total in the mode-line like this
;; 
;; --:**  org-mode.org   91% (2348,73) Git:master  (Org Fly yas Font)-----[0:35/1:00 (Document my use of org-mode)]-------
;; I've spent 35 minutes of my 1 hour so far today on this document and other help on IRC.
;; 
;; I set up an alarm so the Star Trek door chime goes off when the total estimated time is hit. (Yes I'm a Trekkie :) )
;; 
(setq org-clock-sound (concat basicPath "dingdang.wav") )

;;
;; 18.14 Habit Tracking
;;
;; A habit is just like a regular task except it has a special PROPERTY value setting and a special SCHEDULED date entry like this:
;;      
;;      * TODO Update Org Mode Doc
;;      SCHEDULED: <2009-11-21 Sat .+7d/30d>
;;      [2009-11-14 Sat 11:45]
;;      PROPERTIES:
;;      STYLE: habit
;;      END:
;; 
;; This marks the task as a habit and separates it from the regular task display on the agenda. When you mark a habit done it shows up on your daily agenda the next time based on the first interval in the SCHEDULED entry (.+7d)

; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb
                          org-bibtex
                          org-crypt
                          org-gnus
                          org-id
                          org-info
                          org-jsinfo
                          org-habit
                          org-inlinetask
                          org-irc
                          org-mew
                          org-mhe
                          org-protocol
                          org-rmail
                          org-vm
                          org-wl
                          org-w3m)))

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

;; During the day I'll turn off the habit display in the agenda with K. This is a persistent setting and since I leave my Emacs running for days at a time my habit display doesn't come back. To make sure I look at the habits daily I have the following settings to redisplay the habits in the agenda each day. This turns the habit display on again at 6AM each morning.
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

;;
;; 18.21 Insert Inactive Timestamps And Exclude From Export
;;
;; automatically insert the inactive timestamp whenever a headline is created.
;;   the timestamp can be controlled by f9 T which toggles the creation of the timestamp on and off for new headlines.

(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

;; To prevent the timestamps from being exported in documents I use the following setting

(setq org-export-with-timestamps nil)

;;
;; 18.23 Highlight Clock When Running Overtime
;;
;; The current clocking task is displayed on the modeline. If this has an estimated time and we run over the limit I make this stand out on the modeline by changing the background to red as follows

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))


;;
;; 18.24 Meeting Notes
;;
;; Here is the formatting function. Just highlight the region for the notes and it turns tabs into spaces, and highlights todo items. The resulting notes are in the kill buffer ready to paste to another application.

(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))


;;
;; 18.25 Remove Highlights After Changes
;;
;; I'm finding I use org-occur C-c / / a lot when trying to find details in my org-files. The following setting keeps the highlighted results of the search even after modifying the text. This allows me to edit the file without having to reissue the org-occur command to find the other matches in my file. C-c C-c removes the highlights.
(setq org-remove-highlights-with-change nil)
;; Setting this variable to t will automatically remove the yellow highlights as soon as the buffer is modified.
;;
;; I've gone back to automatically removing the highlights with change which is the default setting. I've been using regular M-x occur a lot more lately to find things in any Emacs buffer.
(setq org-remove-highlights-with-change t)


;;
;; 18.28 Automatically Change List Bullets
;;
;; !! I do NOT need this change !!
;      (setq org-list-demote-modify-bullet (quote (("+" . "-")
;                                                  ("*" . "-")
;                                                  ("1." . "-")
;                                                  ("1)" . "-"))))
;      


;;
;; 18.29 Remove Indentation On Agenda Tags View
;;
;; To make all of the matched headings for a tag show at the same level in the agenda set the following variable:
(setq org-tags-match-list-sublevels t)

;;  
;; 18.31 Agenda Persistent Filters
;;  
;; This is a great feature! Persistent agenda filters means if you limit a search with / TAB SomeTag the agenda remembers this filter until you change it.
;;  
;; Enable persistent filters with the following variable
;;  
;  (setq org-agenda-persistent-filter t)
  
;;
;; 18.38 Remove Multiple State Change Log Details From The Agenda
;;
;; I skip multiple timestamps for the same entry in the agenda view with the following setting.
(setq org-agenda-skip-additional-timestamps-same-entry t)


;;
;; 18.41 Use The Current Window For The Agenda
;;
; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)


;;
;; 18.43 Cycling Plain Lists
;;
;; Org mode can fold (cycle) plain lists.

(setq org-cycle-include-plain-lists t)


;;
;; 18.46 NEXT Is For Tasks
;;
;; NEXT keywords are for tasks and not projects. I've added a function to the todo state change hook and clock in hook so that any parent tasks marked NEXT automagically change from NEXT to TODO since they are now projects and not tasks.

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)


;;
;; 18.47 Startup In Folded View
;;
(setq org-startup-folded t)

;;
;; 18.53 Prevent Editing Invisible Text
;;
;; The following setting prevents accidentally editing hidden text when the point 
;;   is inside a folded region. This can happen if you are in the body of a 
;;   heading and globally fold the org-file with S-TAB
;;
;; This setting prevents me to add a TODO item below the current one, so it is 
;;   disabled.  by  cuiyidong@20130715
; (setq org-catch-invisible-edits 'error)



;;===================================================
;; 在 agenda/remember模式时，emacs会自动打开新的frame
;; 为了避免打开新的frame，尝试如下代码
;;(defun make-frame (&optional ignore) 
;;(selected-frame)) 
(setq ns-pop-up-frames nil)
(put 'scroll-left 'disabled nil)
