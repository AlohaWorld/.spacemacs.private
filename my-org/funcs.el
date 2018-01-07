;;; funcs.el --- General utility functions for my-org layer
;;
;; Copyright (c) 2014-2016 Yidong Cui & Contributors
;;
;; Author: Yidong Cui <nathan.cui@gmail.com>
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Because of autoloading, calling to =org= functions will trigger the loading up
;; of the =org= shipped with emacs wich will induce conflicts.
;; One way to avoid conflict is to wrap your =org= config code in a
;; =with-eval-after-load= block like this:
(with-eval-after-load 'org

  ;; ======= Functions for keybindings =========================================
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


  ;; ===== Functions for dealing with tasks
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


  ;; 为org模式提供额外小窗口显示大纲，并且避免行的自动截短
  ;; The following hook function should be hooked to org-mode-hook
  ;; Please see package.el to find the call to "add-hook"
  (defun my-org-toc-show-hook()
    (interactive)
    (local-set-key (kbd "M-o") 'org-toc-show)
    (setq truncate-lines nil)
    )

  ;; This is the end of "with-eval-after-load"
  ;; After this comment, do not write any codes
  )
