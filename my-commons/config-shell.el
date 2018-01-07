;;
;; <my-commons> layer config file
;; Shell configuration: setting up the shell environment
;; Cui Yidong <nathan.cui@gmail.com>
;; Rev: 20160730
;;

;; ==================================================
;; Change the default shell from cmd.exe to cygwin
;; (setq shell-file-name "C:/cygwin/bin/zsh.exe")
;; "C:/cygwin/bin/zsh.exe")

(when (eq system-type 'windows-nt)
  (progn
    	(setq explicit-shell-file-name "C:/cygwin/bin/zsh.exe")
    	(setq shell-file-name "zsh")
    ;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
    	(setenv "SHELL" shell-file-name)
    ;; remove the input Ctrl-M character
    	(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
    	(add-to-list 'process-coding-system-alist
                 '("zsh" . (undecided-dos . undecided-unix)))
  )
)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t) 
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
