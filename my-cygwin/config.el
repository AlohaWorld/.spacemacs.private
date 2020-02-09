;;
;; <my-cygwin> layer config file
;; main configuration: miscellaneous configuration
;; Cui Yidong
;; Rev: 20180609
;;

;; In order to load el files in current directory, we need add current dir to
;; load-path
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;; Under windows system, we set cygwin's root path, in case we use the programs 
;;    in cygwin
(if (eq system-type 'windows-nt)
    (defconst cygwin-root-path "c:/cygwin/"
      "root dir of cygwin" ))

      
;; ==================================================
;; Change the default shell from cmd.exe to cygwin zsh
;; (setq shell-file-name "C:/cygwin/bin/zsh.exe")

(when (eq system-type 'windows-nt) 
  (if (file-exists-p "C:/cygwin/bin/zsh.exe")
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
    (message "Can not find C:/cygwin/bin/zsh.exe. Keep the shell as-is(cmd.exe)")
  )
)

