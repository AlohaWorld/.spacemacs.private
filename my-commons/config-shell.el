;;
;; <my-commons> layer config file
;; Shell configuration: setting up the shell environment
;; Cui Yidong <nathan.cui@gmail.com>
;; Rev: 20160730, 20180609


;; ==================================================
;; Change the default shell from cmd.exe to msys2
;; (setq shell-file-name "C:/msys64/usr/bin/zsh.exe")

(when (eq system-type 'windows-nt) 
  (if (file-exists-p "C:/msys64/usr/bin/zsh.exe")
    (progn
      (setq explicit-shell-file-name "C:/msys64/usr/bin/zsh.exe")
      (setq shell-file-name "zsh")
      ;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
      (setenv "SHELL" shell-file-name)
      ;; remove the input Ctrl-M character
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
      (add-to-list 'process-coding-system-alist
        '("zsh" . (undecided-dos . undecided-unix)))
    )
    (message "Can not find C:/msys64/usr/bin/zsh.exe. Keep the shell as-is(cmd.exe)")
  )
)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t) 
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
