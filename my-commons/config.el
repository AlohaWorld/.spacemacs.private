;;
;; <my-commons> layer config file
;; main configuration: miscellaneous configuration
;; Cui Yidong
;; Rev: 20160730
;;

;; In order to load el files in current directory, we need add current dir to
;; load-path
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;; Setup personal information
(setq user-full-name "Cui Yidong")
(setq user-mail-address "nathan.cui@gmail.com")

;; Setup altitude and logitude. Then we can get sunrise/sunset time in calendar
(setq calendar-latitude 39.55)          ;; Altitude, positive number ==> north
(setq calendar-longitude 116.25)        ;; Longitude, positive number ==> east
(setq calendar-location-name "Beijing") ;; City name

;; Setup Coding page
;; To solve "Some unexpected code \241\256 in Interactive-Haskell mode"
;; https://github.com/haskell/haskell-mode/issues/400
(setq default-process-coding-system '(cp936-dos . utf-8-unix))

;; Setup the general paths
(load "config-path.el")



;; Setup the shell (cygwin zsh)
(load "config-shell.el")


;; Configuration about editing/viewing environment
;; cyd@20160739
(load "config-editview.el")


;; github integration
(setq paradox-github-token "3b94e043f45d61a8f2590bb63bda99e2a0dcb601")
