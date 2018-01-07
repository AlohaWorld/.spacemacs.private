;; sdcv是一个命令行工具，在控制台可以查startDict安装好的词典，支持交互模式，也支持 命令行传参数。

;; linux下安装sdcv很简单，windows下可要自己编译，有点麻烦，这里有一份编译好的cygwin
;; 版本(http://code.google.com/p/bamanzi-misc/downloads/list?q=label:cygwin)。先安
;; 装startDict，从这里下载(http://www.stardict.org/)，startDict词典不好下载，这里提
;; 供了一个朗道英汉5.0(http://files.cnblogs.com/machine/dic.rar)，将解压后的文件
;; 夹放到 /cygwin/usr/share/stardict/dic下。

;; 然后建立一个sdcv.bat，确保在PATH变量中会被Emacs找到(可以放到emacs的bin目录下面)。

;; @echooff
;; set PATH=e:\cygwin\bin;c:\cygwin\usr\bin
;; rem cygwin-1.7 use utf-8 as default locale, if nothing (LANG/LC_xxx) set
;; rem but Emacs would set LANG according system active codepage
;; set LANG=
;; sdcv.exe %*

;; 编写elsip程序：
;; author: pluskid
;; 调用 stardict 的命令行程序 sdcv 来查辞典
;; 如果选中了 region 就查询 region 的内容，否则查询当前光标所在的单词
;; 查询结果在一个叫做 *sdcv* 的 buffer 里面显示出来，在这个 buffer 里面
;; 按 q 可以把这个 buffer 放到 buffer 列表末尾，按 d 可以查询单词

(global-set-key (kbd "C-c d") 'kid-sdcv-to-buffer)
(defun kid-sdcv-to-buffer ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "Search the dictionary for (default %s): " word)
                            nil nil word))
    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)
    (let ((process (start-process-shell-command "sdcv.bat" "*sdcv*" "sdcv" "-n" word)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (string= (buffer-name) "*sdcv*")
             (setq kid-sdcv-window-configuration (current-window-configuration))
             (switch-to-buffer-other-window "*sdcv*")
             (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
             (local-set-key (kbd "q") (lambda ()
                                        (interactive)
                                        (bury-buffer)
                                        (unless (null (cdr (window-list))) ; only one window
                                          (delete-window)))))
           (goto-char (point-min))))))))
