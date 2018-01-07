;;
;; <my-commons> layer config file
;; path configuration: setting up the general paths
;; Cui Yidong <nathan.cui@gmail.com>
;; Rev 20160521,20160730,20161218
;;

;; -----------------------------------------------------------------------------
;; Setup all the necessary directories
;; The following directories are all global variables
  ;; 为Windows下的emacs增加加载路径
  ;; setq 序列 (concat 序列 " " (int-to-string 变))

(if (eq system-type 'windows-nt)
    (defconst myDocument "D:/MyDocument/"))

(if (eq system-type 'cygwin)
    (defconst myDocument "/cygdrive/d/MyDocument/"))

(if (eq system-type 'gnu/linux)
  	(defconst myDocument "~/Documents/"
      "Dir where I usually work in Linux"))

;; This is a private dir. All personal modifications and settings are stored
;; here. All personal configuration-layers are also stored here. All layers are
;; stored in subdirs
(defconst basicPath "~/.spacemacs.private/"
	"Basic path for any customized dirs in emacs which will be version controlled with git" )

(defconst audioPath (concat basicPath "audio/")
  "Path to store audio files used in emacs")

(defconst varPath (concat basicPath "var/")
  "Path for run-time storage" )

(if (eq system-type 'windows-nt)
    (defconst cygwin-root-path "c:/cygwin/"
      "root dir of cygwin" ))

(if (eq system-type 'windows-nt)
    (defconst orgPath (concat myDocument "99.Org/")
      "Path for all .org files under windows" ))

(if (eq system-type 'gnu/linux)
    (defconst orgPath (concat myDocument "99.org/")
      "Path for all .org files under linux" ))

;; dropbox 是用来做pc与手机端的 mobile org mode 内容同步的
;;
;; !!!NOTE!!!
;;
;; There is already a dropbox path defined in ~/.emacs.d/core/core-load-paths.el
;; (defconst user-dropbox-directory
;;   (expand-file-name (concat user-home-directory "Dropbox/"))
;;   "Dropbox directory.")
;;
;; Although "user-dropbox-directory" is defined as a "const", we acturally can
;; change its value by setq. ==> https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html
(if (eq system-type 'windows-nt)
    (setq user-dropbox-directory
          (expand-file-name (concat myDocument "95.Dropbox/"))))

(if (eq system-type 'gnu/linux)
    (setq user-dropbox-directory
          (expand-file-name (concat myDocument "95.Dropbox/"))))

;; Set the default dir when using C-c C-f to open files
(setq default-directory myDocument)


;; “日记”存放的位置
(setq diary-file (concat basicPath "diary"))
