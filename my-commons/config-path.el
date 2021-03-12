;;
;; <my-commons> layer config file
;; path configuration: setting up the general paths
;; Cui Yidong <nathan.cui@gmail.com>
;; Rev 20160521,20160730,20161218,20181112
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
    (defconst orgPath (concat myDocument "99.Org/")
      "Path for all .org files under windows" ))

(if (eq system-type 'gnu/linux)
    (defconst orgPath (concat myDocument "99.org/")
      "Path for all .org files under linux" ))


;; The location for "diary" file
;; More details: https://www.gnu.org/software/emacs/manual/html_node/emacs/Diary.html#Diary
;; The Emacs diary keeps track of appointments or other events on a daily basis,
;; in conjunction with the calendar. To use the diary feature, you must first
;; create a diary file containing a list of events and their dates. Then Emacs
;; can automatically pick out and display the events for today, for the immediate
;; future, or for any specified date.
;;
;; cyd@20181112 We already have set diary-file to "org-my-diary-file" in layer my-org/config.el
;; Please check 18.3.3 in my-org/config.el
;;
;; (setq diary-file (concat basicPath "diary/diary.txt"))

;; The directory that appears in the prompt for C-x C-f ('find-file') comes from
;;   the value of default-directory, which is a buffer-local variable. When you
;;   first start Emacs, the initial buffer displayed is the GNU Emacs buffer.
;; That buffer's default-directory is set from the variable command-line-default-directory.
(setq command-line-default-directory myDocument )
;; Set the default dir when using C-x C-f to open files
;; 2018/01/07 Seems this configuration does not work. see above comments
;; Because the variable "default-directory" is a buffer-local variables
(setq default-directory myDocument)

;; Another way to set default start directory for C-x C-f is to start emacs with a parameter
;; You can start Emacs in a given directory, by passing that directory on the command line.
;; You can use a Windows shortcut to do this too. And you can have the shortcut visit that directory in Dired.
;;    Example shortcut info:
;;    Target: C:\Emacs\bin\runemacs.exe "C:\my\favorite\folder"
;;    Start in: C:\my\favorite\folder

;; dropbox directory
(setq user-dropbox-directory (concat orgPath "dropbox/"))
