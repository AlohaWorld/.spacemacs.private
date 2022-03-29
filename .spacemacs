;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.private/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     better-defaults
     helm
     (auto-completion :variables
                      ;; auto-completion-front-end 'auto-complete
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory  (concat (car dotspacemacs-configuration-layer-path) "snippets/")
                      auto-completion-enable-sort-by-usage t
      )
     multiple-cursors
     treemacs
     ;; Development
     (lsp :variables
          lsp-restart 'auto-restart
          lsp-lens-enable t)
     (c-c++ :variables
            c-c++-backend 'lsp-clangd
            lsp-clients-clangd-executable "c:/Program Files/LLVM/bin/clangd.exe"
            c-c++-lsp-enable-semantic-highlight 'rainbow
            ;; When semantic highlighting is enabled then the method that's used
            ;; to highlight the text relies on Emacs' fastest =font-lock= mechanism.
            ;; A more accurate but also slower method is to use overlays.
            c-c++-lsp-semantic-highlight-method 'overlay
            ;; Debugger(dap integration)
            c-c++-dap-adapters '(dap-lldb dap-cpptools)
            c-c++-adopt-subprojects t
            ;; To organize the file header includes on save
            c-c++-enable-organize-includes-on-save nil
            ;; [[http://clang.llvm.org/docs/ClangFormat.html][clang-format]] allows
            ;; for reformatting either a selected region of code
            ;; (=clang-format-region=) or a whole buffer (=clang-format-buffer=)
            ;; according to a style defined in a =.clang-format= file. This file is
            ;; either located in the same as the file being edited, or in any of its
            ;; parent directories. If no =.clang-format= file is found, then a default
            ;; style will be used.
            c-c++-enable-clang-format-on-save nil
            ;; If we do NOT need ClangFormat, then we need google-style
            c-c++-enable-google-style t
            c-c++-enable-google-newline t
            ;;  Automatically adds newlines after certain characters
            c-c++-enable-auto-newline t
            ;; To prevent projectile from using subproject root when visiting
            ;; files in a subproject, set c-c++-adopt-subprojects to t.
            c-c++-adopt-subprojects nil
            )
     cmake
     emacs-lisp
     autohotkey
     (plantuml :variables
               plantuml-jar-path (concat (car dotspacemacs-configuration-layer-path)
                                         "java/plantuml-1.2021.16.jar")
               org-plantuml-jar-path (concat (car dotspacemacs-configuration-layer-path)
                                             "java/plantuml-1.2021.16.jar")
               )
     ;git
     ;; spell & syntax checking
     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-tooltips t)

     ;; Markdown needs an engine to render md files. We use pandoc
     (markdown :variables markdown-command "pandoc")

     ;; org-mode setup
     (org :variables
          ;; 设置变量org-agenda-files让Org-Mode知道在哪些文件搜寻TODO
          ;; Push all .org files in the path to the file list
          ;; org-agenda-files (directory-files-recursively orgPath "\\.org$")
          ;; org-agenda-files (directory-files orgPath t "\\.org$")
          org-agenda-files (list (concat orgPath "TODOs.org")
                                 (concat orgPath "refile.org"))


          org-id-link-to-org-use-id t ;Create ID to make link
          org-enable-github-support nil; with t, emacs fork a process to do git sync which consumes cpu
          ;; Enforce dependencies in TODO hierarchy
          ;; org-todo-dependencies-strategy 'naive-auto

          ;; Enable notifications for agenda events
          org-enable-notifications t
          org-start-notification-daemon-on-startup t

          ;; ---------------------------------------------------------------------------
          ;; Org-journal support
          org-enable-org-journal-support t
          ;; By default, journal files are stored in =~/Documents/journal/=
          org-journal-dir (concat orgPath "journal/")
          ;;To change the journal file name format. Note: do NOT append an extend filename
          org-journal-file-format "%Y-%m-%d"
          ;;to have new journal files created with following header
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%Y/%m/%d, %A"
          ;; The default entry is a second level heading (=** =) followed by a
          ;; timestamp. If you start your journal files with a Title as shown
          ;; above you may want to adjust entries to start at the first level
          ;; heading and you may want to change or omit timestamp.
          org-journal-time-prefix "* "
          org-journal-time-format ""

          ;;  generates [[https://gohugo.io][Hugo]] -compatible Markdown
          org-enable-hugo-support t

          ;; To set filename where you want to store project-specific TODOs
          ;; If this is an absolute path, all todos will be stored in the same
          ;; file (organized by project)
          org-projectile-file (concat orgPath "TODOs.org")

          ;; Enable org-brain support
          org-enable-org-brain-suppor t
          org-brain-path (concat orgPath "brain")

          ;; Enable org sticky header support
          org-enable-sticky-header t

          ;; Toggles visibility of emphasis markers, links, subscripts, and
          ;; superscripts in org mod
          org-enable-appear-support t

          org-enable-reveal-js-support t
          org-re-reveal-root "file:///D://MyDocument/99.Org/reveal.js"

          ;; This will replace the general diary-file settings
          org-my-diary-file (concat orgPath "diary.org")

          ;; Fontify code in code blocks
          org-src-fontify-natively t

          :bind (:map spacemacs-org-mode-map-root-map ("M-RET" . nil)))

     ;; html is required by org-export
     html
     ;; pdf, which needs pdf-tools
     pdf
     ;; latex needs texlive software. Install independent texlive software or msys2
     ;; texlive package. e.g. pacman -Ss texlive and then install related packages
     ;; mingw-w64-x86_64-texlive
     ;; core, latex-recommended, latex-extra, extra-util, bibtex-extra, plain-generic, lang-chinese, science
     ;; to support cjk fonts in latex, add the following lines in tex file
     ;; \usepackage{xeCJK}
     ;; \setCJKmainfont{微软雅黑}
     (latex :variables
            ;; When writing latex documents, the backend support syntax completion
            ;; Currently, the LaTeX LSP backend depends on TexLab
            latex-backend 'lsp
            ;; To get latexmk, under msys2: pacman -S mingw-w64-x86_64-texlive-extra-utils
            latex-build-command 'latexmk
            ;; latex-build-command "LaTeX" ; msys2 LaTeX == pdflatex
            latex-build-engine 'xetex
            ;; To update the preview buffer whenever the compiled PDF file changes
            latex-refresh-preview t
            ;; make =pdf-tools= open in a split window
            latex-view-pdf-in-split-window nil
            ;; prefer another pdf viewer to preview
            latex-view-with-pdf-tools nil
            latex-enable-folding t)
     ;; shell
     (shell :variables
             shell-default-height 30
             shell-default-position 'bottom)

     ;; Fonts
     (unicode-fonts :variables unicode-fonts-enable-ligatures t)

     ;; theme
     themes-megapack
     theming

     ;; chinese input method
     (chinese :variables
              chinese-default-input-method 'pinyin
              chinese-enable-fcitx nil)

     ;; email processing
     gnus
     notmuch

     ;; Paper writing/reference manager, with ebib support
     ;(bibtex :variables
     ;        bibtex-enable-ebib-support t
     ;        ebib-preload-bib-files '("file:///D://MyDocument/99.Org/ebib/myref.bib")
             ; ebib-file-search-dirs '("/path-to-documents-directory")
             ; ebib-import-directory "path-to-e.g.-download-directory"
     ;        )
     ;; My personal layers
     my-commons
     my-buffer
     (my-org :variables
             org-enable-latex-support t
             )
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                )
                               ; (projects . 5)
                               ; (agenda . 5)
                               ; (todos . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark-high-contrast
                         monokai
                         doom-solarized-dark
                         darkburn
                         cyberpunk
                         spacemacs-dark
                         spacemacs-light
                         spolsky
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 0.6)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '(
                               ;;("Fira Code Retina"
                               ("Cascadia Code"
                               :size 13.0
                               :weight semi-light
                               :width normal
                               :powerline-scale 0.8)
                               )
   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   ;; dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")
   ;; repair M-RET keybindings in spacemacs
   ;; We want M-RET to be a new list item action instead of menu leader key.
   ;;dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names t

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil

   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize t

   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'right-then-bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 80

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil)

  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
          ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ("nongnu"   . "https://elpa.nongnu.org/nongnu/")
          ))
  (setq-default package-archives configuration-layer-elpa-archives)

  ;; We do NOT check package signature for key errors
  (setq package-check-signature nil)

  ) ;; end defun dotspacemacs/init ()

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; Setup proxy
   ;; (setq url-proxy-services
   ;;    '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
   ;;      ("http" . "127.0.0.1:20080")
   ;;      ("https" . "127.0.0.1:20080")))

  ;; Setup modifications for themes
  (setq theming-modifications
        '((solarized-dark-high-contrast
           (default :background "black")
           ;; (highlight :background "blue")
           )))

  ;; Setup emacs running environment
  ;; If uname is from msys2, then sub-system-type is msys
  (setq uname-os-string (downcase (shell-command-to-string "uname -o")))
  (defconst windows-msys "msys")
  (defconst sub-system-type (when (string-prefix-p "msys" uname-os-string) 'windows-msys))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Setup all the necessary directories
  ;; The following directories are all global variables
  ;; 为Windows下的emacs增加加载路径
  ;; setq 序列 (concat 序列 " " (int-to-string 变量))

  (if (eq system-type 'windows-nt)
      (defconst myDocument "D:/MyDocument/"))

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

  (setq bibtex-completion-bibliography (concat orgPath "Papers/references.bib")
        bibtex-completion-library-path (concat orgPath "Papers/")
        bibtex-completion-notes-path (concat orgPath "Papers/notes.org"))

  ) ;; end def dotspacemacs/user-init ()

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check the paths in load-path exist or not if not exists then prompt errors
  (dolist (pathx load-path)
    (if (equal nil (file-directory-p pathx))
        (message (concat "MY-INFO: dir does not exist in load-path: " pathx))
      )
    )

  ;; The directory that appears in the prompt for C-x C-f ('find-file') comes from
  ;; the value of default-directory, which is a buffer-local variable. When you
  ;; first start Emacs, the initial buffer displayed is the GNU Emacs buffer.
  ;; That buffer's default-directory is set from the variable
  ;; =command-line-default-directory=.
  (setq command-line-default-directory myDocument )

  ;; Set the default dir when using C-x C-f to open files
  ;; 2018/01/07 Seems this configuration does not work. see above comments
  ;; Because the variable "default-directory" is a buffer-local variables
  (setq default-directory myDocument)

  ;; dired-quick-sort depends on the following variable
  (setq ls-lisp-use-insert-directory-program t)
  ;; Switches passed to `ls' for Dired.
  (setq dired-listing-switches
        (cond ((eq sub-system-type 'windows-msys)   ; Cygwin/msys2 version of Emacs.
               "-a -F --group-directories-first -l --time-style=long-iso")
              ((eq system-type 'windows-nt) ; Native Windows version of Emacs.
               "-a -F -l")
              ))
  ;; (message (concat "Variable dired-listing-switches is " dired-listing-switches))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; setup chinese fonts
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      ;; charset (font-spec :family "华文楷体")
                      ;; charset (font-spec :family "微软雅黑")
                      charset (font-spec :family "华光楷体_CNKI")
                      ;; charset (font-spec :family "华光小标宋_CNKI")
                      )

    (setq face-font-rescale-alist '(("华文楷体" . 1.3)
                                    ("微软雅黑" . 1.3)
                                    ("华光楷体_CNKI" . 1.2)
                                    ("华光小标宋_CNKI" . 1.3))) ; Fira 12:1.2; Fira 13:1.3
    ) ;; end dolist

  ;; Set current window size to golden ratio(0.618)
  ;; (golden-ratio-mode t)

  ;; 设置stardict查词
  ;; ***相关文件安装***请参见 basicPath/lisp/init/init-stardict.el
  ;; (if (eq system-type 'windows-nt)
  ;;     (load "init-stardict"))

  ;; org-wild-notifier uses the [[https://melpa.org/#/alert][alert]] package for
  ;; showing notifications
  (setq alert-default-style 'notifications)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Since version 0.300, spacemacs uses the =org= version from the ELPA repository
  ;; instead of the one shipped with emacs. Then, any =org= related code should not
  ;; be loaded before =dotspacemacs/user-config=, otherwise both versions will be
  ;; loaded and will conflict.
  (with-eval-after-load 'org
    ;; Org related directories
    ;; Set to the location of your Org files on your local system
    (setq org-directory orgPath)

    ;; Emacs启动之后，首先显示日程列表
    ;; (eyebrowse-switch-to-window-config-1)
    ;; (setq org-agenda-window-setup 'current-window)
    (org-agenda-list)
    ;; (sleep-for 1)
    ;; (toggle-frame-maximized)
    )

  ;; 将org layer中设置好的org-projectile-todo-files 加入到org-agenda中
  ;; (with-eval-after-load 'org-agenda
  ;;   (require 'org-projectile)
  ;;   (mapcar (lambda (file)
  ;;              (when (file-exists-p file)
  ;;                (when (not (member file org-agenda-files))
  ;;                  (push file org-agenda-files) )))
  ;;           (org-projectile-todo-files)))

  (with-eval-after-load 'ox-latex
    (setf org-latex-default-packages-alist
          (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
    )

  ;;; To permanently enable mode line display of org clock
  (setq spaceline-org-clock-p t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; C++编译时若Makefile与源码不在同一目录，（一般在build目录）中，使用Project
  ;; 根目录下面的.dir-locals.el文件指定。该文件中一般会这样写：
  ;;   ((c++-mode (helm-make-build-dir . "build/")))
  ;; 指明Makefile所在目录。但是emacs会抱怨该目录不安全，因此让emacs闭嘴的方法：
  (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Input Method
  (setq default-input-method "pyim")
  (global-set-key (kbd "C-\\") 'toggle-input-method)

  ;;光标靠近鼠标的时候，让鼠标自动让开，别挡住视线
  (mouse-avoidance-mode 'animate)

  ;; 光标停留在一个括号上时，令配对括号反显
  (setq show-paren-delay 0)
  (show-paren-mode t)

) ; end user-config

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cookie-file "~/fortunes/tao")
 '(evil-want-Y-yank-to-eol nil)
 '(highlight-parentheses-colors '("#3cafa5" "#c49619" "#3c98e0" "#7a7ed2" "#93a61a"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-gnus org-habit org-id ol-info org-inlinetask org-tempo ol-jsinfo org-habit org-inlinetask ol-irc ol-mew ol-mhe org-protocol ol-rmail ol-vm ol-wl ol-w3m))
 '(package-selected-packages
   '(doom-modeline shrink-path lsp-latex company-reftex company-math math-symbol-lists company-auctex auctex pdf-view-restore pdf-tools tablist simple-httpd haml-mode counsel-css counsel swiper ivy web-completion-data add-node-modules-path org-wild-notifier helm-ctest cmake-mode zonokai-emacs ox-gfm ligature inspector info+ gendoxy cal-china-x buffer-move font-lock+ zenburn-theme zen-and-art-theme yasnippet-snippets xterm-color ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unicode-fonts unfill undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-icons-dired toxi-theme toc-org terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection string-edit spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme quickrun pyim purple-haze-theme pug-mode professional-theme prettier-js popwin plantuml-mode planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el password-generator paradox pangu-spacing overseer organic-green-theme org-superstar org-rich-yank org-re-reveal org-projectile org-present org-pomodoro org-mime org-download org-contrib org-cliplink open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme nameless mwim mustang-theme multi-term multi-line monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme madhat2r-theme macrostep lush-theme lsp-ui lsp-origami lorem-ipsum link-hint light-soap-theme kaolin-themes jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide impatient-mode hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-rtags helm-purpose helm-projectile helm-org-rifle helm-org helm-notmuch helm-mode-manager helm-make helm-lsp helm-ls-git helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate google-c-style golden-ratio gnuplot gh-md gandalf-theme fuzzy flyspell-correct-helm flycheck-ycmd flycheck-rtags flycheck-pos-tip flycheck-package flycheck-elsa flx-ido flatui-theme flatland-theme find-by-pinyin-dired farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emr emmet-mode elisp-slime-nav editorconfig dumb-jump drag-stuff dracula-theme dotenv-mode doom-themes django-theme disaster dired-quick-sort diminish define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode dakrone-theme cyberpunk-theme cpp-auto-include company-ycmd company-web company-statistics company-rtags company-c-headers column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode chocolate-theme chinese-conv cherry-blossom-theme centered-cursor-mode ccls busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes ahk-mode aggressive-indent afternoon-theme ace-pinyin ace-link ace-jump-helm-line ac-ispell))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pyim-dicts
   '(:name "pyim-greatdict" :file "d:/home/cyd/.spacemacs.private/local/pyim-bigdict.pyim" :coding utf-8-unix :dict-type pinyin-dict))
 '(pyim-page-tooltip t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))))
 '(show-paren-match ((t (:foreground "black" :background "red")))))
)
