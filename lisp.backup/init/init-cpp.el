;; This file "init-cpp.el" is for C++ programming language

; I want to use in c++ mode
(c-add-style "my-style" 
			 '("gnu"
			   (indent-tabs-mode . nil)        ; use spaces rather than tabs
			   (c-basic-offset . 4)            ; indent by four spaces
			   (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
								   (brace-list-open . 0)
								   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state 0))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; Add cmake-ide to enhance c/c++ programming
(require 'rtags) ;; optional, must have rtags installed
(require 'company-rtags)
(cmake-ide-setup)

;; Setup company-rtags
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))

;; Set up rtags
(setq rtags-completions-enabled t)
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings c-mode-base-map "\C-xr")
;; (rtags-enable-standard-keybindings)

(setq rtags-use-helm)

;; enable code completion in Emacs with company mode
;;; (setq rtags-autostart-diagnostics t)
;;; (setq rtags-completions-enabled t)
;;; (require 'company)
;;; (global-company-mode)
;;; (push 'company-rtags company-backends)

;; The C compiler flags to use.  Should have -I flags for system includes.
(setq cmake-ide-flags-c '("-I/usr/include"))
;;(setq cmake-ide-flags-c  '("-I/C:/cygwin/usr/include"))

;; The C++ compiler flags to use.  Should have -I flags for system includes.
;;   For a system with gcc, you can get this information by running
;;   gcc -v -xc++ /dev/null -fsyntax-only

(setq cmake-ide-flags-c++ '("-I/usr/include"
							"/lib/gcc/i686-pc-cygwin/5.3.0/include/c++"
							"/usr/lib/gcc/i686-pc-cygwin/5.3.0/include/c++/i686-pc-cygwin"
							"/usr/lib/gcc/i686-pc-cygwin/5.3.0/include/c++/backward"
							"/usr/lib/gcc/i686-pc-cygwin/5.3.0/include"
							"/usr/local/include"
							"/usr/lib/gcc/i686-pc-cygwin/5.3.0/../../../../include/w32api"
							))

;; The build directory to run CMake in.  If nil, runs in a temp dir.  DEPRECATED, use cmake-ide-build-dir instead.
(setq cmake-ide-dir nil)

;; The build directory to run CMake in.  If nil, runs in a temp dir.
(setq cmake-ide-build-dir nil)

;; The command to use to compile the project.  Can also include running tests.")
(setq cmake-ide-compile-command "gcc")
