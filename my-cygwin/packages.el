;;; packages.el --- my-cygwin layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <cyd@ALOHAWORLD>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-cygwin-mount-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-cygwin-mount/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-cygwin-mount/pre-init-PACKAGE' and/or
;;   `my-cygwin-mount/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-cygwin-packages
  '(
    (cygwin-mount :location elpa)
    ;;cygwin-mount
    )
  "The list of Lisp packages required by the my-cygwin-mount layer."
  )

(defun my-cygwin/init-cygwin-mount ()
  "Initialize my package"
  (use-package cygwin-mount
    :defer t
    :init
    ;; Code to execute before cygwin-mount is loaded
    (progn
      (setenv "PATH" (concat "c:/cygwin/bin;c:/cygwin/usr/bin;c:/cygwin/usr/local/bin;" (getenv "PATH")))
      (setq exec-path (cons "c:/cygwin/bin/" exec-path))
      )
    :config
    ;; Code to execute after cygwin-mount is loaded
    (cygwin-mount-activate)
    )
  )

;;; packages.el ends here
