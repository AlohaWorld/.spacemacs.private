;;; packages.el --- my-org layer packages file for Spacemacs.
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
;; added to `my-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-org/pre-init-PACKAGE' and/or
;;   `my-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-org-packages
  '(
    ;; Following package are from config.el/18.4
    ;; org-checklist
    )
  "The list of Lisp packages required by the my-org layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(when (configuration-layer/layer-usedp 'org)
  (defun my-org/post-init-org ()
    ;; The following hook will be installed to org-mode only when org layer is
    ;; enabled.
    ;; 在org模式中自动检测table
    (add-hook 'org-mode-hook 'table-recognize)
    (add-hook 'org-mode-hook 'my-org-toc-show-hook)
	)
  )

;; 为org模式提供额外小窗口显示大纲，并且避免行的自动截短
;; See funcs.el to find function "my-org-toc-show-hook"
;; (defun my-org/init-org-toc ()
;; use-package org-toc
;;   :defer t
;;   :config
;;   (progn
;;     (when (configuration-layer/layer-usedp 'org)
;;       (add-hook 'org-mode-hook 'my-org-toc-show-hook)))
;;   )



;; (defun my-org/init-org-habit ()
;;   use-package org-habit
;;   :defer t
;;   )
;; 
;; (defun my-org/init-org-id ()
;;   use-package org-id
;;   :defer t
;;   )
;; 
;; (defun my-org/init-ox-html ()
;;   use-package ox-html
;;   :defer t
;;   )
;; 
;; (defun myorg/init-ox-latex ()
;;   use-package ox-latex
;;   :defer t
;;   )
;; 
;; (defun myorg/init-ox-ascii ()
;;   use-package ox-ascii
;;   :defer t
;;   )
;;; packages.el ends here 
