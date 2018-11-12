;;; packages.el --- my-commons layer packages file for Spacemacs.
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
;; added to `my-commons-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-commons/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-commons/pre-init-PACKAGE' and/or
;;   `my-commons/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-commons-packages
  '(
    (cal-china-x :location elpa)
    )
  "The list of Lisp packages required by the my-commons layer.

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
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format"
)

(defun my-commons/init-cal-china-x ()
  (use-package cal-china-x
    :defer t
    :init
    ;; Code to execute before cal-china-x is loaded
    :config
    ;; Code to execute after cal-china-x is loaded
    (progn
      (setq mark-holidays-in-calendar t)
      (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
      (setq calendar-holidays
            (append cal-china-x-important-holidays
                    calendar-holidays))
;;                    other-holidays))
      (setq cal-china-x-general-holidays
            '((holiday-lunar 1 15 "元宵节")))
      ) ;; end (progn
    )
  )

;;;;; packages.el ends here
