;;; packages.el --- my-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <cyd@ALOHAWORLD>
;; URL: https://github.com/AlohaWorld
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; This file is loaded after layers.el

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

;;;  packages.el must define a variable called <layer>-packages,
;; which should be a list of all the packages that this layer needs.
(defconst my-org-packages
  '(
    (ox-latex :location built-in
              :toggle my-org-enable-latex-support) ; see config.el
    (org-yaap :location (recipe :fetcher gitlab :repo "tygrdev/org-yaap")
              :toggle my-org-enable-org-yaap) ; see config.el
    (org-timed-alerts :location (recipe :fetcher github :repo "legalnonsense/org-timed-alerts")
                      :toggle my-org-enable-org-timed-alerts) ; see config.el
    )
  "The list of Lisp packages required by the my-org layer.")


(when (configuration-layer/layer-usedp 'org)
  (defun my-org/post-init-org ()
    ;; The following hook will be installed to org-mode only when org layer is
    ;; enabled.
    ;; 在org模式中自动检测table
    (add-hook 'org-mode-hook 'table-recognize)
    (add-hook 'org-mode-hook 'my-org-toc-show-hook)
    (if my-org-enable-fill-column-mode (turn-on-auto-fill) (turn-off-auto-fill))
    ;; (add-hook 'org-mode-hook 'turn-off-auto-fill) ; We do NOT hard wrap lines in org-mode
    (use-package ox-md)
	  ) ; end defun my-org/post-init-org

  (defun my-org/pre-init-ox-latex ()
    (spacemacs|use-package-add-hook org :post-config (require 'ox-latex)))
  (defun my-org/init-ox-latex ()
    (use-package ox-latex
      :init
      :config
      (progn
        (setq org-latex-compiler my-org-latex-compiler)
        (setq org-latex-image-default-width
              my-org-latex-image-default-width)
        ;; set the package list
        (setq org-latex-packages-alist '())
        (add-to-list 'org-latex-packages-alist '("" "color" t))
        (add-to-list 'org-latex-packages-alist '("" "physics" t))
        (add-to-list 'org-latex-packages-alist '("" "mathtools" t))
        (add-to-list 'org-latex-packages-alist '("" "xfrac" t))
        (add-to-list 'org-latex-packages-alist '("" "siunitx" t))
        (add-to-list 'org-latex-packages-alist '("" "mhchem" t))
        (add-to-list 'org-latex-packages-alist '("" "fontenc" t))
        (add-to-list 'org-latex-packages-alist '("" "multirow" t))
        (add-to-list 'org-latex-packages-alist '("" "xeCJK" t))
        (add-to-list 'org-latex-packages-alist '("" "listings" t))
        ) ; end progn
      )
    ) ; end defun my-org/init-ox-latex
  (defun my-org/post-init-ox-latex()
    (progn
      (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                    "xelatex -interaction nonstopmode %f"))

      ;; export cn character
      (setf org-latex-default-packages-alist
            (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
      ) ; end progn
    ) ; end defun my-org/post-init-ox-latex

  (defun my-org/pre-init-org-yaap()
      nil)
  (defun my-org/init-org-yaap()
      (use-package org-yaap
        :init
        :config
        (org-yaap-mode 1))
      )
  (defun my-org/post-init-org-yaap()
      nil)

  ;; org-timed-alerts
  (defun my-org/pre-init-org-timed-alerts()
      nil)
  (defun my-org/init-org-timed-alerts()
    (progn
    (use-package org-timed-alerts
      :after (org)
      :custom
      (org-timed-alerts-alert-function 'alert)
      (org-timed-alerts-tag-exclusions nil)
      (org-timed-alerts-default-alert-props nil)
      (org-timed-alerts-warning-times '(-10 -5))
      (org-timed-alerts-agenda-hook-p t)
      (org-timed-alert-final-alert-string "IT IS %alert-time\n\n%todo %headline")
      (org-timed-alert-warning-string (concat "%todo %headline\n at %alert-time\n "
                                              "it is now %current-time\n "
                                              "*THIS IS YOUR %warning-time MINUTE WARNING*"))
      :config
      (add-hook 'org-mode-hook #'org-timed-alerts-mode))
    (use-package alert
      :commands (alert)
      :config (setq alert-default-style 'toast))

    (use-package alert-toast
      :after alert)
    )
  )

  (defun my-org/post-init-org-timed-alerts()
    nil)

  ;;(defun org/init-ox-latex ()
  ;;  (use-package ox-latex :after ox))
) ; end when


;;; Initialize each packagee
;; For each included package, you may define one or more of the following functions,
;;   which are called in order by Spacemacs to initialize the package.
;;     <layer>/pre-init-<package>
;;     <layer>/init-<package>
;;     <layer>/post-init-<package>

;;; packages.el ends here 
