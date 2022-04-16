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
