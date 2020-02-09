;; It contains this list of packages of the layer and the actual configuration
;; for the packages included in the layer.

;; This file is loaded after layers.el.

;; It must define a variable called <layer>-packages, which should be a list of
;; all the packages that this layer needs. Some valid package specifications are
;; as follows:

;; (defconst mylayer-packages
;;   '(
;;     ;; Get the package from MELPA, ELPA, etc.
;;     some-package
;;     (some-package :location elpa)
;; 
;;     ;; A local package
;;     (some-package :location local)
;; 
;;     ;; A package recipe
;;     (some-package :location (recipe
;;                              :fetcher github
;;                              :repo "some/repo"))
;; 
;;     ;; An excluded package
;;     (some-package :excluded t)
;;     ))

;; The :location attribute specifies where the package may be found. Spacemacs
;; currently supports packages on ELPA compliant repositories, local packages
;; and MELPA recipes (through the Quelpa package). Local packages should reside
;; at <layer>/local/<package>/. For information about recipes see the MELPA
;; documentation.
;; 
;; Packages may be excluded by setting the :excluded property to true. This will
;; prevent the package from being installed even if it is used by another layer.
;; 
;; For each included package, you may define one or more of the following
;; functions, which are called in order by Spacemacs to initialize the package.
;; 
;; <layer>/pre-init-<package>
;; <layer>/init-<package>
;; <layer>/post-init-<package>
;;
;; It is the responsibility of these functions to load and configure the package
;; in question. Spacemacs will do nothing other than download the package and
;; place it in the load path for you.
;; 
;; Note: A package will not be installed unless at least one layer defines an
;; init function for it. That is to say, in a certain sense, the init function
;; does mandatory setup while the pre-init and post-init functions do optional
;; setup. This can be used for managing cross-layer dependencies, which we will
;; discuss later.

(defconst my-buffer-packages
  '(
    ;; Get the package from MELPA, ELPA, etc.
    (buffer-move :location elpa)
    ))

(defun my-buffer/init-buffer-move ()
  (use-package buffer-move
    :defer t)
  )
