;;; packages.el --- noah layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <noah@Archier>
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
;; added to `noah-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `noah/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `noah/pre-init-PACKAGE' and/or
;;   `noah/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst noah-packages
  '(
    (eaf :location local)
    (sublimity :location local)
    (vline-mode :location local)
    highlight-indent-guides
    tabbar-ruler
    ))



(defun noah/init-eaf()
  (use-package eaf
    :defer t
    :init
    (require 'eaf)
    (spacemacs/declare-prefix  "a f" "frame")
    (spacemacs/set-leader-keys "a f o" 'eaf-open)
    (spacemacs/set-leader-keys "a f a" 'eaf-open-application)
    (spacemacs/set-leader-keys "a f u" 'eaf-upload-file)

    (spacemacs/declare-prefix  "F a" "applications")
    (spacemacs/set-leader-keys "F a o" 'eaf-open)
    (spacemacs/set-leader-keys "F a o" 'eaf-open-application)
    (spacemacs/set-leader-keys "F a u" 'eaf-upload-file)))

(defun noah/init-sublimity()
  (use-package sublimity
    :defer t
    :init
    ;;; set the sublimity
    (require 'sublimity-map)
    ;; (require 'sublimity-scroll)
    ;; (require 'sublimity-attractive)
    (sublimity-map-set-delay 0.2)
    (spacemacs/set-leader-keys "t M" 'sublimity-mode)))

(defun noah/init-vline-mode()
  (use-package vline-mode
    :defer t
    :init
    (require 'vline)
    (spacemacs/set-leader-keys "t C-v" 'vline-mode)))

(defun noah/init-highlight-indent-guides()
  (use-package highlight-indent-guides
    :defer t
    :init
    (setq highlight-indent-guides-method 'character)))

(defun noah/init-tabbar-ruler()
  (use-package tabbar-ruler
    :defer t
    :init
    (tabbar-mode t)
    (setq tabbar-separator (list 0.5))))



;;; packages.el ends here
