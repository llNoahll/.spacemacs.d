;;; packages.el --- noah layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Noah <noah@noah-VirtualBox>
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
    highlight-indent-guides
    tabbar-ruler
    (vline-mode :location local)
    (sublimity :location local)
    ))



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

(defun noah/init-vline-mode()
  (use-package vline-mode
    :defer t
    :init
    (require 'vline)
    (spacemacs/set-leader-keys "t C-v" 'vline-mode)))

(defun noah/init-sublimity()
  (use-package sublimity
    :defer t
    :init
    ;;; set the sublimity
    (require 'sublimity-map)
    ;; (require 'sublimity-scroll)
    ;; (require 'sublimity-attractive)
    (spacemacs/set-leader-keys "t M" 'sublimity-mode)))



;;; packages.el ends here
