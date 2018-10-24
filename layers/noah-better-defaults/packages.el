;;; packages.el --- noah-better-defaults layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <noah-better-defaults@Archier>
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
;; added to `noah-better-defaults-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `noah-better-defaults/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `noah-better-defaults/pre-init-PACKAGE' and/or
;;   `noah-better-defaults/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst noah-better-defaults-packages
  '(
    (sublimity :location local)
    (vline-mode :location local)
    evil-paredit
    highlight-indent-guides
    tabbar-ruler
    ))

(unless (and (spacemacs/system-is-mswindows) window-system)
  (setq noah-better-defaults-packages
        (append noah-better-defaults-packages
         '(
           (eaf :location local)
           ))))



(defun noah-better-defaults/init-eaf()
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

(defun noah-better-defaults/init-sublimity()
  (use-package sublimity
    :defer t
    :init
    ;;; set the sublimity
    (require 'sublimity-map)
    ;; (require 'sublimity-scroll)
    ;; (require 'sublimity-attractive)
    (sublimity-map-set-delay 0.2)
    (spacemacs/set-leader-keys "t M" 'sublimity-mode)))

(defun noah-better-defaults/init-vline-mode()
  (use-package vline-mode
    :defer t
    :init
    (require 'vline)
    (spacemacs/set-leader-keys "t C-v" 'vline-mode)))

(defun noah-better-defaults/init-evil-paredit()
  (use-package evil-paredit
    :defer t
    :init
    (evil-define-key 'visual evil-paredit-mode-map
      (kbd "d") 'evil-delete
      (kbd "c") 'evil-change
      (kbd "y") 'evil-yank
      (kbd "D") 'evil-delete-line
      (kbd "C") 'evil-change-line
      (kbd "S") 'evil-change-whole-line
      (kbd "Y") 'evil-yank-line
      (kbd "X") 'delete-char
      (kbd "x") 'delete-backward-char)))

(defun noah-better-defaults/init-highlight-indent-guides()
  (use-package highlight-indent-guides
    :defer t
    :init
    (setq highlight-indent-guides-method 'character)))

(defun noah-better-defaults/init-tabbar-ruler()
  (use-package tabbar-ruler
    :defer t
    :init
    (tabbar-mode t)
    (setq tabbar-separator (list 0.5))))



;;; packages.el ends here
