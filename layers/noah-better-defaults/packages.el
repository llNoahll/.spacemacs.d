;;; packages.el --- noah-better-defaults layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
  `(
    (sublimity       :location local)
    (vline-mode      :location local)
    ;; (quack-mode      :location local)
    (rash-mode       :location local)
    (scribble-mode   :location local)
    (yin-mode        :location local)
    mips-mode
    on-parens
    evil-textobj-anyblock
    evil-paredit
    highlight-indent-guides
    tabbar-ruler
    company-tabnine
    ,(unless (and (spacemacs/system-is-mswindows) window-system)
       '(eaf :location local))
    ))



(defun noah-better-defaults/init-eaf ()
  (use-package eaf
    :defer t
    :init
    (require 'eaf)

    (spacemacs/declare-prefix  "a f" "frame")
    (spacemacs/set-leader-keys "a f o" 'eaf-open)
    (spacemacs/set-leader-keys "a f a" 'eaf-open-application)

    (spacemacs/declare-prefix  "F a" "applications")
    (spacemacs/set-leader-keys "F a o" 'eaf-open)
    (spacemacs/set-leader-keys "F a o" 'eaf-open-application)))

(defun noah-better-defaults/init-sublimity ()
  (use-package sublimity
    :defer t
    :init
    (require 'sublimity-map)
    ;; (require 'sublimity-scroll)
    ;; (require 'sublimity-attractive)

    (setq sublimity-map-size 13)
    (setq sublimity-map-text-scale -11)
    (sublimity-map-set-delay 1.0)
    (spacemacs/set-leader-keys "t M" 'sublimity-mode)))

(defun noah-better-defaults/init-vline-mode ()
  (use-package vline-mode
    :defer t
    :init
    (require 'vline-mode)

    ;; set the highlight column color
    (set-face-background vline-face (face-attribute hl-line-face :background))
    ;; set idle time
    (setq vline-idle-time 1)

    (spacemacs/set-leader-keys "t C-v" 'vline-mode)))

(defun noah-better-defaults/init-quack-mode ()
  (use-package quack-mode
    :defer t
    :init
    (require 'quack)
    ))

(defun noah-better-defaults/init-rash-mode ()
  (use-package rash-mode
    :defer t
    :init
    (require 'rash)
    ))

(defun noah-better-defaults/init-scribble-mode ()
  (use-package scribble-mode
    :defer t
    :init
    (require 'scribble)
    ))

(defun noah-better-defaults/init-yin-mode ()
  (use-package yin-mode
    :defer t
    :init
    (require 'yin-mode)
    (add-to-list 'auto-mode-alist '("\\.yin\\'" . yin-mode))
    ))

(defun noah-better-defaults/init-on-parens ()
  (use-package on-parens
    :defer t
    :init
    ))

(defun noah-better-defaults/init-mips-mode ()
  (use-package mips-mode
    :defer t
    :init
    ))

(defun noah-better-defaults/init-evil-textobj-anyblock ()
  (use-package evil-textobj-anyblock
    :defer t
    :init
    ))

(defun noah-better-defaults/init-evil-paredit ()
  (use-package evil-paredit
    :defer t
    :init
    (require 'evil-paredit)

    (define-key evil-paredit-mode-map (kbd "C-<left>") 'paredit-forward-barf-sexp)
    (define-key evil-paredit-mode-map (kbd "C-<right>") 'paredit-forward-slurp-sexp)
    (define-key evil-paredit-mode-map (kbd "M-r") 'paredit-raise-sexp)

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

(defun noah-better-defaults/init-highlight-indent-guides ()
  (use-package highlight-indent-guides
    :defer t
    :init
    (setq highlight-indent-guides-method 'character)))

(defun noah-better-defaults/init-tabbar-ruler ()
  (use-package tabbar-ruler
    :defer t
    :init
    (tabbar-mode t)
    (setq tabbar-separator (list 0.5))))

(defun noah-better-defaults/init-company-tabnine ()
  (use-package company-tabnine
    :defer t
    :ensure t
    :init
    (require 'company-tabnine)

    (add-to-list 'company-backends #'company-tabnine)))



;;; packages.el ends here
