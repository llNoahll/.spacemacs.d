;;; packages.el --- noah-scheme layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
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
;; added to `noah-scheme-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `noah-scheme/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `noah-scheme/pre-init-PACKAGE' and/or
;;   `noah-scheme/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst noah-scheme-packages
  '(
    ,(unless (and (spacemacs/system-is-mswindows) window-system)
       '(r6lint :location local))
    ))



(defun noah-scheme/init-r6lint ()
  (use-package r6lint
    :defer t
    :init
    (require 'flycheck-r6lint)
    (eval-after-load 'flycheck '(flycheck-r6lint-setup))))



;;; packages.el ends here
