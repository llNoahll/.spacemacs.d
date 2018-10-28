;;; packages.el --- matlab layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <noah@Archier>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst matlab-packages
  '(
    matlab-mode
    ))



(defun matlab/init-matlab-mode()
  (use-package matlab-mode
    :defer t
    ))



;;; packages.el ends here
