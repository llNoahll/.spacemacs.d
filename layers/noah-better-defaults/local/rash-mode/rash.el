;;; rash.el --- the Racket command shell  -*- lexical-binding:t -*-
;;
;; Copyright (c) 2012-2019 Noah
;;
;; Author:  <noah@Archier>
;; URL: https://github.com/llNoahll/.spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;;###autoload
(define-derived-mode rash-mode eshell-mode "Rash"
  "Racket shell interactive mode."
  (setq-local rash-mode t)
  (setq-local evil-move-cursor-back nil))



(provide 'rash)
