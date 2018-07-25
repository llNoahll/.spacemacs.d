;;; config.el --- noah layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Noah <noah@noah-VirtualBox>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;;; add list
(add-to-list 'load-path "~/.spacemacs.d/packages/vline-mode")
(add-to-list 'load-path "~/.spacemacs.d/packages/nyan-mode")


;;;; require packages
(require 'vline)
(require 'python)
(require 'nyan-mode)


;; set fundamental-mode for big files
(add-hook 'find-file-hook 'spacemacs/check-large-file)

;; reload python-mode to fix company-backends bugs
(add-hook 'find-file-hook 'spacemacs/reload-python-mode)

;; set the highlight column color
(set-face-background vline-face (face-attribute hl-line-face :background))

;; change the tab's space
(setq c-basic-offset 4)
(setq c-default-style "ellemtel")
(setq default-tab-width 4)
(setq octave-block-offset 4)
;; SPC instand of Tab
(setq-default indent-tabs-mode nil)

;; basic hack
(menu-bar-mode t)
(electric-pair-mode t)
(delete-selection-mode t)
(global-font-lock-mode t)

;;; setq semantic
;; disable emacs-lisp-mode
(add-hook 'semantic-inhibit-functions
          (lambda ()
            (member major-mode '(emacs-lisp-mode))))
;; enable cc-mode
;; (setq semantic-inhibit-functions
;;       '(lambda ()
;;          (not (and (featurep 'cc-defs)
;;                    c-buffer-is-cc-mode))))

;; set alpha-list
(setq alpha-list '((95 65) (85 55) (75 45) (65 35) (0 0) (100 100)))

;; display time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-use-mail-icon t)
(setq display-time-interval 60)
(setq display-time-format "%m.%d %A %H:%M")
(display-time-mode t)

;; set default configs in prog-mode
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq require-final-newline nil)
             (company-mode t)
             (highlight-indent-guides-mode t)))

;; set eshell
(add-hook 'eshell-mode-hook
          '(lambda ()
             (setq-local evil-move-cursor-back nil)))

;; set company for text-mode
(add-hook 'text-mode-hook
          '(lambda ()
             (setq require-final-newline nil)
             (company-mode t)))

;; set magit-mode
(add-hook 'magit-mode-hook
          '(lambda ()
             (display-line-numbers-mode t)))

;; set eww-mode
(add-hook 'eww-mode-hook
          '(lambda ()
             (vline-mode t)))

;; set treemacs-mode
(add-hook 'treemacs-mode-hook
          '(lambda ()
             (vline-mode t)
             (highlight-indentation-mode t)))

;; set racket-mode
(add-hook 'racket-mode-hook
          '(lambda ()
             (geiser-mode t)))

(add-hook 'racket-repl-mode-hook
          '(lambda ()
             (setq-local evil-move-cursor-back nil)
             (geiser-mode t)))

;; define octave-mode
(add-hook 'octave-mode-hook
          '(lambda ()
             (setq-local comment-start "% ")))

(add-hook 'inferior-octave-mode-hook
          '(lambda ()
             (company-mode t)))

;; set LaTeX-mode
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (rainbow-delimiters-mode t)))

;; set ein:ml-mode
(add-hook 'ein:notebook-multilang-mode-hook
          '(lambda ()
             (yas-activate-extra-mode 'python-mode)))


;; font-lock-mode in org file
(setq-default org-src-fontify-natively t)



;;; config.el ends here
