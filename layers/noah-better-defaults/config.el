;;; config.el --- noah-better-defaults layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <noah@Archier>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;;; add packages
(add-to-list 'load-path "~/.spacemacs.d/packages/nyan-mode/")


;;;; require packages
(require 'python)
(require 'nyan-mode)


;; set fundamental-mode for big files
(add-hook 'find-file-hook 'spacemacs/check-large-file)

;; ;; reload python-mode to fix company-backends bugs
;; (add-hook 'find-file-hook 'spacemacs/reload-python-mode)

;; change the tab's space
(setq c-basic-offset 4)
(setq c-default-style "ellemtel")
(setq default-tab-width 4)
(setq nasm-basic-offset 4)
(setq octave-block-offset 4)
;; SPC instand of Tab
(setq-default indent-tabs-mode nil)

;; basic hack
(menu-bar-mode t)
(electric-pair-mode t)
(delete-selection-mode t)
(global-font-lock-mode t)

;; set comment-style
(setq comment-style 'multi-line)

;; cancel paste indent
(add-hook 'spacemacs-indent-sensitive-modes 'snippet-mode)

;; add auto-mode-lists
(add-to-list 'auto-mode-alist '("\\.snippet\\'" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

;; set current window's number
(defvar spacemacs/helm-find-files-window-number 0)

;; set alpha-list
(setq alpha-list '((95 65) (85 55) (75 45) (65 35) (0 0) (100 100)))

;; display time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-use-mail-icon t)
(setq display-time-interval 60)
(setq display-time-format "%m.%d %A %H:%M")
;; (display-time-mode t)

;;; set semantic
;; disable emacs-lisp-mode
(add-hook 'semantic-inhibit-functions
          '(lambda ()
             (member major-mode '(emacs-lisp-mode scheme-mode))))

;; only enable cc-mode
;; (setq semantic-inhibit-functions
;;       '(lambda ()
;;          (not (and (featurep 'cc-defs)
;;                    c-buffer-is-cc-mode))))


;; set default configs in prog-mode
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq-local require-final-newline nil)
             (company-mode t)
             (highlight-indent-guides-mode t)))

;; set eshell
(add-hook 'eshell-mode-hook
          '(lambda ()
             (setq-local evil-move-cursor-back nil)))

;; set company for text-mode
(add-hook 'text-mode-hook
          '(lambda ()
             (setq-local require-final-newline nil)
             (company-mode t)))

;; set magit-status-mode
(add-hook 'magit-status-mode-hook
          '(lambda ()
             (display-line-numbers-mode t)))

;; set eww
(add-hook 'eww-after-render-hook
          '(lambda ()
             (setq-local truncate-lines nil)
             (vline-mode t)))

;; set treemacs-mode
(add-hook 'treemacs-mode-hook
          '(lambda ()
             (vline-mode t)
             (highlight-indentation-mode t)))

;; set c-mode
(add-hook 'c-mode-hook
          '(lambda ()
             (setq-local comment-start "// ")
             (setq-local comment-end "")))

;; ;; set c++-mode
;; (add-hook 'c++-mode-hook
;;           '(lambda ()
;;              (setq-local comment-start "/* ")
;;              (setq-local comment-end   " */")))

;; ;; set java-mode
;; (add-hook 'java-mode-hook
;;           '(lambda ()
;;              (setq-local comment-start "/* ")
;;              (setq-local comment-end   " */")))

;; set css-mode
(add-hook 'css-mode-hook
          '(lambda ()
             (setq-local comment-start "/* ")
             (setq-local comment-end   " */")))

;; set lisp-mode
(add-hook 'lisp-mode-hook
          '(lambda ()
             (setq-local comment-start "; ")))

;; set emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (setq-local comment-start "; ")))

;; set scheme-mode
(add-hook 'scheme-mode-hook
          '(lambda ()
             (setq-local comment-start "; ")))

;; setq scheme's default geiser-implementations
(setq geiser-implementations-alist
      '(((regexp "\\.scm$")
         chez)
        ((regexp "\\.ss$")
         chez)))


;; set racket-mode
(add-hook 'racket-mode-hook
          '(lambda ()
             (setq-local comment-start "; ")
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


;; define matlab-mode
(add-hook 'matlab-mode-hook
          '(lambda ()
             (setq-local yas-indent-line 'fixed)
             (setq-local comment-add 1)
             (setq-local evil-shift-width 4)
             (setq-local comment-start "% ")))

;; set LaTeX-mode
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (setq-local yas-indent-line 'fixed)
             (setq-local comment-add 1)
             (setq-local comment-start "% ")
             (rainbow-delimiters-mode t)))

;; set nasm-mode
(add-hook 'nasm-mode-hook
          '(lambda ()
             (setq-local comment-start "; ")))

;; set python-mode
(add-hook 'python-mode-hook
          '(lambda ()
             (setq-local comment-add 1)
             (setq-local yas-indent-line 'fixed)))

;; set ein:ml-mode
(add-hook 'ein:notebook-multilang-mode-hook
          '(lambda ()
             (smartparens-mode t)
             (setq-local comment-add 1)
             (setq-local evil-shift-width 4)
             (setq-local yas-indent-line 'fixed)
             (yas-activate-extra-mode 'python-mode)))


;; set spelling-checking
(add-hook 'text-mode-hook
          '(lambda ()
             (quote spacemacs/toggle-spelling-checking-on)))

;; set org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             (setq-local truncate-lines nil)))
;; font-lock-mode in org file
(setq-default org-src-fontify-natively t)



;;; config.el ends here