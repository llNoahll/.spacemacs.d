;;; keybindings.el --- noah layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Noah <noah@noah-VirtualBox>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;;; require packages
(require 'evil-org)


;; set "C-Tab" as "Esc"
(define-key key-translation-map (kbd "<C-tab>") (kbd "<escape>"))

;; change hjkl to ijkl
(define-key evil-normal-state-map (kbd "H") 'evil-insert-line)
(define-key evil-normal-state-map (kbd "I") 'nil)
(define-key evil-normal-state-map (kbd "h") 'evil-insert)
(define-key evil-normal-state-map (kbd "i") 'evil-previous-line)
(define-key evil-normal-state-map (kbd "k") 'evil-next-line)
(define-key evil-normal-state-map (kbd "j") 'evil-backward-char)

(define-key evil-visual-state-map (kbd "h h") 'evil-indent-plus-i-indent)
(define-key evil-visual-state-map (kbd "i i") 'nil)
(define-key evil-visual-state-map (kbd "H") 'evil-insert)
(define-key evil-visual-state-map (kbd "I") 'nil)
(define-key evil-visual-state-map (kbd "h") evil-inner-text-objects-map)
(define-key evil-visual-state-map (kbd "i") 'evil-previous-line)
(define-key evil-visual-state-map (kbd "k") 'evil-next-line)
(define-key evil-visual-state-map (kbd "j") 'evil-backward-char)

(define-key evil-motion-state-map (kbd "H") 'nil)
(define-key evil-motion-state-map (kbd "h") 'nil)
(define-key evil-motion-state-map (kbd "I") 'evil-window-top)
(define-key evil-motion-state-map (kbd "i") 'evil-previous-line)
(define-key evil-motion-state-map (kbd "k") 'evil-next-line)
(define-key evil-motion-state-map (kbd "j") 'evil-backward-char)

(define-key evil-normal-state-map (kbd "<up>") 'previous-line)
(define-key evil-normal-state-map (kbd "<down>") 'next-line)
(define-key evil-normal-state-map (kbd "<left>") 'left-char)
(define-key evil-normal-state-map (kbd "<right>") 'right-char)

(define-key evil-motion-state-map (kbd "<up>") 'previous-line)
(define-key evil-motion-state-map (kbd "<down>") 'next-line)
(define-key evil-motion-state-map (kbd "<left>") 'left-char)
(define-key evil-motion-state-map (kbd "<right>") 'right-char)

(define-key evil-evilified-state-map (kbd "h") 'nil)
(define-key evil-evilified-state-map (kbd "i") 'evil-previous-line)
(define-key evil-evilified-state-map (kbd "k") 'evil-next-line)
(define-key evil-evilified-state-map (kbd "j") 'evil-backward-char)

(define-key evil-operator-state-map (kbd "i") 'evil-previous-line)
(define-key evil-operator-state-map (kbd "h") evil-inner-text-objects-map)

;; exchange "h" and "i"
(let ((state '(visual operator)))
  (evil-define-key state evil-org-mode-map (kbd "i e") 'nil)
  (evil-define-key state evil-org-mode-map (kbd "i E") 'nil)
  (evil-define-key state evil-org-mode-map (kbd "i r") 'nil)
  (evil-define-key state evil-org-mode-map (kbd "i R") 'nil)
  (evil-define-key state evil-org-mode-map (kbd "i") 'evil-previous-line)
  (evil-define-key state evil-org-mode-map (kbd "h e") 'evil-org-inner-element)
  (evil-define-key state evil-org-mode-map (kbd "h E") 'evil-org-inner-greater-element)
  (evil-define-key state evil-org-mode-map (kbd "h r") 'evil-org-inner-greater-element)
  (evil-define-key state evil-org-mode-map (kbd "h R") 'evil-org-inner-subtree))

(add-hook 'eww-mode-hook
          '(lambda ()
             (define-key eww-link-keymap (kbd "h") 'shr-browse-image)
             (define-key eww-link-keymap (kbd "i") 'nil)
             (define-key shr-image-map (kbd "h") 'shr-browse-image)
             (define-key shr-image-map (kbd "i") 'nil)
             (define-key shr-map (kbd "h") 'shr-browse-image)
             (define-key shr-map (kbd "i") 'nil)))

(add-hook 'package-menu-mode-hook
          '(lambda ()
             (define-key evil-normal-state-local-map (kbd "k") 'evil-previous-line)
             (define-key evil-normal-state-local-map (kbd "j") 'evil-next-line)
             (define-key evil-normal-state-local-map (kbd "h") 'evil-backward-char)))

;; modify "C-x b" list-buffers
(global-set-key (kbd "C-x b") 'helm-mini)

;; set treemacs
(spacemacs/set-leader-keys "-" 'treemacs-select-window)
(define-key winum-keymap (kbd "M--") 'treemacs-select-window)
(define-key winum-keymap (kbd "C-x w -") 'treemacs-select-window)

(add-hook 'treemacs-mode-hook
          '(lambda ()
             (define-key evil-normal-state-local-map (kbd "i") 'treemacs-mode)

             (define-key treemacs-mode-map (kbd "<escape>") 'evil-normal-state)

             (define-key evil-normal-state-local-map (kbd "k") 'evil-previous-line)
             (define-key evil-normal-state-local-map (kbd "j") 'evil-next-line)
             (define-key evil-normal-state-local-map (kbd "h") 'evil-backward-char)
             (define-key evil-emacs-state-local-map  (kbd "j") 'treemacs-next-line)
             (define-key evil-emacs-state-local-map  (kbd "k") 'treemacs-previous-line)
             (define-key evil-hybrid-state-local-map (kbd "j") 'treemacs-next-line)
             (define-key evil-hybrid-state-local-map (kbd "k") 'treemacs-previous-line)
             (define-key evil-insert-state-local-map (kbd "j") 'treemacs-next-line)
             (define-key evil-insert-state-local-map (kbd "k") 'treemacs-previous-line)

             (define-key evil-normal-state-local-map (kbd "d") 'treemacs-delete)
             (define-key evil-normal-state-local-map (kbd "R") 'treemacs-rename)
             (define-key evil-normal-state-local-map (kbd "r") 'treemacs-refresh)
             (define-key evil-normal-state-local-map (kbd "c d") 'treemacs-create-dir)
             (define-key evil-normal-state-local-map (kbd "c f") 'treemacs-create-file)

             (define-key evil-insert-state-local-map (kbd ":") 'evil-ex)
             (define-key evil-insert-state-local-map (kbd "G") 'evil-goto-line)
             (define-key evil-insert-state-local-map (kbd "m") 'evil-set-marker)
             (define-key evil-insert-state-local-map (kbd "'") 'evil-goto-mark-line)
             (define-key evil-insert-state-local-map (kbd "/") 'evil-ex-search-forward)
             (define-key evil-insert-state-local-map (kbd "p") 'evil-ex-search-backward)
             (define-key evil-insert-state-local-map (kbd "n") 'evil-ex-search-next)
             (define-key evil-insert-state-local-map (kbd "N") 'evil-ex-search-previous)

             (define-key evil-hybrid-state-local-map (kbd ":") 'evil-ex)
             (define-key evil-hybrid-state-local-map (kbd "G") 'evil-goto-line)
             (define-key evil-hybrid-state-local-map (kbd "m") 'evil-set-marker)
             (define-key evil-hybrid-state-local-map (kbd "'") 'evil-goto-mark-line)
             (define-key evil-hybrid-state-local-map (kbd "/") 'evil-ex-search-forward)
             (define-key evil-hybrid-state-local-map (kbd "p") 'evil-ex-search-backward)
             (define-key evil-hybrid-state-local-map (kbd "n") 'evil-ex-search-next)
             (define-key evil-hybrid-state-local-map (kbd "N") 'evil-ex-search-previous)

             (define-key evil-emacs-state-local-map  (kbd ":") 'evil-ex)
             (define-key evil-emacs-state-local-map  (kbd "G") 'evil-goto-line)
             (define-key evil-emacs-state-local-map  (kbd "m") 'evil-set-marker)
             (define-key evil-emacs-state-local-map  (kbd "'") 'evil-goto-mark-line)
             (define-key evil-emacs-state-local-map  (kbd "/") 'evil-ex-search-forward)
             (define-key evil-emacs-state-local-map  (kbd "p") 'evil-ex-search-backward)
             (define-key evil-emacs-state-local-map  (kbd "n") 'evil-ex-search-next)
             (define-key evil-emacs-state-local-map  (kbd "N") 'evil-ex-search-previous)))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key evil-normal-state-local-map (kbd "<escape>") 'evil-insert)
             (define-key evil-normal-state-local-map (kbd "k") 'evil-previous-line)
             (define-key evil-normal-state-local-map (kbd "j") 'evil-next-line)
             (define-key evil-normal-state-local-map (kbd "h") 'evil-backward-char)))

(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)

(progn
  (define-prefix-command 'mwim-leader-key)
  (define-key evil-insert-state-map (kbd "C-w") mwim-leader-key))

(define-key evil-insert-state-map (kbd "C-w C-e") 'mwim-end-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-w C-a") 'mwim-beginning-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-w C-e") 'mwim-end-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-w C-a") 'mwim-beginning-of-code-or-line)

;; change emacs state to hybrid hstate
(define-key evil-hybrid-state-map (kbd "C-z") 'evil-exit-hybrid-state)
(define-key evil-insert-state-map (kbd "C-z") 'evil-hybrid-state)
(define-key evil-motion-state-map (kbd "C-z") 'evil-hybrid-state)
(define-key evil-emacs-state-map  (kbd "<escape>") 'evil-normal-state)

;; set eval-jump in normal
(define-key evil-normal-state-map (kbd "C-k") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "C-j") 'evil-jump-forward)

;; switch to another state temporarily
(define-key evil-emacs-state-map   (kbd "C-o") 'evil-execute-in-normal-state)
(define-key evil-hybrid-state-map  (kbd "C-o") 'evil-execute-in-normal-state)
(define-key evil-normal-state-map  (kbd "C-o") 'evil-execute-in-insert-state)
(define-key evil-replace-state-map (kbd "C-o") 'evil-execute-in-insert-state)

;; create new line in normal
(define-key evil-normal-state-map (kbd "<C-return>") 'newline-and-indent)

;;;; set evil-open-below in insert
(global-set-key (kbd "<C-return>") 'evil-newline-below)
(global-set-key (kbd "<C-S-return>") 'evil-newline-above)

;; set evil-shift
(define-key evil-insert-state-map (kbd "C-S-t") 'evil-shift-left-line)

;; delete four spaces and semi in c, c++, java , octave, matlab mode.
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-map (kbd ";") 'self-insert-command)
             (define-key c-mode-map (kbd "<backspace>") 'python-indent-dedent-line-backspace)))

(add-hook 'c++-mode-hook
          '(lambda ()
             (define-key c++-mode-map (kbd ";") 'self-insert-command)
             (define-key c++-mode-map (kbd "<backspace>") 'python-indent-dedent-line-backspace)))

(add-hook 'java-mode-hook
          '(lambda ()
             (define-key java-mode-map (kbd ";") 'self-insert-command)
             (define-key java-mode-map (kbd "<backspace>") 'python-indent-dedent-line-backspace)))

(add-hook 'matlab-mode-hook
          '(lambda ()
             (define-key matlab-mode-map (kbd "<backspace>") 'python-indent-dedent-line-backspace)))

(add-hook 'octave-mode-hook
          '(lambda ()
             (define-key octave-mode-map (kbd "<backspace>") 'python-indent-dedent-line-backspace)))

;; define web-mode
(add-hook 'web-mode-hook
          '(lambda ()
             (define-key evil-insert-state-local-map (kbd "<") 'noah/complete-apex-beackets)
             (define-key evil-insert-state-local-map (kbd ">") 'noah/close-apex-beackets)
             (define-key evil-insert-state-local-map (kbd "<backspace>") 'noah/delete-backward-char)))

;; define html-mode
(add-hook 'html-mode-hook
          '(lambda ()
             (define-key evil-insert-state-local-map (kbd "<") 'noah/complete-apex-beackets)
             (define-key evil-insert-state-local-map (kbd ">") 'noah/close-apex-beackets)
             (define-key evil-insert-state-local-map (kbd "<backspace>") 'noah/delete-backward-char)))


;; define racket-mode
(add-hook 'racket-mode-hook
          '(lambda ()
             (define-key evil-hybrid-state-local-map (kbd "C-x C-e") 'racket-better-send-last-sexp)
             (define-key evil-insert-state-local-map (kbd "C-x C-e") 'racket-better-send-last-sexp)
             (define-key evil-motion-state-local-map (kbd "C-x C-e") 'racket-better-send-last-sexp)
             (define-key evil-emacs-state-local-map  (kbd "C-x C-e") 'racket-better-send-last-sexp)
             (define-key racket-mode-map (kbd "C-x C-e") 'racket-better-send-last-sexp)))


(add-hook 'racket-repl-mode-hook
          '(lambda ()
             (define-key evil-insert-state-local-map (kbd "<C-return>") 'newline-and-indent)
             (define-key racket-repl-mode-map (kbd "C-w") 'nil)
             (define-key racket-repl-mode-map (kbd "C-x C-e") 'racket-repl-send-last-sexp)))

(add-hook 'geiser-repl-mode-hook
          '(lambda ()
             (define-key evil-insert-state-local-map (kbd "<C-return>") 'newline-and-indent)
             (define-key geiser-repl-mode-map (kbd "C-d") 'nil)
             (define-key geiser-repl-mode-map (kbd "C-x C-e") 'racket-repl-send-last-sexp)))


;; define delete
(define-key evil-insert-state-map (kbd "C-D") 'nil)
(define-key evil-insert-state-map (kbd "C-S-d") 'nil)
(define-key evil-insert-state-map (kbd "C-d") 'nil)
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "M-S-d") 'backward-kill-word)
(define-key clean-aindent-mode--keymap (kbd "M-<backspace>") 'kill-word)
(define-key evil-ex-completion-map (kbd "C-d") 'nil)
(global-set-key (kbd "C-D") 'delete-backward-char)
(global-set-key (kbd "C-S-d") 'delete-backward-char)
(global-set-key (kbd "C-d") 'delete-char)
(define-key evil-normal-state-map (kbd "x") 'delete-char)
(define-key evil-normal-state-map (kbd "X") 'delete-backward-char)

;; add yasnippet
(define-key evil-emacs-state-map  (kbd "C-q") 'company-yasnippet)
(define-key evil-hybrid-state-map (kbd "C-q") 'company-yasnippet)
(define-key evil-insert-state-map (kbd "C-q") 'company-yasnippet)
(global-set-key (kbd "C-c y") 'company-yasnippet)

;; set hs-toggle-hiding
(global-set-key (kbd "<f4>") 'hs-toggle-hiding)

;; set eshell
(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key evil-emacs-state-local-map  (kbd "C-k") 'eshell-previous-matching-input-from-input)
             (define-key evil-emacs-state-local-map  (kbd "C-j") 'eshell-next-matching-input-from-input)
             (define-key evil-hybrid-state-local-map (kbd "C-k") 'eshell-previous-matching-input-from-input)
             (define-key evil-hybrid-state-local-map (kbd "C-j") 'eshell-next-matching-input-from-input)
             (define-key evil-insert-state-local-map (kbd "C-k") 'eshell-previous-matching-input-from-input)
             (define-key evil-insert-state-local-map (kbd "C-j") 'eshell-next-matching-input-from-input)
             (define-key evil-normal-state-local-map (kbd "C-k") 'eshell-previous-matching-input-from-input)
             (define-key evil-normal-state-local-map (kbd "C-j") 'eshell-next-matching-input-from-input)
             (define-key evil-normal-state-local-map (kbd "<return>") 'eshell-send-input)))

;; set term
(add-hook 'term-mode-hook
          '(lambda ()
             (define-key evil-motion-state-local-map (kbd "RET") 'term-send-raw)))

;; set magit
(add-hook 'magit-status-mode-hook
          '(lambda ()
             (define-key magit-status-mode-map (kbd "1") 'nil)
             (define-key magit-status-mode-map (kbd "2") 'nil)
             (define-key magit-status-mode-map (kbd "3") 'nil)
             (define-key magit-status-mode-map (kbd "4") 'nil)

             (define-key magit-status-mode-map (kbd "M-1") 'nil)
             (define-key magit-status-mode-map (kbd "M-2") 'nil)
             (define-key magit-status-mode-map (kbd "M-3") 'nil)
             (define-key magit-status-mode-map (kbd "M-4") 'nil)

             (define-key magit-status-mode-map (kbd "C-1") 'magit-section-show-level-1)
             (define-key magit-status-mode-map (kbd "C-2") 'magit-section-show-level-2)
             (define-key magit-status-mode-map (kbd "C-3") 'magit-section-show-level-3)
             (define-key magit-status-mode-map (kbd "C-4") 'magit-section-show-level-4)

             (define-key magit-status-mode-map (kbd "C-M-1") 'magit-section-show-level-1-all)
             (define-key magit-status-mode-map (kbd "C-M-2") 'magit-section-show-level-2-all)
             (define-key magit-status-mode-map (kbd "C-M-3") 'magit-section-show-level-3-all)
             (define-key magit-status-mode-map (kbd "C-M-4") 'magit-section-show-level-4-all)))

(add-hook 'magit-mode-hook
          '(lambda ()
             (define-key magit-mode-map (kbd "1") 'nil)
             (define-key magit-mode-map (kbd "2") 'nil)
             (define-key magit-mode-map (kbd "3") 'nil)
             (define-key magit-mode-map (kbd "4") 'nil)

             (define-key magit-mode-map (kbd "M-1") 'nil)
             (define-key magit-mode-map (kbd "M-2") 'nil)
             (define-key magit-mode-map (kbd "M-3") 'nil)
             (define-key magit-mode-map (kbd "M-4") 'nil)

             (define-key magit-mode-map (kbd "C-1") 'magit-section-show-level-1)
             (define-key magit-mode-map (kbd "C-2") 'magit-section-show-level-2)
             (define-key magit-mode-map (kbd "C-3") 'magit-section-show-level-3)
             (define-key magit-mode-map (kbd "C-4") 'magit-section-show-level-4)

             (define-key magit-mode-map (kbd "C-M-1") 'magit-section-show-level-1-all)
             (define-key magit-mode-map (kbd "C-M-2") 'magit-section-show-level-2-all)
             (define-key magit-mode-map (kbd "C-M-3") 'magit-section-show-level-3-all)
             (define-key magit-mode-map (kbd "C-M-4") 'magit-section-show-level-4-all)))


;; set transparency
(global-set-key (kbd "<f11>") 'noah/loop-alpha-down)
(global-set-key (kbd "<f12>") 'noah/loop-alpha-up)

;; set vline-mode
(spacemacs/set-leader-keys "t C-v" 'vline-mode)

;; set occur-dwim
(spacemacs/set-leader-keys "o d" 'occur-dwim)

;; set youdao translation
(global-set-key (kbd "<f1>") 'youdao-search-at-point-or-from-input)

;; set company-complete
(add-hook 'company-search-mode-hook
          '(lambda ()
             (define-key company-search-map (kbd "C-l") 'company-complete)))

;; set smex-mode
(spacemacs/set-leader-keys "M-m" 'spacemacs/smex)

;; set symon-mode
(spacemacs/set-leader-keys "t m s" 'symon-mode)

;; set tabbar-mode
(spacemacs/set-leader-keys "t t" 'tabbar-mode)

;; set winum-select-window-9
(spacemacs/set-leader-keys "0" 'winum-select-window-0-or-10)
(define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)



;;; keybindings.el ends here
