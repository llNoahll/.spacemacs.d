;;; config.el --- noah-better-defaults layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author:  <noah@Archier>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;;; add packages
;; (add-to-list 'load-path "~/.spacemacs.d/packages/nyan-mode/")


;;;; require packages
(require 'python)

;; set the width of line number
(text-scale-set -8)

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
(add-hook 'spacemacs-indent-sensitive-modes 'asm-mode)
(add-hook 'spacemacs-indent-sensitive-modes 'masm-mode)

;; add auto-mode-lists
(add-to-list 'auto-mode-alist '("\\.sls\\'"       . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sps\\'"       . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.snippet\\'"   . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.scrbl\\'"     . racket-mode))
(add-to-list 'auto-mode-alist '("\\.rashrc\\'"    . racket-mode))
(add-to-list 'auto-mode-alist '("\\.xprofile\\'"  . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.mat\\'"       . matlab-mode))
(add-to-list 'auto-mode-alist '("\\.com\\'"       . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.bin\\'"       . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.exe\\'"       . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'"       . hexl-mode))

;; open binary file in hexl-mode.
(add-hook 'find-file-hooks 'hexl-if-binary)

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
          (λ ()
            (member major-mode '(emacs-lisp-mode scheme-mode))))

;; only enable cc-mode
;; (setq semantic-inhibit-functions
;;       (λ ()
;;          (not (and (featurep 'cc-defs)
;;                    c-buffer-is-cc-mode))))

;; set company-mode
(setq company-show-numbers t)
(add-hook 'company-mode-hook
          (λ ()
            ;; ;; Use the tab-and-go frontend.
            ;; ;; Allows TAB to select and complete at the same time.
            ;; (company-tng-configure-default)
            ;; (setq company-frontends
            ;;       '(company-tng-frontend
            ;;         company-pseudo-tooltip-frontend
            ;;         company-echo-metadata-frontend))
            (setq company-idle-delay 0)))

;; set default configs in prog-mode
(add-hook 'prog-mode-hook
          (λ ()
            (setq-local require-final-newline nil)
            (company-mode t)
            (highlight-indent-guides-mode t)))

;; set eshell
(add-hook 'eshell-mode-hook
          (λ ()
            (setq-local evil-move-cursor-back nil)
            (smartparens-mode t)))

;; set company for text-mode
(add-hook 'text-mode-hook
          (λ ()
            (setq-local require-final-newline nil)
            (company-mode t)))

;; set magit-status-mode
(add-hook 'magit-status-mode-hook
          (λ ()
            (display-line-numbers-mode t)))

;; set eww
(add-hook 'eww-after-render-hook
          (λ ()
            (setq-local truncate-lines nil)
            (vline-mode t)))

;; set treemacs-mode
(add-hook 'treemacs-mode-hook
          (λ ()
            (text-scale-set -2)
            (vline-mode t)
            (highlight-indentation-mode t)

            (treemacs-create-icon :file "~/.spacemacs.d/packages/treemacs/icons/tex.png"
                                  :extensions ("tex"))
            (treemacs-create-icon :file "~/.spacemacs.d/packages/treemacs/icons/scheme.png"
                                  :extensions ("ss" "scm" "sls" "sps"))
            (treemacs-create-icon :file "~/.spacemacs.d/packages/treemacs/icons/racket.png"
                                  :extensions ("racket" "rkt" "rktl" "rktd" "scrbl" "scribble" "plt"))
            (treemacs-create-icon :file "~/.spacemacs.d/packages/treemacs/icons/vsc/vim.png"
                                  :extensions ("vim" "vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"))))

;; set c-mode
(add-hook 'c-mode-hook
          (λ ()
            (setq-local comment-start "// ")
            (setq-local comment-end "")))

;; ;; set c++-mode
;; (add-hook 'c++-mode-hook
;;           (λ ()
;;              (setq-local comment-start "/* ")
;;              (setq-local comment-end   " */")))

;; ;; set java-mode
;; (add-hook 'java-mode-hook
;;           (λ ()
;;              (setq-local comment-start "/* ")
;;              (setq-local comment-end   " */")))

;; set css-mode
(add-hook 'css-mode-hook
          (λ ()
            (setq-local comment-start "/* ")
            (setq-local comment-end   " */")))

;; set lisp-mode
(add-hook 'lisp-mode-hook
          (λ ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (setq-local comment-start "; ")))

;; set common-lisp-mode
(add-hook 'common-lisp-mode-hook
          (λ ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)))

;; set emacs-lisp-mode
(font-lock-add-keywords 'emacs-lisp-mode
                        `((,(regexp-opt '("else" "_" "...") 'symbols)
                           . font-lock-keyword-face)
                          (,(regexp-opt '("+" "-" "*" "/"
                                          "<" ">" "=" "<=" ">="
                                          "nil" "t") 'symbols)
                           . font-lock-builtin-face)
                          (,(rx (or
                                 ;; symbol
                                 (seq ?' ?| (+ any) ?|)
                                 (seq ?' (1+ (or (syntax word) (syntax symbol))))
                                 (seq "#\\" (1+ (or (syntax word) (syntax symbol))))))
                           . font-lock-string-face)
                          (,(concat "(" (regexp-opt '("eval" "apply" "map") t) "\\>")
                           . font-lock-keyword-face)
                          (,(concat "(" (regexp-opt
                                         '("setcar" "setcdr"
                                           "cons" "consp"
                                           "car" "cdr"
                                           "caar" "cadr"
                                           "cdar" "cddr"
                                           "caaar" "caadr"
                                           "cadar" "caddr"
                                           "cdaar" "cdadr"
                                           "cddar" "cdddr"
                                           "caaaar" "caaadr"
                                           "caadar" "caaddr"
                                           "cadaar" "cadadr"
                                           "caddar" "cadddr"
                                           "cdaaar" "cdaadr"
                                           "cdadar" "cdaddr"
                                           "cddaar" "cddadr"
                                           "cdddar" "cddddr"
                                           "list" "list*" "listp" "null"
                                           "append" "reverse"
                                           "true" "false"
                                           "not" "xor")
                                         t)
                                    "\\>")
                           . font-lock-builtin-face))
                        t)
(add-hook 'emacs-lisp-mode-hook
          (λ ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (setq-local comment-start "; ")))

;; set iell-mode
(add-hook 'ielm-mode-hook
          (λ ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (setq-local comment-start "; ")))

;; set scheme-mode
(font-lock-add-keywords 'scheme-mode
                        `((,(regexp-opt '("else" "_" "...") 'symbols) . font-lock-keyword-face)
                          (,(regexp-opt '("+" "-" "*" "/"
                                          "<" ">" "=" "<=" ">=") 'symbols)
                           . font-lock-builtin-face)
                          (,(regexp-opt '("#t" "#f" "+inf.0" "-inf.0" "+nan.0") 'symbols)
                           . font-lock-string-face)
                          (,(rx (or
                                 ;; symbol
                                 (seq ?' ?| (+ any) ?|)
                                 (seq ?' (1+ (or (syntax word) (syntax symbol))))
                                 (seq "#\\" (1+ (or (syntax word) (syntax symbol))))))
                           . font-lock-string-face)

                          ;; Numeric literals including Scheme reader hash prefixes.
                          (,(rx
                             (seq symbol-start
                                  (or
                                   ;; #d #e #i or no hash prefix
                                   (seq (? "#" (any "dei"))
                                        (or (seq (? (any "-+"))
                                                 (1+ digit)
                                                 (? (any "./") (1+ digit)))
                                            (seq (1+ digit)
                                                 ?e
                                                 (? (any "-+"))
                                                 (1+ digit))))
                                   ;; #x
                                   (seq "#x"
                                        (? (any "-+"))
                                        (1+ hex-digit)
                                        (? (any "./") (1+ hex-digit)))
                                   ;; #b
                                   (seq "#b"
                                        (or (seq (? (any "-+"))
                                                 (1+ (any "01"))
                                                 (? (any "./") (1+ (any "01"))))
                                            (seq (1+ (any "01"))
                                                 ?e
                                                 (? (any "-+"))
                                                 (1+ (any "01")))))
                                   ;; #o
                                   (seq "#o"
                                        (or (seq (? (any "-+"))
                                                 (1+ (any "0-7"))
                                                 (? (any "./") (1+ (any "0-7"))))
                                            (seq (1+ (any "0-7"))
                                                 ?e
                                                 (? (any "-+"))
                                                 (1+ (any "0-7"))))))
                                  symbol-end))
                           . font-lock-keyword-face)
                          (,(concat "(" (regexp-opt
                                         '("set!"
                                           "eval" "apply"
                                           "amb"
                                           "case-λ"
                                           "filter") t) "\\>")
                           . font-lock-keyword-face)
                          (,(concat "(" (regexp-opt
                                         '("exit" "error" "format"
                                           "eq?" "eqv?" "equal?"
                                           "display" "displayln"
                                           "write" "writeln"
                                           "print" "printf" "println" "fprintf"
                                           "newline"
                                           "read" "read-char"
                                           "close-port" "close-input-port" "close-output-port"
                                           "open-bytevector-output-port"
                                           "open-bytevector-input-port"
                                           "open-fd-output-port" "open-fd-input-port"
                                           "open-fd-input/output-port"
                                           "open-source-file"
                                           "open-input-file" "open-output-file"
                                           "open-input-output-file"
                                           "open-file-input-port" "open-file-output-port"
                                           "open-file-input/output-port"
                                           "open-output-string"
                                           "open-input-string"
                                           "open-string-input-port"
                                           "open-string-output-port"
                                           "open-process-ports"
                                           "assert" "assoc"
                                           "assp" "assq" "assv"
                                           "assertion-violation" "assertion-violation?"
                                           "assertion-violationf"
                                           "number?"
                                           "number->string"
                                           "numerator"
                                           "symbol?"
                                           "symbol=?"
                                           "symbol->string"
                                           "string" "string?"
                                           "string-length" "string-ref"
                                           "string-copy" "string-copy!" "string-append"
                                           "string->list" "string->immutable-string"
                                           "string->number" "string->symbol"
                                           "string-normalize-nfc" "string-normalize-nfd"
                                           "string-normalize-nfkc" "string-normalize-nfkd"
                                           "string-upcase" "string-downcase"
                                           "string-foldcase" "string-titlecase"
                                           "string-ci=?"
                                           "string-ci<=?" "string-ci<?"
                                           "string-ci>=?" "string-ci>?"
                                           "string=?" "string<=?" "string>=?"
                                           "string<?" "string>?"
                                           "string-set!" "string-fill!"
                                           "set-car!" "set-cdr!"
                                           "vector" "vector?"
                                           "vector-length" "vector-ref"
                                           "vector->immutable-vector" "vector->list"
                                           "vector-set!" "vector-sort!" "vector-fill!"
                                           "vector-map" "vector-sort" "vector-copy"
                                           "box" "box-immutable" "box?"
                                           "set-box!" "box-cas!"
                                           "cons" "pair?" "last-pair"
                                           "car" "cdr"
                                           "caar" "cadr"
                                           "cdar" "cddr"
                                           "caaar" "caadr"
                                           "cadar" "caddr"
                                           "cdaar" "cdadr"
                                           "cddar" "cdddr"
                                           "caaaar" "caaadr"
                                           "caadar" "caaddr"
                                           "cadaar" "cadadr"
                                           "caddar" "cadddr"
                                           "cdaaar" "cdaadr"
                                           "cdadar" "cdaddr"
                                           "cddaar" "cddadr"
                                           "cdddar" "cddddr"
                                           "list" "list*" "list?"
                                           "list->fxvector" "list->string" "list->vector"
                                           "list-copy" "list-sort" "list-tail"
                                           "list-ref" "list-head" "length"
                                           "append" "apeend!" "reverse" "reverse!"
                                           "empty" "null" "nil"
                                           "empty?" "null?" "nil?"
                                           "member"
                                           "memp" "memq" "memv"
                                           "remv" "remv!"
                                           "remp" "remq" "remq!"
                                           "remove" "remove!"
                                           "remainder"
                                           "sort" "sort!"
                                           "stream-cons" "stream-first" "stream-rest"
                                           "stream-car" "stream-cdr"
                                           "stream-caar" "stream-cadr"
                                           "stream-cdar" "stream-cddr"
                                           "stream-caaar" "stream-caadr"
                                           "stream-cadar" "stream-caddr"
                                           "stream-cdaar" "stream-cdadr"
                                           "stream-cddar" "stream-cdddr"
                                           "stream-caaaar" "stream-caaadr"
                                           "stream-caadar" "stream-caaddr"
                                           "stream-cadaar" "stream-cadadr"
                                           "stream-caddar" "stream-cadddr"
                                           "stream-cdaaar" "stream-cdaadr"
                                           "stream-cdadar" "stream-cdaddr"
                                           "stream-cddaar" "stream-cddadr"
                                           "stream-cdddar" "stream-cddddr"
                                           "stream" "stream*" "stream?"
                                           "stream-append" "stream-ref"
                                           "stream-map" "stream-filter"
                                           "empty-stream" "stream-empty?"
                                           "make-date"
                                           "make-parameter"
                                           "make-polar" "make-rectangular"
                                           "make-input-port" "make-output-port"
                                           "make-list" "make-vector" "make-string"
                                           "true" "false" "true?" "false?"
                                           "not" "xor" "nor" "nand" "implies")
                                         t)
                                    "\\>")
                           . font-lock-builtin-face))
                        t)

;; set indent for scheme
(put 'case-λ 'scheme-indent-function 0)

(add-hook 'scheme-mode-hook
          (λ ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (setq-local comment-start "; ")))

;; setq scheme's default geiser-implementations
(setq geiser-implementations-alist
      '(((regexp "\\.scm$")
         chez)
        ((regexp "\\.ss$")
         chez)))

;; set racket-mode
(font-lock-add-keywords 'racket-mode
                        `((,(regexp-opt '("case-λ:" "opt-λ:" "pcase-λ:" "pλ:" "popt-λ:"
                                          "match-λ" "match-λ*" "match-λ**"
                                          "true?"
                                          "nil" "nil?"
                                          "mcaar" "mcadr"
                                          "mcdar" "mcddr"
                                          "mcaaar" "mcaadr"
                                          "mcadar" "mcaddr"
                                          "mcdaar" "mcdadr"
                                          "mcddar" "mcdddr"
                                          "mcaaaar" "mcaaadr"
                                          "mcaadar" "mcaaddr"
                                          "mcadaar" "mcadadr"
                                          "mcaddar" "mcadddr"
                                          "mcdaaar" "mcdaadr"
                                          "mcdadar" "mcdaddr"
                                          "mcddaar" "mcddadr"
                                          "mcdddar" "mcddddr"
                                          "mlist?" "mlist" "mlist*" "mlistof"
                                          "list->mlist" "mlist->list"
                                          "mlength" "mlist-ref" "mlist-tail"
                                          "mappend" "mappend!"
                                          "mreverse" "mreverse!"
                                          "mmap" "mfor-each"
                                          "mmember" "mmemv" "mmemq" "massoc"
                                          "massv" "massq"
                                          "stream-car" "stream-cdr"
                                          "stream-caar" "stream-cadr"
                                          "stream-cdar" "stream-cddr"
                                          "stream-caaar" "stream-caadr"
                                          "stream-cadar" "stream-caddr"
                                          "stream-cdaar" "stream-cdadr"
                                          "stream-cddar" "stream-cdddr"
                                          "stream-caaaar" "stream-caaadr"
                                          "stream-caadar" "stream-caaddr"
                                          "stream-cadaar" "stream-cadadr"
                                          "stream-caddar" "stream-cadddr"
                                          "stream-cdaaar" "stream-cdaadr"
                                          "stream-cdadar" "stream-cdaddr"
                                          "stream-cddaar" "stream-cddadr"
                                          "stream-cdddar" "stream-cddddr")
                                        'symbols)
                           . font-lock-builtin-face)
                          (,(regexp-opt '("eval" "case-λ" "amb") 'symbols) . font-lock-keyword-face))
                        t)
(add-hook 'racket-mode-hook
          (λ ()
            (setq-local comment-start "; ")
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (geiser-mode t)

            (mapc (λ (x)
                    (put (car x) 'racket-indent-function (cadr x))
                    (let ((typed (intern (format "%s:" (car x)))))
                      (put typed 'racket-indent-function (cadr x))))
                  '((for/stream racket--indent-for)
                    (case-λ 0)))))

(add-hook 'racket-repl-mode-hook
          (λ ()
            (setq-local evil-move-cursor-back nil)
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (geiser-mode t)))


;; define octave-mode
(add-hook 'octave-mode-hook
          (λ ()
            (setq-local comment-start "% ")))

(add-hook 'inferior-octave-mode-hook
          (λ ()
            (company-mode t)))


;; define matlab-mode
(add-hook 'matlab-mode-hook
          (λ ()
            (setq-local yas-indent-line 'fixed)
            (setq-local comment-add 1)
            (setq-local evil-shift-width 4)
            (setq-local comment-start "% ")))

;; set LaTeX-mode
(add-hook 'LaTeX-mode-hook
          (λ ()
            (setq-local yas-indent-line 'fixed)
            (setq-local comment-add 1)
            (setq-local comment-start "% ")
            (rainbow-delimiters-mode t)))


;; set asm-mode
(add-hook 'asm-mode-hook
          (λ ()
            (setq-local tab-width 4)
            (setq-local indent-line-function 'indent-relative)
            (setq-local comment-start "; ")))

;; set nasm-mode
(add-hook 'nasm-mode-hook
          (λ ()
            (setq-local comment-start "; ")))


;; set python-mode
(add-hook 'python-mode-hook
          (λ ()
            (setq-local comment-add 1)
            (setq-local yas-indent-line 'fixed)))

;; set ein:ml-mode
(add-hook 'ein:notebook-multilang-mode-hook
          (λ ()
            (smartparens-mode t)
            (setq-local comment-add 1)
            (setq-local evil-shift-width 4)
            (setq-local yas-indent-line 'fixed)
            (yas-activate-extra-mode 'python-mode)))


;; define web-mode
(add-hook 'web-mode-hook
          (λ ()
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?\< . ?\>))))))


;; define html-mode
(add-hook 'html-mode-hook
          (λ ()
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?\< . ?\>))))))

;; set spelling-checking
(add-hook 'text-mode-hook
          (λ ()
            (quote spacemacs/toggle-spelling-checking-on)))

;; set org-mode
(add-hook 'org-mode-hook
          (λ ()
            (setq-local truncate-lines nil)))
;; font-lock-mode in org file
(setq-default org-src-fontify-natively t)



;;; config.el ends here
