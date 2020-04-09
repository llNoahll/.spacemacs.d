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
(global-prettify-symbols-mode t)

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
          (lambda ()
            (member major-mode '(emacs-lisp-mode scheme-mode))))

;; only enable cc-mode
;; (setq semantic-inhibit-functions
;;       (lambda ()
;;          (not (and (featurep 'cc-defs)
;;                    c-buffer-is-cc-mode))))

;; set company-mode
(setq company-show-numbers t)
(add-hook 'company-mode-hook
          (lambda ()
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
          (lambda ()
            (setq-local require-final-newline nil)
            (company-mode t)
            (highlight-indent-guides-mode t)))

;; set eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local evil-move-cursor-back nil)
            (smartparens-mode t)))

;; set company for text-mode
(add-hook 'text-mode-hook
          (lambda ()
            (setq-local require-final-newline nil)
            (company-mode t)))

;; set magit-status-mode
(add-hook 'magit-status-mode-hook
          (lambda ()
            (display-line-numbers-mode t)))

;; set eww
(add-hook 'eww-after-render-hook
          (lambda ()
            (setq-local truncate-lines nil)
            (vline-mode t)))

;; set treemacs-mode
(add-hook 'treemacs-mode-hook
          (lambda ()
            (text-scale-set -2)
            (vline-mode t)
            (highlight-indentation-mode t)
            ;; (treemacs-create-icon :file "scheme.png"
            ;;                       :extensions ("ss" "scm" "sls" "sps"))
            ;; (treemacs-create-icon :file "vsc/vim.png"
            ;;                       :extensions ("vim" "vimrc" "_vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"))
            ))

;; set c-mode
(add-hook 'c-mode-hook
          (lambda ()
            (setq-local comment-start "// ")
            (setq-local comment-end "")))

;; set c++-mode
(add-hook 'c++-mode-hook
          (lambda ()
             (setq-local comment-start "/* ")
             (setq-local comment-end   " */")
             (flycheck-mode t)))

;; ;; set java-mode
;; (add-hook 'java-mode-hook
;;           (lambda ()
;;              (setq-local comment-start "/* ")
;;              (setq-local comment-end   " */")))

;; set css-mode
(add-hook 'css-mode-hook
          (lambda ()
            (setq-local comment-start "/* ")
            (setq-local comment-end   " */")))

;; set lisp-mode
(add-hook 'lisp-mode-hook
          (lambda ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (setq-local comment-start "; ")
            (setq-local prettify-symbols-alist '(("lambda" . ?λ)))))

;; set common-lisp-mode
(add-hook 'common-lisp-mode-hook
          (lambda ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (setq-local prettify-symbols-alist '(("lambda" . ?λ)))))

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
          (lambda ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (setq-local comment-start "; ")
            (setq-local prettify-symbols-alist '(("lambda" . ?λ)))))

;; set iell-mode
(add-hook 'ielm-mode-hook
          (lambda ()
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

                                           "vector" "vector?"
                                           "vector-length" "vector-ref"
                                           "vector->immutable-vector" "vector->list"
                                           "vector-set!" "vector-sort!" "vector-fill!"
                                           "vector-map" "vector-sort" "vector-copy"

                                           "box" "box-immutable" "box?"
                                           "set-box!" "box-cas!"

                                           "cons" "pair?" "last-pair"
                                           "set-car!" "set-cdr!"
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

                                           "zero?" "sub1" "add1" "1+" "1-" "-1+"
                                           "true" "false" "true?" "false?"
                                           "not" "xor" "nor" "nand" "implies")
                                         t)
                                    "\\>")
                           . font-lock-builtin-face))
                        t)

;; set indent for scheme
(put 'case-λ 'scheme-indent-function 0)

(add-hook 'scheme-mode-hook
          (lambda ()
            ;; (paredit-mode)
            ;; (flycheck-mode t)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (setq-local comment-start "; ")
            (setq-local prettify-symbols-alist '(("lambda" . ?λ)
                                                 ("case-lambda" .
                                                  (?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl) ?λ))))))

;; setq scheme's default geiser-implementations
(setq geiser-implementations-alist
      '(((regexp "\\.scm$")
         chez)
        ((regexp "\\.ss$")
         chez)))

;; set racket-mode
(font-lock-add-keywords 'racket-mode
                        `((,(regexp-opt '(":-" "?" "~" "!="
                                          ;; "datalog" "datalog!"
                                          ;; "make-theory" "write-theory" "read-theory" "theory/c"

                                          "case-λ:" "λ:" "opt-λ:" "pcase-λ:" "pλ:" "popt-λ:"
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
                                          "stream-cdddar" "stream-cddddr"

                                          "serializable?" "serialize" "deserialize" "serialized=?"
                                          "deserialize-module-guard"
                                          "serializable-struct"
                                          "define-serializable-struct"
                                          "serializable-struct/versions"
                                          "define-serializable-struct/versions"
                                          "make-deserialize-info"
                                          "prop:serializable"
                                          "make-serialize-info"

                                          "s-exp->fasl" "fasl->s-exp")
                                        'symbols)
                           . font-lock-builtin-face)
                          (,(regexp-opt '("eval" "case-λ" "amb") 'symbols) . font-lock-keyword-face))
                        t)

;; set indent for racket
(mapc (lambda (x)
        (put (car x) 'racket-indent-function (cadr x))
        (let ((typed (intern (format "%s:" (car x)))))
          (put typed 'racket-indent-function (cadr x))))
      '((for/stream racket--indent-for)
        (case-λ 0)))

(add-hook 'racket-mode-hook
          (lambda ()
            ;; (paredit-mode)
            ;; (flycheck-mode t)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (geiser-mode t)
            (setq-local comment-start "; ")
            (setq-local prettify-symbols-alist '(("lambda" . ?λ)
                                                 ("case-lambda" .
                                                  (?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl)
                                                      ?λ))
                                                 ("match-lambda" .
                                                  (?m (Br . Bl) ?a (Br . Bl) ?t (Br . Bl) ?c (Br . Bl) ?h (Br . Bl) ?- (Br . Bl)
                                                      ?λ))
                                                 ("match-lambda*" .
                                                  (?m (Br . Bl) ?a (Br . Bl) ?t (Br . Bl) ?c (Br . Bl) ?h (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?*))
                                                 ("match-lambda**" .
                                                  (?m (Br . Bl) ?a (Br . Bl) ?t (Br . Bl) ?c (Br . Bl) ?h (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?* (Br . Bl) ?*))
                                                 ("lambda:" .
                                                  (?λ (Br . Bl) ?:))
                                                 ("case-lambda:" .
                                                  (?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?:))
                                                 ("opt-lambda:" .
                                                  (?o (Br . Bl) ?p (Br . Bl) ?t (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?:))
                                                 ("pcase-lambda:" .
                                                  (?p (Br . Bl) ?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?:))
                                                 ("plambda:" .
                                                  (?p (Br . Bl)
                                                      ?λ (Br . Bl) ?:))
                                                 ("popt-lambda:" .
                                                  (?p (Br . Bl) ?o (Br . Bl) ?p (Br . Bl) ?t (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?:))))))

(add-hook 'racket-repl-mode-hook
          (lambda ()
            ;; (paredit-mode)
            (evil-paredit-mode)
            (evil-cleverparens-mode)
            (evil-cp-redefine-keys)
            (geiser-mode t)
            (setq-local evil-move-cursor-back nil)
            (setq-local comment-start "; ")
            (setq-local prettify-symbols-alist '(("lambda" . ?λ)
                                                 ("case-lambda" .
                                                  (?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl)
                                                      ?λ))
                                                 ("match-lambda" .
                                                  (?m (Br . Bl) ?a (Br . Bl) ?t (Br . Bl) ?c (Br . Bl) ?h (Br . Bl) ?- (Br . Bl)
                                                      ?λ))
                                                 ("match-lambda*" .
                                                  (?m (Br . Bl) ?a (Br . Bl) ?t (Br . Bl) ?c (Br . Bl) ?h (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?*))
                                                 ("match-lambda**" .
                                                  (?m (Br . Bl) ?a (Br . Bl) ?t (Br . Bl) ?c (Br . Bl) ?h (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?* (Br . Bl) ?*))
                                                 ("case-lambda:" .
                                                  (?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?:))
                                                 ("opt-lambda:" .
                                                  (?o (Br . Bl) ?p (Br . Bl) ?t (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?:))
                                                 ("pcase-lambda:" .
                                                  (?p (Br . Bl) ?c (Br . Bl) ?a (Br . Bl) ?s (Br . Bl) ?e (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?:))
                                                 ("plambda:" .
                                                  (?p (Br . Bl)
                                                      ?λ (Br . Bl) ?:))
                                                 ("popt-lambda:" .
                                                  (?p (Br . Bl) ?o (Br . Bl) ?p (Br . Bl) ?t (Br . Bl) ?- (Br . Bl)
                                                      ?λ (Br . Bl) ?:))))))


;; define octave-mode
(add-hook 'octave-mode-hook
          (lambda ()
            (setq-local comment-start "% ")))

(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (company-mode t)))


;; define matlab-mode
(add-hook 'matlab-mode-hook
          (lambda ()
            (setq-local yas-indent-line 'fixed)
            (setq-local comment-add 1)
            (setq-local evil-shift-width 4)
            (setq-local comment-start "% ")))

;; set LaTeX-mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq-local yas-indent-line 'fixed)
            (setq-local comment-add 1)
            (setq-local comment-start "% ")
            (rainbow-delimiters-mode t)))


;; set asm-mode
(add-hook 'asm-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (setq-local indent-line-function 'indent-relative)
            (setq-local comment-start "; ")))

;; set nasm-mode
(add-hook 'nasm-mode-hook
          (lambda ()
            (setq-local comment-start "; ")))


;; set verilog-mode
(add-hook 'verilog-mode-hook
          (lambda ()
            (setq-local verilog-align-ifelse t)
            (setq-local verilog-auto-newline nil)
            (setq-local verilog-auto-delete-trailing-whitespace nil)

            (setq-local verilog-highlight-grouping-keywords t)
            (setq-local verilog-highlight-modules t)

            (setq-local verilog-cexp-indent 0)
            (setq-local verilog-case-indent 4)
            (setq-local verilog-indent-level 4)
            (setq-local verilog-indent-level-behavioral 4)
            (setq-local verilog-indent-level-module 4)
            (setq-local verilog-indent-level-declaration 4)
            (setq-local tab-width 4)

            (flycheck-mode t)))


;; set python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local comment-add 1)
            (setq-local yas-indent-line 'fixed)))

;; set ein:ml-mode
(add-hook 'ein:notebook-multilang-mode-hook
          (lambda ()
            (smartparens-mode t)
            (setq-local comment-add 1)
            (setq-local evil-shift-width 4)
            (setq-local yas-indent-line 'fixed)
            (yas-activate-extra-mode 'python-mode)))


;; define web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?\< . ?\>))))))


;; define html-mode
(add-hook 'html-mode-hook
          (lambda ()
            (setq-local electric-pair-pairs
                        (append electric-pair-pairs '((?\< . ?\>))))))

;; set spelling-checking
(add-hook 'text-mode-hook
          (lambda ()
            (quote spacemacs/toggle-spelling-checking-on)))

;; set org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local truncate-lines nil)))
;; font-lock-mode in org file
(setq-default org-src-fontify-natively t)



;;; config.el ends here
