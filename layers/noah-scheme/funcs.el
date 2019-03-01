;;; configs.el --- noah-scheme layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author:  <noah@Archier>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; define basic functions in scheme.
(defmacro define (name &rest body)
  "Same with the define in scheme."
  (cond
   ;; (define (f) ...)
   ((listp name)
    `(defun ,(car name) ,(cdr name)
       ,@body))
   ;; (define f (λ x ...))
   ((and
     (listp (car body))
     (listp (cadr body))
     (or (string-equal (caar body) 'lambda)
         (string-equal (caar body) 'λ)))
    `(defun ,name ,(cadar body)
       ,@(cddar body)))
   ;; (define a b)
   ('t
    `(defvar ,name ,@body))))

(defalias 'λ 'lambda "λ is an alias for ‘lambda’.")
(defalias '-λ '-lambda "-λ is an alias for ‘-lambda’.")

(defalias 'set! 'setq "set! is an alias for ‘setq’.")
(defalias 'set-car! 'setcar "set-car! is an alias for ‘setcar’.")
(defalias 'set-cdr! 'setcdr "set-cdr! is an alias for ‘setcdr’.")

(defalias 'eq? 'eq "eq? is an alias for ‘eq’.")
(defalias 'equal? 'equal "equal? is an alias for ‘equal’.")
(defalias 'list? 'listp "list? is an alias for ‘listp’.")
(defalias 'symbol? 'symbolp "symbol? is an alias for ‘symbolp’.")
(defalias 'number? 'numberp "number? is an alias for ‘numberp’.")
(defalias 'string? 'stringp "string? is an alias for ‘stringp’.")
(defalias 'pair? 'consp "pair? is an alias for ‘consp’.")

(defalias 'string->list 'string-to-list "string->list is an alias for ‘string-to-list’.")
(defalias 'string->number 'string-to-number "string->number is an alias for ‘string-to-number’.")
(defalias 'number->string 'number-to-string "number->string is an alias for ‘number-to-string’.")

(defalias 'remainder '% "remainder is an alias for ‘%’.")



;;; funcs.el ends here
