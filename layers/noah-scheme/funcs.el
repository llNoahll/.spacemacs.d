;;; configs.el --- noah-scheme layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
    (append
     (list 'defun (car name) (cdr name))
     body))
   ;; (define f (λ x ...))
   ((and
     (listp (cadr body))
     (or (string-equal (caar body) 'lambda)
         (string-equal (caar body) 'λ)))
    (append
     (list 'defun name (cadar body))
     (cddar body)))
   ;; (define a b)
   ('t
    (append
     (list 'defvar name)
     body))))

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
(defalias 'string? 'stringp "string? is an alias for ‘strinfp’.")
(defalias 'pair? 'consp "pair? is an alias for ‘consp’.")


(defun circular (object)
  "Translate a list to a circular."
  (interactive)
  (when (listp object)
    (setcdr (last object) object)))
(defun circularp (object)
  "Return t if OBJECT is a circular.
Otherwise, return nil."
  (interactive)
  (eq (last object) (cdr object)))
(defalias 'circular? 'circularp "circular? is an alias for ‘circularp’.")


(defalias 'remainder '% "remainder is an alias for ‘%’.")



;;; funcs.el ends here
