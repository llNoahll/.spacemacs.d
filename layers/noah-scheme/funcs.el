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


;;; define basic functions of scheme.

(defmacro define-1 (name &optional docstring &rest body)
  "(define (f) A)"
  (unless (or (not docstring)
              (stringp docstring))
    (setq body (cons docstring body))
    (setq docstring 'nil))
  (when (and (cdr body)
             (not (cdr (cdr body))))
    (setq body (car body)))
  (list 'defun (car name) (cdr name)
        docstring
        (cons 'progn body)))

(defmacro define-2 (name &optional docstring &rest body)
  "(define f (lambda () ()))"
  (unless (or (not docstring)
              (stringp docstring))
    (setq body (cons docstring body))
    (setq docstring 'nil))
  (when (and (not (cdr body))
             (not (cdr (car body)))
             (cdr (car (car body))))
    (setq body (car body)))
  (list 'defun name (car (cdr (car body)))
        docstring
        (cons 'progn (cdr (cdr (car body))))))

(defmacro define-3 (name &optional docstring &rest body)
  "(define A B)"
  (unless (or (not docstring)
              (stringp docstring))
    (setq body (cons docstring body))
    (setq docstring 'nil))
  (when (and (cdr body)
             (not (cdr (cdr body))))
    (setq body (car body)))
  (defvar name '0 docstring)
  (list 'setq name
        (cons 'progn body)))

(defmacro define (name &optional docstring &rest body)
  "Same with the define in scheme."
  (unless (or (not docstring)
              (stringp docstring))
    (setq body (cons docstring body))
    (setq docstring 'nil))
  (cond ((listp name)
         (list 'define-1 name docstring (car body)))
        ((and
          (listp (car body))
          (or (string-equal (car (car body)) 'lambda)
              (string-equal (car (car body)) 'λ)))
         (list 'define-2 name docstring (car body)))
        ('t
         (list 'define-3 name docstring (car body)))))



;;; funcs.el ends here