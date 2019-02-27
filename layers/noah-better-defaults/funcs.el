;;; funcs.el --- noah-better-defaults layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author:  <noah@Archier>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; some basic funcs
(defun cons-to-list (object)
  "Convert a cons to a circular."
  (interactive)
  (when (consp object)
    (let ((end (last object)))
      (setcdr end (cdr (-cons-to-list end))))
    object))

(defalias 'pair->list 'cons-to-list "pair->list is an alias for ‘cons-to-list’.")


(defun circular (&rest object)
  "Return a newly created cycle
by a list."
  (interactive)
  (setcdr (last object) object)
  object)

(defun list-to-circular (object)
  "Convert a list to a circular."
  (interactive)
  (when (listp object)
    (setcdr (last object) object)))
(defalias 'list->circular 'list-to-circular "list->circular is an alias for ‘list-to-circular’.")


(defun circularp (object)
  "Return t if OBJECT is a circular.
Otherwise, return nil."
  (interactive)
  (cond ((listp (last object))
         (cond ((eq (last object) 'nil) 'nil)
               ('t (eq (last object) (cdr object))))
         ('t 'nil))))

(defalias 'circular? 'circularp "circular? is an alias for ‘circularp’.")


(defmacro better-last-sexp (last-sexp)
  (list 'progn
        '(cond ((or (looking-at ")\n")
                    (looking-at "]\n")
                    (looking-at "}\n"))
                (right-char 2))
               ((or (looking-at ")")
                    (looking-at "]")
                    (looking-at "}"))
                (right-char 1)))

        last-sexp

        '(cond ((looking-back "\n") (left-char 2))
               ((or (looking-at ")")
                    (looking-at "]")
                    (looking-at "}"))
                (left-char 1)))))



;;; defun new major-mode.
;;;###autoload
(define-derived-mode masm-mode asm-mode "MASM"
  "Major mode for masm.")



;;; defun noah/

;; Setting transparency
(defun noah/loop-alpha-down ()
  "Turn down the alpha"
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (cadr h))
    (setq alpha-list (cdr (append alpha-list (list h))))))

(defun noah/loop-alpha-up ()
  "Turn up the alpha"
  (interactive)
  (let ((g (car (last alpha-list 1)))
        (h (car (last alpha-list 2))))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (cadr h))
    (setq alpha-list (butlast (append (list g) alpha-list) 1))))

(defun noah/right-char (&optional n)
  "Move point N characters to the right (to the left if N is negative).
If right characters is \n, skip it."
  (interactive "^p")
  (right-char 1)
  (if (looking-at "\n")
      (right-char (+ n 1))
    (right-char n))
  (left-char 1))



;;; defun spacemacs/

(defun spacemacs/check-large-file ()
  "If the buffer-size is more than 500000, open the file by fundamental-mode"
  (when (> (buffer-size) 500000)
    (progn (fundamental-mode)
           (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      (fundamental-mode)))

(defun spacemacs/reload-python-mode ()
  "Reload python-mode to fix company-backends bugs"
  (defvar reload-python-times 0)
  (when (and (= reload-python-times 0) (string-equal major-mode 'python-mode))
    (python-mode)
    (setq reload-python-times 1)))



;;; funcs.el ends here
