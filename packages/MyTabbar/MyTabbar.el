;;; my-tabbar.el ---
;;
;; Author: Hua Liang[Stupid ET] <et@everet.org>
;; Time-stamp: <2014-09-03 13:15:39 Wednesday by Hua Liang>
;; ==================== tabbar ====================
;;tabbar
(tabbar-mode 1)
(global-set-key [(meta j)] 'tabbar-backward)
(global-set-key [(meta k)] 'tabbar-forward)
(global-set-key [(control meta j)] 'tabbar-backward-group)
(global-set-key [(control meta k)] 'tabbar-forward-group)
;;set group strategy
(defun tabbar-buffer-groups ()
  "tabbar group"
  (list
   (cond
    ((memq major-mode '(shell-mode sh-mode))
     "shell"
     )
    ((memq major-mode '(c-mode c++-mode))
     "cc"
     )
    ((memq major-mode '(dired-mode ibuffer-mode))
     "files"
     )
    ((eq major-mode 'python-mode)
     "python"
     )
    ((eq major-mode 'ruby-mode)
     "ruby"
     )
    ((memq major-mode
	   '(php-mode nxml-mode nxhtml-mode))
     "WebDev"
     )
    ((eq major-mode 'emacs-lisp-mode)
     "Emacs-lisp"
     )
    ((memq major-mode
	   '(tex-mode latex-mode text-mode snippet-mode org-mode moinmoin-mode markdown-mode))
     "Text"
     )
    ((string-equal "*" (substring (buffer-name) 0 1))
     "emacs"
     )
    (t
     "other"
     )
    )))
(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)



;; cancel grouping
;;(setq tabbar-buffer-groups-function
;;    (lambda (b) (list “All Buffers”)))
;;(setq tabbar-buffer-list-function
;;    (lambda ()
;;        (remove-if
;;          (lambda(buffer)
;;             (find (aref (buffer-name buffer) 0) ” *”))
;;          (buffer-list))))
;; -------------------- tabbar --------------------
