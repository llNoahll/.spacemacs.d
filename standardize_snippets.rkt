#lang typed/racket


(: deal-file [-> Path Void])
(define deal-file
  (λ (file-path)

    (: modify-file [-> Path Void])
    (define modify-file
      (λ (file-path)

        (: new-content String)
        (define new-content
          (string-trim
           (call-with-input-file file-path
             (λ ([in : Input-Port]) : String
               (let loop : String ([line : (U EOF String) (read-line in)]
                                   [content : String ""])
                 (cond [(eof-object? line) content]
                       [(string-prefix? line "# key:")
                        (cond [(string-suffix? line ".")
                               (loop (read-line in) (string-append content line "\n"))]
                              [(string-suffix? line ":")
                               (loop (read-line in)
                                     (string-append content (string-replace line ":" ".") "\n"))]
                              [else
                               (loop (read-line in)
                                     (string-append content (string-append line ".\n")))])]
                       [else (loop (read-line in) (string-append content line "\n"))]))))))


        (call-with-output-file file-path #:exists 'replace
          (λ ([out : Output-Port]) : String
            (display new-content out)))))

    (: rename-file [-> Path Void])
    (define rename-file
      (λ (file-path)
        (let ([file-name : String (path->string file-path)])
          (cond [(string-suffix? file-name ".org") (void)]
                [(string-suffix? file-name ".markdown") (void)]
                [(string-suffix? file-name ".el") (void)]
                [(string-suffix? file-name ".elc") (void)]
                [(string-suffix? file-name ".js") (void)]
                [(string-suffix? file-name ".gitignore") (void)]
                [(string-suffix? file-name ".snippet") (void)]
                [(string-suffix? file-name ".yas-parents") (void)]
                [(string-suffix? file-name ".yas-make-groups") (void)]
                [(string-suffix? file-name ".yasnippet")
                 (rename-file-or-directory file-path
                                           (string-replace file-name ".yasnippet" ".snippet"))]
                [(string-suffix? file-name ".yasinppet")
                 (rename-file-or-directory file-path
                                           (string-replace file-name ".yasinppet" ".snippet"))]
                [(string-suffix? file-name ".yansnippet")
                 (rename-file-or-directory file-path
                                           (string-replace file-name ".yansnippet" ".snippet"))]
                [else (rename-file-or-directory file-path
                                                (string-append file-name ".snippet"))]))))

    (modify-file file-path)
    (rename-file file-path)))

(: deal-dir [-> Path Void])
(define deal-dir
  (λ (dir-path)
    (for ([path (directory-list dir-path #:build? #t)])
      (cond [(file-exists? path)
             (displayln (format "deal file: ~a" path))
             (deal-file path)]
            [(directory-exists? path)
             (displayln "\n----------------------------------------------------------------------")
             (displayln (format "deal files under: ~a" path))
             (deal-dir path)]
            [else
             (displayln (format "~a is not a directory or a file!" path))]))))


(deal-dir (string->path "snippets"))