#lang typed/racket


(: deal-file [-> Path Void])
(define deal-file
  (λ (file-path)
    (: current-year Natural)
    (define current-year (date-year (seconds->date (current-seconds))))

    (: modify-file [-> Path Void])
    (define modify-file
      (λ (file-path)
        (: new-content String)
        (define new-content
          (call-with-input-file file-path
            (λ ([in : Input-Port]) : String
                (let loop : String ([line : (U EOF String) (read-line in)]
                                    [content : String ""])
                     (cond [(eof-object? line) content]
                           [(string-prefix? line ";; Copyright (c) 2012-")
                            (loop (read-line in)
                                  (string-append content
                                                 (string-replace line
                                                                 (substring line 22 26)
                                                                 (number->string current-year))
                                                 "\n"))]
                           [else (loop (read-line in) (string-append content line "\n"))])))))


        (call-with-output-file file-path #:exists 'replace
          (λ ([out : Output-Port]) : Void
              (display new-content out)))))

    (modify-file file-path)))

(: deal-dir [-> Path Void])
(define deal-dir
  (λ (dir-path)
    (for ([path (directory-list dir-path #:build? #t)])
      (cond [(and (file-exists? path)
                  (string-suffix? (path->string path) ".el"))
             (displayln (format "deal file: ~a" path))
             (deal-file path)]
            [(directory-exists? path)
             (displayln "\n----------------------------------------------------------------------")
             (displayln (format "deal files under: ~a" path))
             (deal-dir path)]
            [else
             (displayln "\n**********************************************************************")
             (displayln (format "~a is not a directory or a file!" path))
             (displayln "\n**********************************************************************")]))))


(deal-dir (string->path "layers"))
