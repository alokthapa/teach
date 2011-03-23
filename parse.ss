#lang racket

(define *indent-length* 1)
(define texxt 
"
?Do you feel like you're good for nothing?
@Yes I am good for nothing.
 ?Are you kinda big?
 @Yes I am big.
  ?You're the big red one!
 @No I am small.
  ?You're the small red one!
@No, I feel quite important
?Do you feel like you're going to explode when you're angry?
@Yes
 ?Ahh... you're the black bomb one then...
@Nope
")

(define (get-question list-text (found? #f) (acc '()))
  (if (or (null? list-text)
          (and (is-question? (car list-text)) found?)
          (is-response? (car list-text)))
      (values (reverse acc) list-text)
      (get-question (cdr list-text) #t (cons (car list-text) acc))))

(define (get-response list-text (found? #f) (acc '()))
  (if (null? list-text)
      (values (reverse acc) list-text)
      (let ((curr (car list-text)))
        (cond
         ((is-response? curr)
          (if found?
              (values acc list-text)
              (get-response (cdr list-text) #t (cons curr  acc))))
         ((is-question? curr)
          (values acc list-text))
         ((is-question-indent? curr)
          (let-values ([(azk rst)  (get-indented-content list-text)])
            (get-response rst #t (cons (list 'azk (parse-noder azk)) acc))))
         (else (get-response (cdr list-text) #t (cons curr acc)))))))

(define (is-helper text chr)
  (and (> (string-length text) 0)
       (char=? chr (string-ref text 0))))

;;move the length of indent and check for ?
(define (is-question-indent? text)
  (and (> (string-length text) *indent-length*)
       (is-question? (substring text *indent-length* (string-length text)))))

(define (is-question? text)
  (is-helper text #\?))

(define (is-response? text)
  (is-helper text #\@))

(define (get-current-text text indent)
  (substring text (min (string-length text) (* indent *indent-length*))))

(define (still-indent? text indent)
  (andmap (lambda (c) (char=? c #\space))
       (string->list (substring text 0 (min (string-length text) (* indent *indent-length*))))))

(define (get-indented-content list-text (indent 1)  (acc '()))
  (if (and (pair? list-text) (still-indent? (car list-text) indent))
      (get-indented-content (cdr list-text) indent (cons (get-current-text (car list-text) indent) acc))
      (values (reverse acc) list-text)))

(define (parse-noder list-text)
  (if (null? list-text)
      '()
      (let ((curr (car list-text)))
        (cond
         ((string=? "" curr)  (parse-noder (cdr list-text)))
         ((is-question? curr)
          (let-values ([(ques rst) (get-question list-text)])
            (cons ques (parse-noder rst))))
         ((is-response? curr)
          (let-values ([(resp rst) (get-response list-text)])
            (cons resp (parse-noder rst ))))))))

(define lst (regexp-split "\n" texxt))
(define result (parse-noder lst))