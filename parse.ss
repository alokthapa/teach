#lang racket
(provide (all-defined-out))

(define *indent-length* 4)

(define frenchtoo
"
?how do you do?
@not bad! how about you?
?I'm find thank you!
@I don't really care!
@You're welcome.
?[greet]Bonjour!
@Bonjour! Is that French?
?Oui! Bonjour means 'good morning' in French
@Ahh! Je comprend!
?What do you think 'bon' in bonjour means?
@umm I don't know...
    ?It means 'good'
    @ahh...
@I guess it means good
    ?awesome
    @thanks
?Ok now what might 'jour' mean?
@night
    ?not quite
    @sorry...
@day
    ?awesome!
    @thanks
?too complicated?
@[greet]yes
@not really
")

(define mathtoo
"
?1 + 1= ?
+2
@3
?1 + 2 = ?
+3
@4
")

(define texxt 
"
?Do you feel like you're good for nothing???????
@Yes
    ?Are you kinda big?
    @Yes
        ?You're the big red one!
    @No
        ?You're the small red one!
@No, I feel quite important
?Do you feel like you're going to explode when you're angry?
@Yes
    ?Ahh... you're the black bomb one then...
@Nope
?Do you poop a lot when you're angry?
@Yes
    ?well then you're the poopy white one!
@No, I poop but not when I'm angry!
?Are you schizophrenic by any chance
@Yes how do you know?
    ?You're the small blue one!!
@No! Don't insult me!
?Are you better when you do stuff backwards?
@Yes
    ?Well you're the wierd one that comes back like a boomarang!
@Nope!
?Well then you're the yellow one!")

(define-struct qnode (text responses label) #:transparent)
(define-struct qresp (text node score goto) #:transparent) 

(define (find-label nodes lbl)
  (memf (lambda (node)
           (and (qnode-label node)
                (string=? (qnode-label node) lbl)))
         nodes))

;utility fn          
(define (is-helper text chr)
  (and (> (string-length text) 0)
       (char=? chr (string-ref text 0))))

(define (is-question? text)
  (or  (is-helper text #\?)))

(define (is-response? text)
  (or (is-helper text #\+) (is-helper text #\@)))

(define (is-score? text)
  (is-helper text #\+))

(define (is-labelled? text)
  (regexp-match #rx".\\[.*\\].*" text))

(define (string-find-index chars char (count 0))
  (cond
    ((null? chars) -1)
    ((char=? (car chars) char) count)
    (else (string-find-index (cdr chars) char (add1 count)))))

;common for both questions and answers 
;is not defined for text that don't have a label.
(define (get-label text)
  (substring text 2 (string-find-index (string->list text) #\])))

(define (build-text lst)
  (string-join (cons
                (remove-label-if-exists (car lst))
                (cdr lst))
               "\n"))

(define (remove-label-if-exists text)
  (if (is-labelled? text)
      (substring text
                 (+ 1 (string-find-index (string->list text) #\]))
                 (string-length text))
      (substring text 1 (string-length text))))
                
(define (get-question list-text (found? #f) (acc '()))
  (if (or (null? list-text)
          (and (is-question? (car list-text)) found?)
          (is-response? (car list-text)))
      (let ((rightorder (reverse acc)))
        (values (build-text rightorder)
                (if (is-labelled? (car rightorder)) (get-label (car rightorder)) #f)
                list-text))
      (get-question (cdr list-text) #t (cons (car list-text) acc))))

(define (build-qresp lst node)
  (let ((qres (build-text lst)))
    (make-qresp qres
                node
                (is-score? (car lst))
                (if (is-labelled? (car lst)) (get-label (car lst)) #f))))

(define (get-response list-text (found? #f) (acc '()) (node #f))
  (if (or (null? list-text)
          (is-question? (car list-text))
          (and (is-response? (car list-text)) found? ))
      (values (build-qresp (reverse acc) node)
              list-text)
      (let ((curr (car list-text)))
        (if (is-question-indent? curr)
            (let-values ([(azk rst)  (get-indented-content list-text)])
              (let-values ([(nd nm) (get-node azk)])
                (values (build-qresp (reverse acc) nd) rst)))
            (get-response (cdr list-text) #t (cons curr acc))))))

;;move the length of indent and check for ?
(define (is-question-indent? text)
  (and (> (string-length text) *indent-length*)
       (is-question? (substring text *indent-length* (string-length text)))))

(define (get-current-text text indent)
  (substring text (min (string-length text) (* indent *indent-length*))))

(define (still-indent? text indent)
  (andmap (lambda (c) (char=? c #\space))
       (string->list (substring text 0 (min (string-length text) (* indent *indent-length*))))))

(define (get-indented-content list-text (indent 1)  (acc '()))
  (if (and (pair? list-text) (still-indent? (car list-text) indent))
      (get-indented-content (cdr list-text) indent (cons (get-current-text (car list-text) indent) acc))
      (values (reverse acc) list-text)))

(define (get-all-responses list-text (acc '()))
  (if (and (pair? list-text)
           (is-response? (car list-text)))
      (let-values ([(rsp rst) (get-response list-text)])
        (get-all-responses rst (cons rsp acc)))
      (values (reverse acc) list-text)))

(define (get-node list-text)
  (let-values ([(ques lbl rst) (get-question list-text)]) 
    (let-values ([(rps rst2) (get-all-responses rst)])
      (values (make-qnode ques rps lbl) rst2)))) 

(define (parse-noder list-text)
  (if (null? list-text)
      '()
      (let ((curr (car list-text)))
        (cond
         ((string=? "" curr)  (parse-noder (cdr list-text)))
         ((is-question? curr)
          (let-values ([(nd rst) (get-node list-text)])
            (cons nd (parse-noder rst))))))))

(define (parse-text txt)
  (parse-noder (regexp-split #rx"\n" txt)))