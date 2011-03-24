#lang racket

(require "parse.ss" 
         rackunit)


;;is-question?
(check-false (is-question? "hello") "Check question false")
(check-not-false (is-question? "?hello") "Check question is not false")
(check-false (is-question? "@hello") "check response not answer @")
(check-false (is-question? "+hello") "check response not answer +")

;;is-response
(check-false (is-response? "hello") "Check response false")
(check-false (is-response? "?hello") "Check response not question")
(check-not-false (is-response? "@hello") "Check response for @")
(check-not-false (is-response? "+hello") "check response for +")

;;find-label
(define parsed-french (parse-text frenchtoo))

(check-pred qnode?  (find-label parsed-french "greet"))
(check-pred false? (find-label parsed-french "nofreakingway"))

;;is-labelled
(check-not-false (is-labelled? "?[greet]hello") "Check for question")
(check-not-false (is-labelled? "@[greet]bye") "Check for response")
(check-false (is-labelled? "?hello"))
(check-false (is-labelled? "@bye"))

;;string-find-index
(check-eq? (string-find-index (string->list "hello") #\h) 0 "returns zero-based index if found")
(check-eq? (string-find-index (string->list "hello") #\z) -1 "if not found then returns -1")

;;get-label
(check-equal? (get-label "?[greet]hello") "greet" "return the label of the question")
(check-equal? (get-label "@[bye]see you") "bye" "return the label of the response")
(check-exn exn:fail? (lambda () (get-label "?hello")) "is not defined for strings that don't have a label")

;build-text
(check-equal? (build-text '("?hello" "world")) "hello\nworld" "join strings together adding newline and removing the question or response marker")
(check-equal? (build-text '("?[greet]hello" "world")) "hello\nworld" "also removing labels")
(check-equal? (build-text '("q<-letter removed")) "<-letter removed" "although indiscriminate")

;;remove-label-if-exists
(check-equal? (remove-label-if-exists "?[greet]hello") "hello" "removes the label")
(check-equal? (remove-label-if-exists "?hello") "hello" "if it exists")
(check-equal? (remove-label-if-exists "q<-letter removed") "<-letter removed" "although does not check for the marker")

;;get-question
