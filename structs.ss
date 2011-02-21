#lang racket

(provide (all-defined-out))

(define *users* (list))

(define (all-users) *users*)

(define-struct user (id name pwd) #:mutable #:prefab)

(define-struct teachpack (id name loc) #:mutable #:prefab)

(define (add-user usr)
  (set! *users* (cons usr *users*)))

(define (find-user name)
  (findf (lambda (usr) 
	    (string=? (user-name usr) name)) 
	(all-users)))

;;test data
(add-user (make-user 1 "alokt" "123"))