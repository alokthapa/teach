#lang racket
(require "db.ss")
(require (planet untyped/snooze/quick-find))

(provide (all-defined-out))

(define-persistent-struct user
  ([name type:string]
   [pwd type:string]))

(define-persistent-struct teachpack
  ([name type:string]))

(define-persistent-struct user-teachpack
  ([user-id type:integer]
   [teachpack-id type:integer]
   [current type:integer]))

(define-persistent-struct lesson
  ([name type:string]
   [order type:integer]
   [data type:string]))

  
;;create all tables

(define (initialize-db)
  (call-with-connection
   (lambda ()
     (create-table user)
     (create-table teachpack)
     (create-table user-teachpack)
     (create-table lesson))))

;;destroy all tables
(define (destroy-db)
  (call-with-connection
   (lambda ()
     (drop-table user)
     (drop-table teachpack)
     (drop-table user-teachpack)
     (drop-table lesson))))

(define (add-dummy-users)
  (call-with-connection
   (lambda ()
   (save! (make-user "alok" "123"))
   (save! (make-user "al" "123")))))


(define (find-user name)
  (find-one (sql (select #:from user #:where (= user.name ,name)))))

