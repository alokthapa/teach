#lang racket
(require "db.ss")
(require (planet untyped/snooze/quick-find))

(provide (all-defined-out))

(define-persistent-struct user
  ([name type:string]
   [pwd type:string]))

(define-persistent-struct quickquiz
  ([user-id type:integer]
   [name type:string]
   [data type:string]
   [style type:string]))
  

(define-persistent-struct teachpack
  ([user-id type:integer]
   [name type:string]))

(define-persistent-struct section
  ((teachpack-id type:string)
   [name type:string]
   [order type:integer]
   [data type:string]
   [style type:string]))

  
;;create all tables

(define (initialize-db)
  (call-with-connection
   (lambda ()
     (create-table user)
     (create-table teachpack)
     (create-table quickquiz)
     (create-table section))))

;;destroy all tables
(define (destroy-db)
  (call-with-connection
   (lambda ()
     (drop-table user)
     (drop-table teachpack)
     (drop-table quickquiz)
     (drop-table section))))

(define (add-dummy-users)
  (call-with-connection
   (lambda ()
   (save! (make-user "alok" "123"))
   (save! (make-user "al" "123")))))

(define (create-user! name pwd)
  (call-with-connection
   (lambda ()
     (save! (make-user name pwd))))
  (find-user name))

(define (find-user name)
  (find-one (sql (select #:from user #:where (= user.name ,name)))))

(define (get-teachpacks-for-user userid)
  (find-all (sql (select #:from teachpack #:where (= teachpack.user-id ,userid)))))

(define (get-quickquiz-for-user userid)
  (find-all (sql (select #:from quickquiz #:where (= quickquiz.user-id ,userid)))))

(define (get-quickquiz-from-id id)
  (find-one (sql (select #:from quickquiz #:where (= quickquiz.id ,id)))))
(define (get-all-quickquiz)
  (find-all (sql (select #:from quickquiz))))

(define (create-teachpack! userid tpname)
  (save! (make-teachpack userid tpname)))

(define (create-quickquiz! userid qqname data style)
  (save! (make-quickquiz userid qqname data style)))


(define (get-sections-for-teachpack tpid)
  (find-all (sql (select #:from section #:where (= section.teachpack-id ,tpid)))))