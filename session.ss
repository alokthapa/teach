#lang racket
(require (planet bzlib/base))
(require (planet bzlib/session))
(require (planet bzlib/session/setup/jsqlite))
(require (planet bzlib/session/web-server))

(require (planet bzlib/dbi))
(require (planet bzlib/dbd-jsqlite))

(provide (all-defined-out))

(define *h* '())
(define *path-db* (build-path (current-directory) "db" "teach.sqlite3"))

(define (setup-session)
    (setup-session-store/jsqlite! *path-db*))

(define (initialize-session)
    (set! *h* (connect 'jsqlite *path-db* '#:load (session-query-path/sqlite))))

(initialize-session)

