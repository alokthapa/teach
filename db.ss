#lang scheme/base

(require (planet untyped/snooze:2)
         (planet untyped/snooze:2/sqlite3/sqlite3))

(define-snooze-interface
  (make-snooze (make-database (build-path (current-directory) "db" "teach.sqlite3"))))

(provide (all-from-out (planet untyped/snooze:2))
         (snooze-interface-out))

