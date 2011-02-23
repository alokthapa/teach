#lang racket

(require "structs.ss")
(provide (all-defined-out))

; a teaching web thingy where you can create your own custom teaching libraries.
; 
; 
; let's call an ask-response an askonce
; askonse contains ask and response
; 
; ask can contain text, pics or combinations of either.
; 
; response can contain text, and choices.
; 
; 
; each askonse can have a label so that you can get back to it later.
; 
; each response leads to another askonse.
; 
; How would it be structured?
; 
; 1. you should be able to ask questions and save them to variables. 
; 
; for eg. you could ask a persons name and reuse it to address the person later on.
; 
; askonce -> list of nodes
; ask/responsetext -> some html markup thingy 
; car of node -> ask reqd
; cdr of node -> list of possible responses reqd 
; third of node (optional) -> assoc list containing attr (label and objects) opt
; car of response -> responsetext reqd 
; cdr of response is an assoc list that may have 
;; azk -> a new azk
;; goto -> a label to jump to
;; score -> a score associated with the response
;; reqobjs -> not have these objs might be harmful

;; if no cdr of a response exists, then it moves to the next askonce

(define new-nodes 
  '(("how do you do?" 
     (("not bad! how about you?")) 
     ((label . home)))
    ("I'm fine thank you!!" 
     (("I don't really care!!" 
       ((goto . home)))
      ("you're welcome")))
    ("Bonjour!" 
     (("Bonjour! Is that French?")) 
     ((label . bonjour)))
    ("Oui! Bonjour means 'good morning' in French"
     (("Ahh! Je comprend!")))
    ("What do you think 'bon' in bonjour means?"
     (("umm I don't know..." 
       ((azk . ("it means 'good'" 
		(("ahh.."))))))
      ("I guess it means good"
       ((azk . ("awesome" 
		(("thanks")))))))
      ((label . complicatednode)))
    ("Ok now what might 'jour' mean?"
     (("night" 
       ((azk . ("not quite" 
		(("sorry.."))))))
      ("day! it means day!" 
       ((azk . ("awesome" 
	       (("thanks!"))))))))
    ("too complicated" 
     (("yes!" 
       ((goto . bonjour)))
      ("not really")))))

(define nodes 
  '(("how do you do?" (("not bad! how about you?")) home)
    ("I'm fine thank you!!" 
     (("I don't really care!!" home)
      ("you're welcome")))
    ("Bonjour" (("Bonjour! Is that French?")) bonjour)
    ("Oui! Bonjour means 'good morning' in French"
     (("Ahh! Je comprend!")))
    ("What do you think 'bon' in bonjour means?"
     (("umm I don't know..." ("it means 'good'" (("ahh.."))))
      ("I guess it means good" ("awesome" (("thanks"))))) complicatednode)
    ("Ok now what might 'jour' mean?"
     (("night" ("not quite" (("sorry.."))))
      ("day! it means day!" ("awesome" (("thanks!"))))))
    ("too complicated" 
     (("yes!" bonjour)
      ("not really")))))

(define (azk-q n) (car n))
(define (azk-rezps n) (cadr n))

(define (azk-assoc n)
  (if (>  (length n) 2)
      (caddr n)
      #f))
(define (azk-assoc-find x n)
  (when (azk-assoc n)
	(let ((lbl (assoc x (azk-assoc n))))
	  (if lbl
	      (cdr lbl)
	      #f))))

(define (azk-lbl n)
  (azk-assoc-find 'label n))

(define (azk-obj n)
  (azk-assoc-find 'obj n))

;;only top level azks can be found for now
(define (find-azk lbl n)
  (memf (lambda (az)
	   (eq? (azk-lbl az) lbl)) n))
		
(define (rezp-text r)
  (car r))

(define (rezp-assoc r)
  (if (> (length r) 1)
      (cadr r)
      #f))

(define (rezp-assoc-find x r)
  (if (rezp-assoc r)
      (let ((val (findf (lambda (ass) 
			  (eq? (car ass) x))
			(rezp-assoc r))))
	(if val 
	    (cdr val)
	    #f))
      #f))

(define (rezp-lbl r)
  (rezp-assoc-find 'goto r))

(define (rezp-azk r)
  (rezp-assoc-find 'azk r))

;;only one of these can be true
(define (rezp-action r)
  (or (rezp-lbl r) (rezp-azk r) '()))

(define (rezp-needs r)
  (rezp-assoc-find 'needs r))

(define atom? 
   (lambda (x) 
     (cond 
       ((null? x) #f)
       ((pair? x)   #f)
       (else #t))))

(define (response-text r) (car r))

(define (find-node lbl)
    (local [(define (find-lbl n)
              (if (null? n) '()
                  (let ((lblname (last (car n))))
                    (cond 
                      ((and 
                        (atom? lblname) 
                        (eq? lblname lbl)) n)
                      (else (find-lbl (cdr n)))))))]
      (find-lbl nodes)))

(define (node-ask n) (first n))
 
(define (response-action r)
  (if (pair? (cdr r))
      (cadr r)
      '()))
 
(define (node-responses n)
  (second n))

(define (node-label n)
  (let ((lbl (last n)))
    (if (atom? lbl)
        lbl
        'empty-label)))

(define (has-label? n)
  (eq? 'emtpy-label (node-label n)))
