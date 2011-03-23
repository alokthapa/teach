#lang racket
(require "structs.ss")
(require racket/system)
(provide (all-defined-out))

;;utility fns for markdown

(require racket/system)

(provide (all-defined-out))

(define (get-markdown-port text)
  (car (process (string-append "echo \"" text "\" | perl /Users/alokthapa/hacking/scheme/teach/Markdown.pl -html4tags"))))

(define (markdown text)
  (call/cc (lambda (ezit)
             (let ((inputport (get-markdown-port text))
                   (out-value ""))
               (letrec ((A (lambda ()
                             (let ((val (read-line inputport)))
                               (if (eof-object? val)
                                   (ezit out-value)
                                   (begin
                                    
                                     (set! out-value (string-append out-value val))
                                     (A)))))))
                 (A))))))





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

(define-struct qnode (text responses (label #:auto)))
(define-struct qresp (text (node #:auto) (score #:auto) (goto #:auto))) 


(define new-nodes 
  '(("how do you do?" 
     (("not bad! how about you?")))
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

(define math-nodes
  '(("1+1 = ?"
     (("2" ((score . #t)))
      ("3")))
    ("1 + 2 = ?"
     (("3" ((score . #t)))
      ("4")))))


(define qnode (text responses label))
(define qresp (text node score goto)) 
(define angry-birds
  '(("Do you feel like you're good for nothing?"
     (("Yes"
       ((azk . ("Are you kinda big?"
		(("Yes" ((azk . ("You're the big red one!"))))
		 ("No" ((azk . ("You're the small red one!")))))))))
      ("No, I feel quite important!")))
    ("Do you feel like you're going to explode when you're angry?"
     (("Yes"
       ((azk . ("Ahh... you're the black bomb one then..."))))
      ("Nope")))
    ("Do you poop a lot when you're angry?"
     (("Yes"
       ((azk . (" well then you're the poopy white one!"))))
      ("No, I poop but not when I'm angry!")))
    ("Are you schizophrenic by any chance"
     (("Yes how do you know?"
       ((azk . ("You're the small blue one!!"))))
      ("NO! Don't insult me!")))
    ("Are you better when you do stuff backwards??"
     (("Yes"
       ((azk . ("Well you're the wierd one that comes back like a boomarang!"))))
      ("Nope!")))
    ("Well then you're the yellow one!")))

(define atom? 
   (lambda (x) 
     (not (or (null? x) (pair? x)))))

(define (azk-q n)  (markdown (car n)))
(define (azk-rezps? n) (> (length n) 1)) 
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
  (markdown (car r)))

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

(define (scores? r)
  (rezp-assoc-find 'score r))

;;only one of these can be true
(define (rezp-action r)
  (or (rezp-lbl r) (rezp-azk r) '()))

(define (rezp-needs r)
  (rezp-assoc-find 'needs r))


