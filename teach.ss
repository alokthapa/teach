#lang racket
(require web-server/servlet)
(require web-server/servlet-env)


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
; car of node -> ask 
; cdr of node -> list of possible responses
; last of node (optional) -> label for node
; car of response -> responsetext
; cdr of response (optional) if atom -> go to label
; cdr of response (optional) if list -> a node
; 
; 
; if no cdr of a response exists, then it moves to the next askonce
; 

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

(define (start request)
  (show-nodes nodes request))
  
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


(define (common-layout body)
  `(html
    (head
     (title "t-t-t-teach")
     (link ((href "/teach.css") (rel "stylesheet") ( type "text/css")))
     (script ((type "text/javascript") (src "/prototype.js")))
     (script ((type "text/javascript") (src "/scriptaculous.js")))
     (script ((type "text/javascript") (src "/effects.js"))))
    (body ((class "all"))
          (div ((class "main"))
               (div ((class "head")) (a ((href "/teach/")) "t-t-t-teach"))
               (center
                ,body)))))


(define (node-resp resp url)
  `(div ((class "node-resp")
         (onclick ,(string-append "javascript:window.location='" url "'")))
        (a ((href ,url))
           ,(response-text resp))))


(define (show-nodes n request)
  (local [(define (current-node) (car n))
          (define (response-generator embed/url)
            (common-layout
             `(div ((class "node-main"))
                   (div ((class "node-ask"))
                        ,(node-ask (current-node)))
                   (div ((class "nodes-resps"))
                        ,@(map (lambda (r)
                                 (let ((em (embed/url (new-node-location r))))
                                   (node-resp  r (embed/url (new-node-location r)))))
                                 (node-responses (current-node)))))))
          (define (end-of-show needless)
            (common-layout
             `(div ((class "node-main"))
                   (div ((class "node-ask"))
                        (p "end of class")))))
          (define (new-node-location resp)
            (let ((action (response-action resp)))
              (lambda (req)
                (cond
                 ((null? action) (display 'is-null) (show-nodes (cdr n) req))
                 ((atom? action)  (display 'is-atom) (show-nodes (find-node action) req))
                 ((pair? action)  (display 'is-pair) (show-nodes (cons action (cdr n)) req))))))]
         (cond
          ((pair? n) (send/suspend/dispatch response-generator))
          (else (send/suspend/dispatch end-of-show)))))

(serve/servlet start #:quit? #t
               #:listen-ip #f
               #:servlet-path "/teach/"
               #:extra-files-paths
               (list (build-path (current-directory)  "public"))
               #:port 8000)
