#lang racket
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/http/cookie)
(require web-server/http/cookie-parse)
(require "structs.ss")

;(require "db.ss")
;(require "model.ss")

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
(define (login request)
  (response/xexpr
   `(p "you are logged in")))

(define (logout request)
  (response/xexpr 
   `(p "you are logged out")))
  
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

(define (make-user-cookie usr)
  (make-cookie "id" "alok" #:secure? #t))

(define (get-user-from-cookie req)
  (let* ((cooks (request-cookies req))
         (usrname (findf (lambda (c) (string=? "user" (client-cookie-name c))) cooks)))
    (find-user usrname)))

(define (login/out req)
  (let* ((cook (request-cookies req))
         (usr (findf (lambda (c) (string=? "user" (client-cookie-name c))) cook)))
    (if usr
        `(a ((href ,(teach-url logout))) "logout")
        `(a ((href ,(teach-url login))) "login"))))

(define (common-layout req  body)
  (response/xexpr
  `(html
    (head
     (title "t-t-t-teach")
     (link ((href "/public/teach.css") (rel "stylesheet") ( type "text/css"))))
    (body ((class "all"))
          (div ((class "main"))
               (div ((class "head"))
                    (a ((href "/teach/")) "t-t-t-teach  "))
	       ,(login/out req))
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
            (common-layout request 
             `(div ((class "node-main"))
                   (div ((class "node-ask"))
                        ,(node-ask (current-node)))
                   (div ((class "nodes-resps"))
                        ,@(map (lambda (r)
                                 (let ((em (embed/url (new-node-location r))))
                                   (node-resp  r (embed/url (new-node-location r)))))
                                 (node-responses (current-node)))))))
          (define (end-of-show needless)
            (common-layout request
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


(define (page404 request)
  (response/xexpr 
   `(html (body (p "sorry the page was not found!")))))

(define-values (teach-dispatch teach-url)
      (dispatch-rules
       [("hello") start]
       [("login") login]
       [("logout") logout]
       [else page404]))


(define run-teach-server
  (serve/servlet teach-dispatch
               #:quit? #t
               #:listen-ip #f
               #:servlet-path "/teach/"
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path (current-directory)  "public"))
               #:port 8000))
