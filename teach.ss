#lang racket
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/http/cookie)
(require web-server/http/cookie-parse)
(require "structs.ss")

(provide (all-defined-out))

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
  (make-cookie "id" (user-name usr)))

(define (make-logout-cookie)
  (make-cookie "id" "noone" ))

(define (get-user-from-cookie req)
  (let ((cooks (request-cookies req)))
    (if (pair? cooks)
	(find-user (client-cookie-value (findf (lambda (c)
			    (and (client-cookie-name c)
				 (string=? "id" (client-cookie-name c))))
			  cooks)))
	#f)))

(define (login/out req)
  (let ((usr (get-user-from-cookie req)))
    (if usr
        `(a ((href ,(teach-url logout))) ,(string-append (user-name usr)  "--logout"))
        `(a ((href ,(teach-url login))) "login"))))

(define (common-layout req body)
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
;; pages 
(define (show-nodes n request)
  (local [(define (current-node) (car n))
	  (define (node-resp resp url)
	    `(div ((class "node-resp")
		   (onclick ,(string-append "javascript:window.location='" url "'")))
		  (a ((href ,url))
		     ,(response-text resp))))
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

(define (login request)
  (local [(define (parse-username bindings)
	    (extract-binding/single 'username bindings))
	  (define (parse-password bindings)
	    (extract-binding/single 'password bindings))
	  
	  (define (login-handler request)
	    (let* ((binds (request-bindings request))
		   (usr (find-user (parse-username binds))))
	      (if (and usr 
		       (string=? (parse-password binds)
				 (user-pwd usr)))
		  (send/suspend (lambda (k-url)
				  (response/xexpr
				   #:headers (map cookie->header (list (make-user-cookie usr)))
				   `(html (body (p "welcome!"))))))
		  (send/suspend/dispatch login))))
	  (define (response-generator make-url)
	    (response/xexpr
	     `(html
	       (head 
		(title "login"))
	       (body 
		(h1 "Login")
		(form ((action ,(make-url login-handler)))
		      (input ((name "username")))
		      (input ((name "password")))
		      (input ((type "submit"))))))))]
	 (send/suspend/dispatch response-generator)))
(define (start request)
  (show-nodes nodes request))

(define (logout request)
  (response/xexpr 
   #:headers (map cookie->header (list (make-logout-cookie)))
   `(p "you are logged out")))

(define (page404 request)
  (response/xexpr 
   `(html (body (p "sorry the page was not found!")))))

(define-values (teach-dispatch teach-url)
      (dispatch-rules
       [("hello") start]
       [("login") login]
       [("logout") logout]
       [else page404]))

(define (run-teach-server)
  (serve/servlet teach-dispatch
               #:quit? #t
               #:listen-ip #f
               #:servlet-path "/teach/"
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path (current-directory)  "public"))
               #:port 8000))
