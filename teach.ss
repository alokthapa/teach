#lang racket
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/http/cookie)
(require web-server/http/cookie-parse)
(require "node.ss")
(require "structs.ss")

(provide (all-defined-out))

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
      (title "Satori")
      (link ((href "http://dl.dropbox.com/u/121008/teach.css") (rel "stylesheet") ( type "text/css"))))
     (body ((class "all"))
	   (div ((class "main"))
		(div ((class "head"))
		     (a ((href "/hello")) "satori ")
		     ,(login/out req))
	   (center
	    ,body))))))
;; pages 

(define (show-nodz st n orig-nodes request)
  (local [(define (azk-curr) (car n))
	  (define (node-rezp resp url)
	    `(div ((class "node-resp")
		   (onclick ,(string-append "javascript:window.location='" url "'")))
		  (a ((href ,url))
		     ,(rezp-text resp))))
	  (define (response-generator embed/url)
	    (common-layout 
	     request
	     `(div ((class "node-main"))
		   (div ((class "node-ask"))
			,(azk-q (azk-curr)))
		   ,(if (azk-rezps? (azk-curr))
			 `(div ((class "nodes-resps"))
			      ,@(map (lambda (r)
				       (let ((em (embed/url (new-node-location r))))
					 (node-rezp r em)))
				     (azk-rezps (azk-curr))))
			 `(p)))))
	  (define (score1 st)
	    (if (eq? (car st) 'no-score-value)
		(list 1 (cdr st))
		(list (+ 1 (car st))
		      (cdr st))))
          (define (new-node-location resp)
            (let ((action (rezp-action resp))
		  (newst (if (scores? resp) (score1 st) st)))
              (lambda (req)
                (cond
                 ((null? action) (show-nodz newst (cdr n) orig-nodes req))
                 ((atom? action) 
		  (cond 
		   ((eq? action 'home) (show-nodz newst orig-nodes orig-nodes req))
		   (else ((show-nodz newst (find-azk action orig-nodes) orig-nodes req)))))
                 ((pair? action) (show-nodz newst (cons action (cdr n)) orig-nodes req))))))

          (define (end-of-show needless)
            (common-layout 
	     request
             `(div ((class "node-main"))
                   (div ((class "node-ask"))
                        (p "end of class")
			(p ,(if (eq? (car st) 'no-score-value) 
				""
				(string-append "your score is " (number->string (car st)))))))))]
	   (if (pair? n)
	       (send/suspend/dispatch response-generator)
	       (send/suspend/dispatch end-of-show))))

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

;;st -> state is a list of score and objs collected. 


(define (welcome request)
  (local [(define (response-generator make-url)
	    (common-layout 
	     request 
	     `(div ((class "welcome-div"))
	       (div (a ((href ,(make-url (lambda (req) (nodestart math-nodes req)))))
		       "math problems"))
	       (div (a ((href ,(make-url (lambda (req) (nodestart angry-birds req)))))
		       "angry birds problems"))

	       (div (a ((href ,(make-url (lambda (req) (nodestart new-nodes req)))))
		       "learning french example")))))]
	 (send/suspend/dispatch response-generator)))

(define (nodestart n request)
  (show-nodz (list 'no-score-value (list)) n n request))

(define (logout request)
  (response/xexpr 
   #:headers (map cookie->header (list (make-logout-cookie)))
   `(p "you are logged out")))

(define (page404 request)
  (response/xexpr 
   `(html (body (p "sorry the page was not found!")))))

(define-values (teach-dispatch teach-url)
      (dispatch-rules
       [("hello") welcome]
       [("login") login]
       [("logout") logout]
       [("angry") (lambda (req) (nodestart angry-birds req))]
       [else page404]))

;;TODO extra-files-path is not working, so no css is being loaded at the moment, however we could always run them in xginx or s;;3 buckets so no big deal, though would be nice if we get it to work.
(define (run-teach-server)
  (serve/servlet teach-dispatch
               #:quit? #t
               #:listen-ip #f
               #:servlet-path "/teach/"
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path (current-directory)  "public"))
               #:port 8000))
