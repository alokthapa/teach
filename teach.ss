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
      (title "t-t-t-teach")
      (link ((href "/teach.css") (rel "stylesheet") ( type "text/css"))))
     (body ((class "all"))
	   (div ((class "main"))
		(div ((class "head"))
		     (a ((href "/hello")) "t-t-t-teach  "))
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
                 ((null? action) (show-nodes (cdr n) req))
                 ((atom? action) (show-nodes (find-node action) req))
                 ((pair? action) (show-nodes (cons action (cdr n)) req))))))]
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
