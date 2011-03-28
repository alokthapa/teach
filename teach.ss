#lang racket
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/http/cookie)
(require web-server/http/cookie-parse)
(require xml)
(require "parse.ss")
(require "model.ss")



(provide (all-defined-out))


;--markdown related

(define (get-markdown-path)
  "~/markdown/Markdown.pl")
(define (get-markdown-port text)
  (car (process (string-append "echo \"" text "\" | perl " (get-markdown-path)))))

(define (markdown text) (string-append "<div>" (markdown1 text) "</div>"))
(define (markdown1 text)
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



;--


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
      (link ((href "/style/teach.css") (rel "stylesheet") ( type "text/css")))
      (script ((src "/js/jquery-1.5.min.js")) "")
      (script ((src "/js/jquery.hotkeys.js")) "")
      (script ((src "/js/underscore.js")) "")
      (script ((src "/js/proper.js")) ""))
     (body ((class "all"))
	   (div ((class "main"))
		(div ((class "head"))
		     (a ((href "/hello")) "satori ")
                     ,(login/out req))
                ,body)))))
;; pages


(define (convert-to-xexpr text)
  (string->xexpr (markdown text)))

(define (show-nodes st n orig-nodes request)
  (local [(define (azk-curr) (car n))
          (define (node-resp resp url)
            `(div ((class "node-resp")
                   (onclick ,(string-append "javascript:window.location='" url "'")))
                  (a ((href ,url))
                     ,(convert-to-xexpr (qresp-text resp)))))
          (define (response-generator embed/url)
            (common-layout 
             request
             `(div ((class "node-main"))
                   (div ((class "node-ask"))
                        ,(convert-to-xexpr (qnode-text (azk-curr))))
                   ,(if (pair? (qnode-responses (azk-curr)))
                        `(div ((class "nodes-resps"))
                              ,@(map (lambda (r)
                                       (let ((em (embed/url (new-node-location r))))
                                         (node-resp r em)))
                                     (qnode-responses (azk-curr))))
                        `(p)))))
          (define (score1 st)
            (if (eq? st 'no-score-value)
                1
                (add1 st)))
          (define (new-node-location resp)
            (let ((newst (if (qresp-score resp) 
                             (score1 st) st)))
              (lambda (req)
                (cond
                  ((qresp-goto resp)
                   (show-nodes newst (find-label orig-nodes (qresp-goto resp)) orig-nodes req))
                  ((qresp-node resp)
                   (show-nodes newst (cons (qresp-node resp) (cdr n)) orig-nodes req))
                  (else  (show-nodes newst (cdr n) orig-nodes req))))))
          (define (end-of-show needless)
            (common-layout 
             request
             `(div ((class "node-main"))
                   (div ((class "node-ask"))
                        (p "end of class")
                        (p ,(if (eq? st 'no-score-value) 
                                ""
                                (string-append "your score is " (number->string st))))))))]
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

(define (list-teachpacks tps)
  (cons 'div (map (lambda (tp)
         `(div ((class "teachpack"))
               ,(teachpack-name tp)))
       tps)))

(define (list-sections sections)
  (cons 'div (map (lambda (sec)
         `(div ((class "section"))
               ,(section-name sec)))
       sections)))

 (define (range a b)
	(if (> a b)	
	'()	
	(cons a (range (add1 a) b))))

(define (with-input fn-after page-after)
  (lambda (request)
    (local [(define (response-generator make-url)
              (common-layout
               request
               `(form ((action ,(make-url handler)))
                      ,@(map (lambda (i)
                               `(input ((name ,(string-append "input" (number->string i))))))
                             (range 1 (procedure-arity fn-after)))
                      (input ((type "submit"))))))
            (define (handler req)
              (let ((binds (request-bindings req)))
                (apply fn-after (map (lambda (i)
                                 (extract-binding/single (string->symbol (string-append "input" (number->string i))) binds))
                               (range 1 (procedure-arity fn-after))))
                (serve/dispatch page-after)))]
            (send/suspend/dispatch response-generator))))
             

(define (single-input-get request fn-after page-after)
  (local [(define (parse-input bindings)
            (extract-binding/single 'textinput bindings))
          (define (handler req)
            (let ((binds (request-bindings req))
                  (usr (get-user-from-cookie req)))
              (fn-after (user-id usr) (parse-input binds))
              (serve/dispatch page-after)))
          (define (response-generator make-url)
            (common-layout
             request
             `(form ((action ,(make-url handler)))
                    (input ((name "textinput")))
                    (input ((type "submit"))))))]
    (send/suspend/dispatch response-generator)))
               
(define (create-quickquiz request)
   (local [(define (parse-qqname bindings)
            (extract-binding/single 'qqname bindings))
           (define (parse-qqdata bindings)
            (extract-binding/single 'qqdata bindings))
          (define (handler req)
            (let ((binds (request-bindings req))
                  (usr (get-user-from-cookie req)))
              (create-quickquiz! (user-id usr) (parse-qqname binds) (parse-qqdata binds) "score" )
              (serve/dispatch dashboard)))
          (define (response-generator make-url)
            (common-layout
             request
             `(form ((method "post") (action ,(make-url handler)))
                    (label () "Name") (input ((name "qqname"))) (br)
                    (label () "Quiz") (textarea ((name "qqdata") (rows "10") (cols "80")) "")(br)
                    (input ((type "submit"))))))]
    (send/suspend/dispatch response-generator)))
  
  
(define (create-teachpack request)
  (local [(define (parse-teachpackname bindings)
            (extract-binding/single 'teachname bindings))
          (define (teachpack-handler req)
            (let ((binds (request-bindings req))
                  (usr (get-user-from-cookie req)))
              (create-teachpack!(user-id usr) (parse-teachpackname binds))
              (serve/dispatch dashboard)))
          (define (response-generator make-url)
            (common-layout
             request
             `(form ((action ,(make-url teachpack-handler)))
                    (input ((name "teachname")))
                    (input ((type "submit"))))))]
    (send/suspend/dispatch response-generator)))

(define (quickquiz-page qq request)
  (local [(define (response-generator make-url)
            (common-layout
             request
             `(div 
               (h1 ,(quickquiz-name qq))
               (div ((class "quiz"))
                    ,(newline->br (quickquiz-data qq)))
               (a ((href ,(make-url  (lambda (req) (nodestart (parse-text (quickquiz-data qq)) req)))))
                  "Run"))))]
    (send/suspend/dispatch response-generator)))

(define (newline->br str)
  (regexp-replace* #rx"\n" str "<br/>"))

(define (teachpack-page tp request)
  (local [(define (response-generator make-url)
            (common-layout
             request
             `(div 
               (h1 ,(teachpack-name tp))
               (div ((class "sections"))
                   ,(list-sections (get-sections-for-teachpack (teachpack-id tp)))))))]
    (send/suspend/dispatch response-generator))) 

(define (create-and-list header make-url objs fn-create fn-obj fn-name)
  `(div ((style "padding:5px;margin:5px;background-color:white"))
    (h1 ,header)
    (a ((href ,(make-url fn-create))) "Create New")
    (div
     ,@(map (lambda (obj)
              `(div ((class "list"))
                    (a ((href ,(make-url (lambda (r) (fn-obj obj r)))))
                       ,(fn-name obj))))
            objs))))
    
   
(define (dashboard request)
  (let ((usr (get-user-from-cookie request)))
    (local [(define (build-teachpack make-url)
              (create-and-list "QuickQuiz"
                               make-url
                               (get-quickquiz-for-user (user-id usr))
                               create-quickquiz
                               quickquiz-page
                               quickquiz-name))
              (define (build-quickquiz make-url)
                (create-and-list "TeachPacks" 
                                 make-url 
                                 (get-teachpacks-for-user (user-id usr))
                                 create-teachpack
                                 teachpack-page
                                 teachpack-name))    
            (define (response-generator make-url)
              (common-layout
               request
               `(div ((class "dashboard-div"))
                    ,(build-quickquiz make-url)
                    ,(build-teachpack make-url))))]
      (send/suspend/dispatch response-generator ))))
                                   
(define (welcome request)
  (local [(define (response-generator make-url)
            (common-layout 
             request
             `(div ((class "welcome-div"))
                   (div (a ((href ,(make-url (lambda (req) (nodestart (parse-text mathtoo) req)))))
                           "math problems"))
                   (div (a ((href ,(make-url (lambda (req) (nodestart (parse-text texxt) req)))))
                           "angry birds problems"))
                   (div (a ((href ,(make-url (lambda (req) (nodestart (parse-text frenchtoo) req)))))
                           "learning french problems"))
                   (div (a ((href ,(make-url (lambda (req) (nodestart (parse-text markdown-text) req)))))
                           "markdown sample")))))]
    (send/suspend/dispatch response-generator)))

(define (nodestart n request)
  (show-nodes 'no-score-value n n request))

(define (logout request)
  (response/xexpr 
   #:headers (map cookie->header (list (make-logout-cookie)))
   `(p "you are logged out")))

(define (page404 request)
  (response/xexpr 
   `(html (body (p "sorry the page was not found!")))))

(define-values (teach-dispatch teach-url)
      (dispatch-rules
       [("dashboard") dashboard]
       [("hello") welcome]
       [("login") login]
       [("logout") logout]
       [("angry") (lambda (req) (nodestart (parse-text texxt) req))]))

;;TODO extra-files-path is not working, so no css is being loaded at the moment, however we could always run them in xginx or s;;3 buckets so no big deal, though would be nice if we get it to work.

(define (run-teach-server)
  (serve/servlet teach-dispatch
               #:quit? #t
               #:listen-ip #f
               #:servlet-path "/teach/"
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path "/Users/alokthapa/hacking/scheme/teach/"  "htdocs"))
               #:port 8000))
