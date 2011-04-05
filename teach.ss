#lang racket
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/servlet-env)
(require web-server/http/cookie)
(require web-server/http/cookie-parse)
(require web-server/http/redirect)
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
        (find-user 
         (client-cookie-value 
          (findf (lambda (c)
                   (and (client-cookie-name c)
                        (string=? "id" (client-cookie-name c))))
                 cooks)))
        #f)))

(define (login/out req)
  (let ((usr (get-user-from-cookie req)))
    (if usr
        `(span (a ((href ,(teach-url dashboard))) ,(user-name usr))
               (a ((href ,(teach-url logout))) "--logout"))
        `(a ((href ,(teach-url login))) "login"))))

(define (common-layout req #:scripts (scripts '()) body)
  (response/xexpr
   `(html
     (head
      (title "Satori")
      (link ((href "/style/teach.css") (rel "stylesheet") ( type "text/css")))
      (script ((src "/js/jquery-1.5.min.js")) "")
      (script ((src "/js/jquery.hotkeys.js")) "")
      (link ((rel "stylesheet") (type "text/css") (href "http://yui.yahooapis.com/2.8.2r1/build/grids/grids-min.css")))
      ,@(map (lambda (s) `(script ((src ,s)) "")) scripts))
     (body
	   (div ((id "doc"))
                (div ((id "hd")(class "head"))
                     (a ((href "/index")) "satori ")
                     ,(login/out req))
                (div ((id "bd")) ,body)
                (div ((id "ft")) "satoriapp.com"))))))

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
          (define (end-of-show _)
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

(define (logout request)
  (response/xexpr 
   #:headers (map cookie->header (list (make-logout-cookie)))
   `(html 
     (head
      (meta ((http-equiv "refresh") (content "0;url=/index"))))
     (body (p "please wait...")))))


(define (login request )
  (local [(define (parse-username bindings)
            (extract-binding/single 'username bindings))
          (define (parse-password bindings)
            (extract-binding/single 'password bindings))
          (define (parse-rpassword bindings)
            (extract-binding/single 'rpassword bindings))
          (define (redirect-to-dashboard usr)
            (send/suspend (lambda _
                            (response/xexpr
                             #:headers (map cookie->header (list (make-user-cookie usr)))
                             `(html 
                               (head
                                (meta ((http-equiv "refresh") (content "0;url=/dashboard"))))
                               (body (p "please wait...")))))))
          (define (register-handler request)
            (let* ((binds (request-bindings request))
                   (existing-usr (find-user (parse-username binds))))
              (cond
                (existing-usr (send/suspend/dispatch (response-generator "The user already exists. Please select another.")))
                ((string=? (parse-password binds)
                           (parse-rpassword binds))
                 (redirect-to-dashboard (create-user! (parse-username binds) (parse-password binds))))
                ((string=? (parse-username binds) "")
                 (send/suspend/dispatch (response-generator "Invalid username.")))
                (else (send/suspend/dispatch (response-generator "Passwords don't match."))))))
          (define (login-handler request)
            (let* ((binds (request-bindings request))
                   (usr (find-user (parse-username binds))))
              (if (and usr 
                       (string=? (parse-password binds)
                                 (user-pwd usr)))
                  (redirect-to-dashboard usr)
                  (send/suspend/dispatch (response-generator "Invalid username or password.")))))
          (define (response-generator (msg ""))
            (lambda (make-url)
              (common-layout
               request
               `(div ((class "login-div"))
                     (p ,msg)
                     (h1 "Login")
                     (form ((action ,(make-url login-handler)))
                           (table 
                            (tr
                             (td (p "username"))
                             (td (input ((name "username")))))
                            (tr
                             (td (p "password"))
                             (td (input ((name "password"))))))
                           (input ((type "submit"))))
                     (h1 "Register")
                     (form ((action ,(make-url register-handler)))
                           (table
                            (tr
                             (td (p "username"))
                             (td (input ((name "username")))))
                            (tr
                             (td (p "password"))
                             (td (input ((name "password") (type "password")))))
                            (tr
                             (td (p "confirm password"))
                             (td (input ((name "rpassword")(type "password"))))))
                           (input ((type "submit"))))))))]
    (send/suspend/dispatch (response-generator))))

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

(define (create-quickquiz request)
   (local [(define (parse-qqname bindings)
            (extract-binding/single 'qqname bindings))
           (define (parse-qqdata bindings)
            (extract-binding/single 'qqdata bindings))
          (define (handler req)
            (let ((binds (request-bindings req))
                  (usr (get-user-from-cookie req)))
              (create-quickquiz! (user-id usr) (parse-qqname binds) (parse-qqdata binds) "score" )
              (redirect-to (teach-url dashboard))))          
          (define (response-generator make-url)
            (common-layout
             #:scripts '("/js/underscore.js" "/js/proper.js" "/js/edit.js")
             request
             `(form ((method "post") (action ,(make-url handler)))
                    (label () "Name") (input ((name "qqname")) "") (br)
                    (label () "Quiz") 
                    (textarea ((name "qqdata") (class "qqdata") (rows "20") (cols "80")) "")(br)
                    (input ((type "submit"))))))]
    (send/suspend/dispatch response-generator)))

(define (edit-quickquiz qq request)
   (local [(define (parse-qqname bindings)
            (extract-binding/single 'qqname bindings))
           (define (parse-qqdata bindings)
            (extract-binding/single 'qqdata bindings))
          (define (handler req)
            (let ((binds (request-bindings req))
                  (usr (get-user-from-cookie req)))
              (set-quickquiz-name! qq (parse-qqname binds))
              (set-quickquiz-data! qq (parse-qqdata binds))
              (update-quickquiz! qq)
              (redirect-to (teach-url dashboard))))          
          (define (response-generator make-url)
            (common-layout
             #:scripts '("/js/underscore.js" "/js/proper.js" "/js/edit.js")
             request
             `(form ((method "post") (action ,(make-url handler)))
                    (label () "Name") (input ((name "qqname" ) (value ,(quickquiz-name qq)))) (br)
                    (label () "Quiz") 
                    (textarea ((name "qqdata") (class "qqdata") (rows "20") (cols "80")) ,(quickquiz-data qq))(br)
                    (input ((type "submit"))))))]
    (send/suspend/dispatch response-generator)))
  
(define (create-teachpack request)
  (local [(define (parse-teachpackname bindings)
            (extract-binding/single 'teachname bindings))
          (define (teachpack-handler req)
            (let ((binds (request-bindings req))
                  (usr (get-user-from-cookie req)))
              (create-teachpack!(user-id usr) (parse-teachpackname binds))
              (redirect-to (teach-url dashboard))))          
          (define (response-generator make-url)
            (common-layout
             request
             `(form ((action ,(make-url teachpack-handler)))
                    (input ((name "teachname")))
                    (input ((type "submit"))))))]
    (send/suspend/dispatch response-generator)))

(define (view-quiz request quizid)
  (let ((quiz (get-quickquiz-from-id quizid)))
    (if quiz
        (nodestart (parse-text (quickquiz-data quiz)) request)
        (common-layout
         request
         `(div "not found")))))
         

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

(define (publicquiz request)
  (local [(define (response-generator make-url)
            (common-layout
             request
             `(div 
               (h1 "Take a quiz")
               ,@(map (lambda (q) `(div (a ((href ,(string-append "/quiz/" (number->string (quickquiz-id q)))))
                                      ,(quickquiz-name q))))
                      (get-all-quickquiz)))))]
    (send/suspend/dispatch response-generator)))

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
  `(div 
    (h1 ,header)
    (a ((class "create") (href ,(make-url fn-create))) "Create New")
    (p)
    (div
     ,@(map (lambda (obj)
              `(div ((class "list"))
                    (a ((href ,(make-url (lambda (r) (fn-obj obj r)))))
                       ,(fn-name obj))))
            objs))))


(define (create-or-edit-quickquiz make-url objs)
  `(div
    (h1 "Quickquiz")
    (a ( (class "create") (href ,(make-url create-quickquiz))) "Create New")
    (p)
    (div 
     ,@(map (lambda (obj)
              `(div ((class "list"))
                    ,(string-append (quickquiz-name obj) "  ")
                    (a ((href ,(make-url (lambda (r) (edit-quickquiz obj r)))))
                       "edit")
                     (a ((href ,(make-url (lambda (r) (begin
                                                        (delete-quickquiz! obj)
                                                        (dashboard r))))))
                       "delete")
                    (a ((href ,(string-append "/quiz/" (number->string (quickquiz-id obj)))))
                       "run")))
            objs))))
   
(define (dashboard request)
  (let ((usr (get-user-from-cookie request)))
    (local [(define (build-quickquiz make-url)
              (create-or-edit-quickquiz 
               make-url
               (get-quickquiz-for-user (user-id usr))))
            (define (build-teachpack make-url)
              (create-and-list "TeachPacks" 
                               make-url 
                               (get-teachpacks-for-user (user-id usr))
                               create-teachpack
                               teachpack-page
                               teachpack-name))
            (define (logintodo make-url)
              (common-layout
               request
               `(div "You have to be logged in to do that")))
            (define (response-generator make-url)
              (common-layout
               request
               `(div ((class "dashboard-div"))
                     ,(build-quickquiz make-url))))]
      (if usr
          (send/suspend/dispatch response-generator )
          (send/suspend/dispatch logintodo)))))

(define (welcome request)
  (local [(define (response-generator make-url)
            (common-layout 
             request
             `(div ((class "welcome-div"))
                   (h1 "Take a quiz")
                   ,@(map (lambda (q) `(div (a ((href ,(string-append "/quiz/" (number->string (quickquiz-id q)))))
                                      ,(quickquiz-name q))))
                      (get-all-quickquiz)))))]
    (send/suspend/dispatch response-generator)))

(define (nodestart n request)
  (show-nodes 'no-score-value n n request))

(define (page404 request)
  (response/xexpr 
   `(html (body (p "sorry the page was not found!")))))

(define-values (teach-dispatch teach-url)
      (dispatch-rules
       [("dashboard") dashboard]
       [("play") publicquiz]
       [("index") welcome]
       [("login") login]
       [("logout") logout]
       [("quiz" (string-arg)) view-quiz]
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
