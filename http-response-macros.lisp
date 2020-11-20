
(in-package :http-response)

(defparameter *session* nil "HTTP session object")
(defparameter *request* nil "The HTTP request will be kept here for future reference...")
(defparameter *entity* nil "The entity thing from allegroserve.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Also in this file: a load of stuff for the use of delegates to take
;; the tedium out of much of the response generation...

(defmacro with-request (req ent &body body)
  `(let ((*request* ,req)
         (*entity* ,ent))
     ,@body))

;; this must be used in the right place
(defmacro with-session ((name) &body forms)
  `(let ((*session* (when ,name
                      (get-or-create-session ,name))))
     ,@forms
     (when (and ,name
                (session::empty-sessionp *session*))
       (session::delete-session *session*))))


(defparameter *process-task* (make-hash-table :test #'equal))

(defun get-or-create-session (&optional site)
  (when (not site)
    (setf site "*"))
  (let ((session (session:get-request-session site *request*)))
    ;; get the cookie or create if it does not exist...
    (when (not session)
      (let ((session-id (session:make-session site)))
        (setf session (session:get-session session-id))
        (session:add-session-to-cookie *request* session-id)))

    ;; !!! This assumes uniqueness of process name, so I should try to enforce that with any processes that I create
    ;; This isn't really important (I forgot I did this!)
    #+ccl(setf (gethash (ccl:process-name ccl:*current-process*)
                        *process-task*)
               (cons (request-raw-uri *request*)
                     session))
    
    session))

;; This will give us an HTML 5 document in fact. I should check whether that causes us any problems. It seems to make IE behave better though. More testing is needed here.
;; I"m defining this as a function so I don't have to recompile all the pages whenever I want to change it.
(defun output-default-doctype ()
  (com.gigamonkeys.html:html (:noescape "<!DOCTYPE html>")))

(defun output-standard-style-sheets ()
  (com.gigamonkeys.html:html
    (:link :rel "STYLESHEET" :type "text/css" :href "/pub/print-style.css" :media "print")
    (:link :rel "STYLESHEET" :type "text/css" :href "/pub/common-style.css")))


;; Default definition
(defmacro user-visible-condition-handler (&body forms)
  `(progn ,@forms))

;; reply by writing out an html page. This takes care of making the usual page stuff
;; *** css-styles not handled
(defmacro html-page-reply ((&key title (style-sheet "/style.css") (session-name "<default>") timeout dtd allow-browser-caching head (include-standard-stylesheets t)) &body forms)
  `(http-reply ,(when session-name `(:session-name ,session-name :timeout ,timeout))
     (com.gigamonkeys.html:html
       ,@(if dtd
             `((:noescape ,dtd))
             `((output-default-doctype)))
       (:html (:head
               (:title ,(if (listp title) `(:print ,title) title))
               ;; *** This doesn't seem to work properly
               ,@(unless allow-browser-caching
                         `((:meta :http-equiv "Pragma" :content "no-cache")
                           (:meta :http-equiv "Expires" :content "-1")))

               ,@(when include-standard-stylesheets
                       '((output-standard-style-sheets)))
               
               ,@(if style-sheet
                     `((:link :rel "STYLESHEET" :type "text/css" :href ,style-sheet)))
               ;; (:link :rel "STYLESHEET" :type "text/css" :href "/pub/print-style.css" :media "print")
               ;; (:link :rel "STYLESHEET" :type "text/css" :href "/pub/common-style.css")
               ,@(when head (list head))
               
               ;; (:meta :name "viewport" :content "initial-scale = 0.8")
               )
              
              (:body 
               (user-visible-condition-handler
                 (com.gigamonkeys.html:html ,@forms)))))))


;; *** I want a timeout for this so the user can see the body content
(defmacro redirect-reply ((&key location retry-after timeout http-head-forms) &body forms)
  `(http-reply (:http-head-forms
               ((setf (reply-header-slot-value *request* :location)
                      ,location)
                ,@(when retry-after
                   `((setf (reply-header-slot-value *request* :retry-after) ,retry-after)))
                ,@http-head-forms)
               :response *response-found* :timeout ,timeout)
     ,@forms))

;; general http replying macro - thin wrapper around the aserve provided stuff + session handling
(defmacro http-reply ((&key (response '*response-ok*) (content-type "text/html; charset=utf-8") (session-name "<default>") timeout http-head-forms)
                      &body forms)
  (let ((in-resp `(with-http-body (*request* *entity*)
                    (com.gigamonkeys.html:with-html-output ((request-reply-stream *request*))
                      ,@forms))))
    `(with-http-response (*request* *entity* :response ,response :content-type ,content-type ,@(when timeout `(:timeout ,timeout)))
       ;; we have to allow more head forms for, eg, redirects. I'll wrap those.
       ,(if session-name
            `(with-session (,session-name)
               ,@http-head-forms
               ,in-resp)
            `(progn
               ,@http-head-forms
               ,in-resp)))))

(defmacro response-function (args &body body)
  (let ((req (gensym "req"))
        (ent (gensym "ent")))
    `#'(lambda (,req ,ent)
         (let* ((*request* ,req)
                (*entity* ,ent)
                ,@(loop for a in args
                     collect (list a `(query-value ,(string-downcase (symbol-name a))))))
           ,@body))))


;; !!! This is annoyingly similar to the previous macro - I should factor out this similarity. Patterny code is bad
(defmacro ajax-response-function (args &body body)
  (let ((req (gensym "req"))
        (ent (gensym "ent")))
    `#'(lambda (,req ,ent)
         (let* ((*request* ,req)
                (*entity* ,ent)
                ,@(loop for a in args
                     collect (list a `(query-value ,(string-downcase (symbol-name a))))))
           ,@body))))

(setf *default-aserve-external-format*
      :latin1-base)

(when nil
  (setf *default-aserve-external-format*
        :utf8-base))

;; thin wrapper...
(defmethod query-value (key)
  (request-query-value key *request*))
