
;;;;;; http-response.asd

(asdf:defsystem :http-response
  :description "HTTP response macros"
  :author "David Ritchie"
  :serial t
  :depends-on (#:session #:html ; gigamonkeys
                         )
  :components ((:file "package")
               (:file "http-response-macros")))
