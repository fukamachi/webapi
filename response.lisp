(defpackage #:webapi/response
  (:use #:cl)
  (:export #:response
           #:response-status
           #:response-headers
           #:response-body
           #:response-uri))
(in-package #:webapi/response)

(defclass response ()
  ((status :initarg :status
           :reader response-status)
   (headers :initarg :headers
            :reader response-headers)
   (body :initarg :body
         :reader response-body)
   (uri :initarg :uri
        :reader response-uri)))
