(uiop:define-package #:webapi
  (:nicknames #:webapi/main)
  (:use #:cl)
  (:use-reexport #:webapi/request
                 #:webapi/response))
(in-package #:webapi)
