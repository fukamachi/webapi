(defpackage #:webapi/request
  (:use #:cl)
  (:import-from #:webapi/response
                #:response)
  (:import-from #:closer-mop)
  (:import-from #:dexador)
  (:import-from #:quri)
  (:import-from #:kebab)
  (:export #:request
           #:request-class
           #:http-method
           #:http-uri
           #:request-path
           #:request-parameters
           #:query-parameters
           #:body-parameters
           #:request-headers
           #:send
           #:parse))
(in-package #:webapi/request)

(defun contains-class-or-subclasses (class target-classes)
  (let ((class (if (typep class 'class)
                   class
                   (find-class class))))
    (find-if (lambda (target-class)
               (let ((target-class (if (typep target-class 'class)
                                       target-class
                                       (find-class target-class nil))))
                 (and target-class
                      (or (eq target-class class)
                          (subtypep target-class class)))))
             target-classes)))

(defclass request ()
  ((base-uri :initarg :base-uri)))

(defclass request-class (standard-class)
  ((http :initarg :http)))

(defmethod c2mop:validate-superclass ((class request-class) (super standard-class))
  t)

(defmethod initialize-instance :around ((class request-class) &rest initargs
                                        &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'request direct-superclasses)
    (push (find-class 'request) (getf initargs :direct-superclasses)))

  (apply #'call-next-method class initargs))

(defmethod reinitialize-instance :around ((class request-class) &rest initargs
                                          &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'request direct-superclasses)
    (push (find-class 'request) (getf initargs :direct-superclasses)))

  (apply #'call-next-method class initargs))

(defgeneric http-method (request)
  (:method ((request request))
    (first (slot-value (class-of request) 'http))))

(defgeneric http-uri (request)
  (:method ((request request))
    (format nil "~A~:[~;~:*~A~]~:[~;~:*?~A~]"
            (slot-value request 'base-uri)
            (request-path request)
            (and (eq (http-method request) :get)
                 (quri:url-encode-params (query-parameters request))))))

(defgeneric request-path (request)
  (:method ((request request))
    (second (slot-value (class-of request) 'http))))

(defgeneric request-parameters (request)
  (:method ((request request))
    '()))

(defgeneric query-parameters (request)
  (:method ((request request))
    (when (eq (http-method request) :get)
      (request-parameters request))))

(defgeneric body-parameters (request)
  (:method ((request request))
    (unless (eq (http-method request) :get)
      (request-parameters request))))

(defgeneric request-headers (request)
  (:method ((request request))
    '()))

(defgeneric send (request)
  (:method ((request request))
    (multiple-value-bind (body status headers uri)
        (dex:request (http-uri request)
                     :method (http-method request)
                     :headers (request-headers request)
                     :content (body-parameters request))
      (let ((response (make-instance 'response
                                     :status status
                                     :headers headers
                                     :body body
                                     :uri uri)))
        (parse request response)))))

(defgeneric parse (request response)
  (:method (request response)
    (error "~S isn't implemented for ~S" 'parse (class-name (class-of request)))))
