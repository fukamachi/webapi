(defpackage #:webapi/codable
  (:use #:cl)
  (:import-from #:closer-mop
                #:standard-direct-slot-definition
                #:direct-slot-definition-class
                #:validate-superclass
                #:class-direct-slots
                #:slot-definition-name
                #:slot-definition-initargs
                #:slot-definition-type)
  (:import-from #:st-json
                #:jso
                #:jso-alist
                #:json-null
                #:json-bool
                #:from-json-bool)
  (:import-from #:alexandria
                #:alist-hash-table)
  (:export #:codable
           #:codable-class
           #:decode-object
           #:defcodable))
(in-package #:webapi/codable)

(defclass codable () ())

(defvar *conc-name* nil)

(defclass codable-slot-class (c2mop:standard-direct-slot-definition)
  ((key :type (or string null)
        :initarg :key
        :initform nil
        :accessor codable-slot-key)))

(defmethod initialize-instance :around ((class codable-slot-class) &rest rest-initargs
                                                                   &key name
                                                                   &allow-other-keys)
  ;; Add the default initarg.
  (pushnew (intern (symbol-name name) :keyword)
           (getf rest-initargs :initargs))

  (when *conc-name*
    (let ((default-accessor (intern
                              (format nil "~:@(~A~A~)" *conc-name* name)
                              *package*)))
      (pushnew default-accessor (getf rest-initargs :readers))
      (pushnew `(setf ,default-accessor) (getf rest-initargs :writers))))

  (apply #'call-next-method class rest-initargs))

(defclass codable-class (standard-class)
  ((conc-name :initarg :conc-name
              :initform nil)
   (key-mapper :initform (make-hash-table :test 'equal))))

(defmethod c2mop:direct-slot-definition-class ((class codable-class) &key &allow-other-keys)
  'codable-slot-class)

(defmethod c2mop:validate-superclass ((class codable-class) (super standard-class))
  t)

(define-condition conversion-failed (error) ())

(defun make-slot-converter (type)
  (if (consp type)
      (progn
        (assert (eq (first type) 'or))
        (lambda (val)
          (block nil
            (dolist (type (rest type) (error 'conversion-failed))
              (handler-case
                  (return (decode-jso-as-type val type))
                (conversion-failed ()))))))
      (lambda (val)
        (decode-jso-as-type val type))))

(defun build-slot-mapper (class)
  (let ((mapper (slot-value class 'key-mapper)))
    (dolist (slot (c2mop:class-direct-slots class))
      (let ((key (or (codable-slot-key slot)
                     (let ((*print-case* :downcase))
                       (princ-to-string (c2mop:slot-definition-name slot))))))

        (setf (gethash key mapper)
              (cons (first (c2mop:slot-definition-initargs slot))
                    (make-slot-converter
                      (or (c2mop:slot-definition-type slot) t))))))))

(defmethod initialize-instance :around ((class codable-class) &rest initargs &key conc-name &allow-other-keys)
  (let ((*conc-name* (first conc-name)))
    (let ((class (apply #'call-next-method class initargs)))
      (build-slot-mapper class)
      class)))

(defmethod reinitialize-instance :around ((class codable-class) &rest initargs &key conc-name &allow-other-keys)
  (let ((*conc-name* (first conc-name)))
    (let ((class (apply #'call-next-method class initargs)))
      (build-slot-mapper class)
      class)))

(defun decode-jso-as-type (value type)
  (case type
    (null
     (unless (typep value 'json-null)
       (error 'conversion-failed))
     nil)
    (boolean (from-json-bool value))
    ((string integer float rational number)
     (handler-case (coerce value type)
       (error () (error 'conversion-failed))))
    (hash-table
      (if (typep value 'st-json:jso)
          (alist-hash-table (st-json::jso-alist value))
          (error 'conversion-failed)))
    ('t (typecase value
          (json-null nil)
          (json-bool (from-json-bool value))
          (otherwise value)))
    (otherwise
      (decode-object value type))))

(defgeneric decode-object (input class)
  (:method (input class)
    (error 'conversion-failed))
  (:method (input (class null))
    (if (typep input 'json-null)
        nil
        (error 'conversion-failed)))
  (:method (input (class symbol))
    (decode-object input (find-class class)))
  (:method ((input st-json:jso) (class codable-class))
    (let ((mapper (slot-value class 'key-mapper)))
      (apply #'make-instance class
             (loop for (key . val) in (jso-alist input)
                   append (destructuring-bind (init-key . converter)
                              (or (gethash key mapper)
                                  (error "Undefined key ~S (= ~S) in ~A"
                                         key val
                                         (class-name class)))
                            (list init-key
                                  (funcall converter val))))))))

(defmacro defcodable (name superclasses slots &rest class-options)
  `(defclass ,name (codable ,@superclasses)
     ,slots
     (:metaclass codable-class)
     (:conc-name ,(format nil "~A-" name))
     ,@class-options))
