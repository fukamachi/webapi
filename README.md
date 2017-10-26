# WebAPI

CLOS-based wrapper builder for Web APIs.

## Usage

```common-lisp
(ql:quickload '(:webapi :jonathan))

;; Define GitHub API request class.
(defclass github-request () ()
  (:default-initargs :base-uri "https://api.github.com"))
  
(defmethod webapi:parse ((request github-request) response)
  (jojo:parse (webapi:response-body response) :as :alist))

;; Request class for /users/:name/repos.
(defclass user-repositories (github-request)
  ((user :initarg :user))
  (:metaclass webapi:request-class)
  (:http :get))
  
(defmethod webapi:request-path ((request user-repositories))
  (format nil "/users/~A/repos" (slot-value request 'user)))

;; Request class for /search/repositories.
(defclass search-repositories (github-request)
  ((q :initarg :q))
  (:metaclass webapi:request-class)
  (:http :get "/search/repositories"))
  
(defmethod webapi:query-parameters ((request search-repositories))
  `(("q" . ,(slot-value request 'q))))
  
;;
;; Send a request

;; Get repositories of "fukamachi".
(webapi:send (make-instance 'user-repositories :user "fukamachi"))

;; Search repositories with related to "Common Lisp".
(webapi:send (make-instance 'search-repositories :q "Common Lisp"))
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2017 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
