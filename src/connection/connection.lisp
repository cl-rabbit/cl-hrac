(in-package :cl-hrac)

(defclass connection ()
  ((spec :initform (make-connection-spec nil) :initarg :spec :reader connection-spec)))

(defgeneric connection.get (connection path))

(defgeneric connection.post (connection path params))

(defgeneric connection.put (connection path params))

(defgeneric connection.delete (connection path))

(defun assemble-path-from-spec (spec path)
  (format nil "~:[http~;https~]://~a:~a~a/~a"
          (connection-spec-use-tls-p spec)
          (connection-spec-host spec)
          (connection-spec-port spec)
          (connection-spec-endpoint spec)
          path))

(defclass drakma-connection (connection)
  ())

(defun decode-resource (res)
  (ia-hash-table:alist-ia-hash-table res))

(defun decode-collection (col)
  (map (type-of col) #'decode-resource col))

(defun decode-response (resp)
  (typecase resp
    (vector (decode-collection resp))
    (list (decode-resource resp))))

(defmethod connection.get ((connection drakma-connection) path)
  (multiple-value-bind (body status-code)
      (drakma:http-request (assemble-path-from-spec (connection-spec connection) path)
                           :accept "application/json"
                           :basic-authorization (list (connection-spec-login (connection-spec connection))
                                                      (connection-spec-password (connection-spec connection))))
    (if (= (floor status-code 100) 2)
        (let ((body-string (or (and (stringp body) body) (babel:octets-to-string body)))
              (yason:*parse-json-arrays-as-vectors* t)
              (yason:*parse-object-as* :alist))
          (decode-response (yason:parse body-string)))
        (error "Connection.get ~a:~a" path status-code))))

(defmethod connection.post ((connection drakma-connection) path content)
  (multiple-value-bind (body status-code)
      (drakma:http-request (assemble-path-from-spec (connection-spec connection) path)
                           :method :post
                           :content-type "application/json"
                           :content content
                           :basic-authorization (list (connection-spec-login (connection-spec connection))
                                                      (connection-spec-password (connection-spec connection))))
    (if (= (floor status-code 100) 2)
        (if body
            (let ((body-string (trivial-utf-8:utf-8-bytes-to-string body)))
              (decode-response (yason:parse body-string)))
            t)
        nil)))

(defmethod connection.put ((connection drakma-connection) path content)
  (multiple-value-bind (body status-code)
      (drakma:http-request (assemble-path-from-spec (connection-spec connection) path)
                           :method :put
                           :content-type "application/json"
                           :content content
                           :basic-authorization (list (connection-spec-login (connection-spec connection))
                                                      (connection-spec-password (connection-spec connection))))
    (if (= (floor status-code 100) 2)
        (if body
            (let ((body-string (trivial-utf-8:utf-8-bytes-to-string body)))
              (decode-response (yason:parse body-string)))
            t)
        nil)))

(defmethod connection.delete ((connection drakma-connection) path)
  (multiple-value-bind (body status-code)
      (drakma:http-request (assemble-path-from-spec (connection-spec connection) path)
                           :method :delete
                           :basic-authorization (list (connection-spec-login (connection-spec connection))
                                                      (connection-spec-password (connection-spec connection))))
    (if (= (floor status-code 100) 2)
        (if body
            (let ((body-string (trivial-utf-8:utf-8-bytes-to-string body)))
              (decode-response (yason:parse body-string)))
            t)
        nil)))
