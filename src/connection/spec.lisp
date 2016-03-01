(in-package :cl-hrac)

(define-constant +connection-endpoint+ "api" :test #'equal)
(define-constant +connection-host+ "localhost" :test #'equal)
(define-constant +connection-port+ 15672)
(define-constant +connection-login+ "guest" :test #'equal)
(define-constant +connection-password+ "guest" :test #'equal)

(defstruct (connection-spec (:constructor make-connection-spec%))
  (endpoint +connection-endpoint+ :type string)
  (host +connection-host+ :type string)
  (port +connection-host+ :type fixnum)
  (login +connection-login+ :type string)
  (password +connection-password+ :type string)
  (use-tls-p nil :type boolean)
  (ssl-options nil :type list))

(defun maybe-unescape-component (value)
  (when value
    (quri:url-decode value)))

(defun parse-user-info (userinfo)
  (destructuring-bind (login &optional (password +connection-password+))
      (split-sequence:split-sequence #\: userinfo)
    (list (maybe-unescape-component login) (maybe-unescape-component password))))

(defun check-connection-string-scheme (scheme)
  (assert (or (equal scheme "http")
              (equal scheme "https")))
  (if (equal scheme "https")
      t))

(defun check-connection-string-port (port)
  (or port +connection-port+))

(defun check-connection-string-credentials (userinfo)
  (cond
    ((or (null userinfo)
         (equal "" userinfo))
     '(#.+connection-login+ #.+connection-password+))
    ((stringp userinfo)
     (parse-user-info userinfo))))

(defmethod make-connection-spec ((raw string))
  (multiple-value-bind (scheme userinfo host port path query fragment)
      (quri:parse-uri raw)
    (declare (ignore fragment query))
    (let ((use-tls (check-connection-string-scheme scheme))
          (port (check-connection-string-port port))
          (credentials (check-connection-string-credentials userinfo)))
      (make-connection-spec% :use-tls-p use-tls
                             :host host
                             :port port
                             :endpoint path
                             :login (first credentials)
                             :password (second credentials)))))
