;;This file contains logic for eXist-db driver
;;drakma http-client is used to transfer http-requests using
;;eXist-db's built-in REST API
;;for correct work driver should be installed through Quicklisp
;;using command (ql:quickload :existcl)

(in-package #:existcl)

(defclass connection ()
  ((address
    :initarg :address
    :accessor address)
   (port
    :initarg :port
    :initform "8080"
    :accessor port)
   (username
    :initarg :username
    :accessor username)
   (password
    :initarg :password
    :accessor password)))

(defparameter *connection* nil)

(defparameter *compile-path* *load-truename*)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))

(defun translate-boolean (arg)
  (cond ((string-equal arg "true") t)
        ((string-equal arg "false") nil)
        (t (error "The value of: ~A is not of boolean type" arg))))

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
                 syms)
     ,@body))

(defun prepare-parameter (param str)
  (when (equal (subseq str 0 (length param)) param)
    (string-right-trim " " (subseq str (+ (length param) 1)))))

(defun check-connection ()
  (if *connection*
      *connection*
      (error "Cannot find active connection. Ensure that you've created file db.config \
and it contains proper information about your connection\
or setup it using (update-db-config-file (&key address port username password)) or \
(make-config :address \"youraddress\" :port (by default is \"8080\") :username \"name\" :password \"password\")")))

(defun with-drakma-http-request (addr method-type &key content-type content parameters return-text)
  (let* ((connection (check-connection))
         (result (multiple-value-list
                  (drakma:http-request
                   (concatenate 'string
                                "http://" (address connection) ":" (port connection) "/exist/rest/db/" addr)
                   :method method-type
                   :content-type content-type
                   :basic-authorization `( ,(username connection)
                                            ,(password connection))
                   :content content
                   :parameters parameters))))
    (if (or (eq (second result) 200)
            (eq (second result) 201))
        (if return-text
            (if (stringp (first result))
                (first result)
                (babel:octets-to-string (first result)))
            t)
        (progn
          (warn (seventh result))
          nil))))

(defmacro define-driver-func (name args documentation defaults &body body)
  `(defun ,name ,args
     ,documentation
     (let ((request (with-drakma-http-request ,(car args) ,@defaults)))
       ,@body)))

(defun parse-resulting-xml (target &key can-be-null multiple)
  (if target
      (if (xmls:xmlrep-children (xmls:parse target))
          (if multiple
              (xmls:xmlrep-find-child-tags
               "value"
               (xmls:parse target))
              (xmls:xmlrep-find-child-tag
               "value"
               (xmls:parse target)))
          (if can-be-null
              nil
              (error "Requested address is not found")))
      (error "Query returned null")))

(defun make-request-parameters (&key query indent encoding howmany start wrap source cache session release)
  (let ((params))
    (when query (push (cons "_query" query) params))
    (when indent (push (cons "_indent" indent) params))
    (when encoding (push (cons "_encoding" encoding) params))
    (when howmany (push (cons "_howmany" howmany) params))
    (when start (push (cons "_start" start) params))
    (when wrap (push (cons "_wrap" wrap) params))
    (when source (push (cons "_source" source) params))
    (when cache (push (cons "_cache" cache) params))
    (when session (push (cons "_session" session) params))
    (when release (push (cons "_release" release) params))
    params))

(defun file-to-string (path)
  (with-output-to-string (out)
    (let ((in (open path :if-does-not-exist nil)))
      (when in
        (loop for line = (read-line in nil)
           while line do (write-line line out))
        (close in)))
    out))

(defun divide-path (path)
  (if (position #\/ path)
      (values (subseq path 0 (1+ (position #\/ path :from-end t)))
              (subseq path (1+ (position #\/ path :from-end t))))
      (values "" path)))

