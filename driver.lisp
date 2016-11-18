;;quicklisp should be installed
;;(load "~/quicklisp/setup.lisp")
;;(ql:add-to-init-file) permanently adds quicklisp to init-file, should be done automaticly
;;(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                         (user-homedir-pathname))))
;;    (when (probe-file quicklisp-init)
;;      (load quicklisp-init)))
;; above are strings which ql:add-to-init-file adds to ~/.sbclrc

(ql:quickload :drakma)


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

(defun prepare-parameter (param str)
  (when (equal (subseq str 0 (length param)) param) (string-right-trim " " (subseq str (+ (length param) 1)))))

(defun read-db-config ()
  (with-open-file (stream "db.config")
  (let ((address (prepare-parameter "address" (read-line stream)))
        (port (prepare-parameter "port" (read-line stream)))
        (username (prepare-parameter "username" (read-line stream)))
        (password (prepare-parameter "password" (read-line stream))))
     (setf *connection* (make-instance 'connection :address address :port port :username username :password password)))))
     
;;(require "asdf")
;;(asdf:load-system :uiop)
;;(uiop:run-program "./exist/bin/client.sh -m /db/testcol77 -p /home/the-barm/exist/xml/shakespeare -P admin" :output *standard-output*)

(defun check-connection ()
     (if *connection*
	    *connection*
	    (format t "Cannot find active connection. Ensure that you have file db.config \
and it contains proper information about your connection\
or setup it using (make-config :address 'youraddress' :port (by default is '8080') :username 'name' :password 'password')")))

(defun make-config (&key address (port "8080") username password)
     (setf *connection* (make-instance 'connection :address address :port port :username username :password password)))

(defmacro with-drakma-http-request (address method-type &key content-type content parameters)
  `(drakma:http-request ,(concatenate 'string "http://" (address *connection*) ":" (port *connection*) "/exist/rest/db/" address)
                         :method ,method-type
                         :content-type ,content-type
                         :basic-authorization '(,(username *connection*) ,(password *connection*))
                         :content ,content
                         :parameters ',parameters))

(defmacro get-document (address)
  `(with-drakma-http-request ,address :get))
;;(get-document "shakespeare/hamlet.xml")
;;is equal to
;;(drakma:http-request "http://localhost:8080/exist/rest/db/shakespeare/hamlet.xml"
;;                         :method :get
;;                         :content-type "application/xml"
;;                         :basic-authorization '("admin" "admin"))
                   
;;if path to file is specified in :content -- binary contents of the file are sent (so I need to create some wrapper-func which will read file, mb
;;check it for validity and then send as string (mb not the bes idea for large files -- should think about something more fast

(defun load-document (address content-type content)
  (with-drakma-http-request address :put :content-type :content-type :content content))

;;(load "mycol2/mdoc.xml" "application/xml" "<test>test</test>")
;;is equal to
;;(drakma:http-request "http://localhost:8080/exist/rest/db/mycol2/mdoc.xml"
;;                         :method :put
;;                         :content-type "application/octet-stream" :content "<test>test</test>"
;;                         :basic-authorization '("admin" "admin"))

                         
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

(defmacro create-collection (name address)
  `(with-drakma-http-request ,address :get :parameters 
			    ,(make-request-parameters :query 
						      (concatenate 'string "xmldb:create-collection('" address "', '" name "')"))))

;;(create-collection "mycol2" "testcol")
;;is equal to 
;;(drakma:http-request "http://localhost:8080/exist/rest/db"
;;                         :method :get
;;                         :basic-authorization '("admin" "admin")
;;                         :parameters '(("_query" . "xmldb:create-collection('mycol2', 'testcol')")))
                         
