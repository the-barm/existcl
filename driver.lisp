;;quicklisp should be installed
;;(load "~/quicklisp/setup.lisp")
;;(ql:add-to-init-file) permanently adds quicklisp to init-file, should be done automaticly
;;(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                         (user-homedir-pathname))))
;;    (when (probe-file quicklisp-init)
;;      (load quicklisp-init)))
;; above are strings which ql:add-to-init-file adds to ~/.sbclrc

(ql:quickload :drakma)
;;(ql:quickload :xmls)


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

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
                 syms)
     ,@body))

(defun prepare-parameter (param str)
  (when (equal (subseq str 0 (length param)) param)
    (string-right-trim " " (subseq str (+ (length param) 1)))))
     
;;(require "asdf")
;;(asdf:load-system :uiop)
;;(uiop:run-program "./exist/bin/client.sh -m /db/testcol77 -p /home/the-barm/exist/xml/shakespeare -P admin" :output *standard-output*)

(defun check-connection ()
     (if *connection*
	    *connection*
	    (error "Cannot find active connection. Ensure that you have file db.config \
and it contains proper information about your connection\
or setup it using (make-config :address \"youraddress\" :port (by default is \"8080\") :username \"name\" :password \"password\")")))

(defmacro with-drakma-http-request (addr method-type &key content-type content parameters return-text)
  (with-gensyms (result connection addr-t method-type-t content-type-t content-t params-t return-text-t)
    `(let* ((,connection ,(check-connection))
            (,addr-t ,addr)
            (,method-type-t ,method-type)
            (,content-type-t ,content-type)
            (,content-t ,content)
            (,params-t ,parameters)
            (,return-text-t ,return-text)
            (,result (multiple-value-list (drakma:http-request
                                           (concatenate 'string "http://" (address ,connection) ":" (port ,connection) "/exist/rest/db/" ,addr-t)
                                           :method ,method-type-t
                                           :content-type ,content-type-t
                                           :basic-authorization (list (username ,connection) (password ,connection))
                                           :content ,content-t
                                           :parameters ,params-t))))
       (declare (ignorable ,method-type-t ,content-type-t ,content-t ,params-t))
       (if (or (eq (second ,result) 200)
               (eq (second ,result) 201))
           (if ,return-text-t
               (if (stringp  (first ,result))
                   (first ,result)
                   (progn
                     (warn "Output is not a string!")
                     (first ,result)))
               t)
           (progn
             (warn (seventh ,result))
             nil)))))

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
      (values (subseq path 0 (1+ (position #\/ path :from-end t))) (subseq path (1+ (position #\/ path :from-end t))))
      (values "" path)))

;;INTERFACE FUNCTIONS
(defun read-db-config ()
  (with-open-file (stream "db.config")
  (let ((address (prepare-parameter "address" (read-line stream)))
        (port (prepare-parameter "port" (read-line stream)))
        (username (prepare-parameter "username" (read-line stream)))
        (password (prepare-parameter "password" (read-line stream))))
     (setf *connection* (make-instance 'connection :address address :port port :username username :password password)))))
;;(read-db-config)

(defun make-config (&key address (port "8080") username password)
     (setf *connection* (make-instance 'connection :address address :port port :username username :password password)))
;;(make-config :address "localhost" :port "8080" :username "admin" :password "admin")

(defmacro get-document (address)
  `(with-drakma-http-request ,address :get :return-text t))
;;(get-document "shakespeare/hamlet.xml")
;;is equal to
;;(drakma:http-request "http://localhost:8080/exist/rest/db/shakespeare/hamlet.xml"
;;                         :method :get
;;                         :content-type "application/xml"
;;                         :basic-authorization '("admin" "admin"))
                   
(defmacro put-xml-document (address content)
  `(with-drakma-http-request ,address :put :content-type "text/xml" :content ,(file-to-string content)))

;;(put-xml-document "kkka/mydoc.xml" "/home/the-barm/workspace/existcl/mdoc.xml")

(defmacro put-document (address content)
  `(with-drakma-http-request ,address :put :content-type "text/plain" :content ,(file-to-string content)))

;;(put-document "kkka/mydoc.xml" "/home/the-barm/workspace/existcl/mdoc.xml")

(defmacro put-document-from-string (address content-type content)
  `(with-drakma-http-request ,address :put :content-type ,content-type :content ,content))

;;(put-document-from-string "mycol2/mdoc.xml" "text/plain" "<test>test</test>")
;;(put-document-from-string "mycol2/mdoc.xml" "application/octet-stream" "<test>test</test>")
;;is equal to
;;(drakma:http-request "http://localhost:8080/exist/rest/db/mycol2/mdoc.xml"
;;                         :method :put
;;                         :content-type "application/octet-stream" :content "<test>test</test>"
;;                         :basic-authorization '("admin" "admin"))

(defmacro create-collection (address name)
  `(execute-query ,address ,`(concatenate 'string "xmldb:create-collection('/db/" ,address "', '" ,name "')")))

;;(create-collection "mycol2" "testcol")
;;is equal to 
;;(drakma:http-request "http://localhost:8080/exist/rest/db"
;;                         :method :get
;;                         :basic-authorization '("admin" "admin")
;;                         :parameters '(("_query" . "xmldb:create-collection('mycol2', 'testcol')")))

(defmacro delete-from-db (address)
  `(with-drakma-http-request ,address :delete))

;;(delete-from-db "mycol2/rararar")

;; "Bad request"
(defmacro get-permissions (address)
  `(execute-query ,address ,`(concatenate 'string "xmldb:get-permissions('" ,address "')")))

;;TODO: (collection-available)
						      
(defmacro move-collection (source direction)
  `(execute-query ,source ,`(concatenate 'string "xmldb:move('/db/" ,source "', '/db/" ,direction "')")))

;;(move-collection "kkka/123" "mycol2")
						      
(defmacro move-document (source direction)
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,source)
       (execute-query ,source ,`(concatenate 'string "xmldb:move('/db/" ,src "', '/db/" ,direction "', '" ,document "')")))))

;;(move-document "kkka/kkk.xml" "mycol2")
					  
(defmacro copy-collection (source direction)
  `(execute-query ,source ,`(concatenate 'string "xmldb:copy('/db/" ,source "', '/db/" ,direction "')")))

;;(copy-collection "kkka/123" "mycol2")
						      
(defmacro copy-document (source direction)
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,source)
       (execute-query ,source ,`(concatenate 'string "xmldb:copy('/db/" ,src "', '/db/" ,direction "', '" ,document "')")))))

;;(copy-document "kkka/kkk.xml" "mycol2")

					  
(defmacro rename-collection (path new-name)
  `(execute-query ,path ,`(concatenate 'string "xmldb:rename('/db/" ,path "', '" ,new-name "')")))
;;(rename-collection "adasd" "kkka")
						      
(defmacro rename-document (path new-name)
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,path)
       (execute-query ,path ,`(concatenate 'string "xmldb:rename('/db/" ,src "', '" ,document "', '" ,new-name "')")))))
;;(rename-document "adasd/mdo.xml" "kkka.xml")
					  
(defmacro execute-query (source query)
  `(with-drakma-http-request ,source :get :parameters 
                             (make-request-parameters :query ,query)))


;; tests

(defun test-interface-functions (tests)
  (loop for test in tests
     if (equal (eval (first test)) (second test))
     do (format t "~A *PASSED*~%" (caar test))
     else do (format t "~A #FAILED#~%" (caar test))))


(make-config :address "localhost" :port "8080" :username "admin" :password "admin")
(test-interface-functions '(((create-collection "mycol2" "testcol") t)
                            ((delete-from-db "mycol2/testcol") t)))

;; (drakma:http-request "http://localhost:8080/exist/rest/db"
;;                          :method :get
;;                          :basic-authorization '("admin" "admin")
;;                          :parameters '(("_query" . "xmldb:rename('/db/rararar', 'adasd')")))
						      
						      
						      
;; ;; want-stream -- makes request return a stream (xml file) which can be proceeded using my own parser or one of the existing
;; (read-line (drakma:http-request "http://localhost:8080/exist/rest/db"
;;                          :method :get
;;                          :basic-authorization '("admin" "admin")
;;                          :parameters '(("_query" . "xmldb:collection-available('/db/mycol2')"))
;;                          :want-stream t))
;; ;; ^
;; (drakma:http-request "http://localhost:8080/exist/rest/db"
;;                          :method :get
;;                          :basic-authorization '("admin" "admin")
;;                          :parameters '(("_query" . "xmldb:get-permissions('/db/mycol2/testcol22')")))
