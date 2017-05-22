;;quicklisp should be installed
;;(load "~/quicklisp/setup.lisp")
;;(ql:add-to-init-file) permanently adds quicklisp to init-file, should be done automaticly
;;(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                         (user-homedir-pathname))))
;;    (when (probe-file quicklisp-init)
;;      (load quicklisp-init)))
;; above are strings which ql:add-to-init-file adds to ~/.sbclrc
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

(defmacro with-drakma-http-request (addr method-type &key content-type content parameters return-text)
  (with-gensyms (result connection addr-t method-type-t content-type-t content-t params-t return-text-t)
    `(let* ((,connection ,(check-connection))
            (,addr-t ,addr)
            (,method-type-t ,method-type)
            (,content-type-t ,content-type)
            (,content-t ,content)
            (,params-t ,parameters)
            (,return-text-t ,return-text)
            (,result (multiple-value-list
                      (drakma:http-request
                       (concatenate 'string
                                    "http://" (address ,connection) ":" (port ,connection) "/exist/rest/db/" ,addr-t)
                       :method ,method-type-t
                       :content-type ,content-type-t
                       :basic-authorization (list (username ,connection)
                                                  (password ,connection))
                       :content ,content-t
                       :parameters ,params-t))))
       (declare (ignorable ,method-type-t ,content-type-t ,content-t ,params-t))
       (if (or (eq (second ,result) 200)
               (eq (second ,result) 201))
           (if ,return-text-t
               (if (stringp  (first ,result))
                   (first ,result)
                   (babel:octets-to-string (first ,result)))
               t)
           (progn
             (warn (seventh ,result))
             nil)))))

(defun parse-resulting-xml (target &key can-be-null multiple)
  (if target
      (if (xmls:xmlrep-children (xmls:parse target))
          (if multiple
              (xmls:xmlrep-find-child-tags "value"
                                           (xmls:parse target))
              (xmls:xmlrep-find-child-tag "value"
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


;;INTERFACE FUNCTIONS
(defun read-db-config-file ()
  "Create new config instance with parameters specified by db.config file inside directory with compiled driver sources (*load-truename*).
File format should contain information about address, port, username and password as variable=value
on separate lines."
  (with-open-file (stream (merge-pathnames "db.config" *compile-path*)
                          :direction :input
                          :if-does-not-exist :error)
    (let ((address (prepare-parameter "address" (read-line stream)))
          (port (prepare-parameter "port" (read-line stream)))
          (username (prepare-parameter "username" (read-line stream)))
          (password (prepare-parameter "password" (read-line stream))))
      (setf *connection* (make-instance 'connection :address address :port port :username username :password password)))))

(defun update-db-config-file (&key address port username password)
  (let ((address (aif address
                   it
                   "localhost"))
        (port (aif port
                it
                "8080"))
        (username (aif username
                    it
                    "admin"))
        (password (aif password
                    it
                    "admin")))
    (with-open-file (stream (merge-pathnames "db.config" *compile-path*)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string 
	(format nil "address=~A~%port=~A~%username=~A~%password=~A~%" address port username password) stream))))

(defun db-config-file-from-current-config ()
  (let ((connection (check-connection)))
    (update-db-config-file
     :address (address connection)
     :port (port connection)
     :username (username connection)
     :password (password connection))))

(defun make-config (&key address (port "8080") username password)
  "Create new config instance from specified parameters"
  (setf *connection* (make-instance 'connection :address address :port port :username username :password password)))

(defmacro get-document (address)
  "Get document from the database. If MIME-type of the document allows it to be converted to string - it will be converted,
otherwise the document is returned as a sequence of octets"
  `(with-drakma-http-request ,address :get :return-text t))

(defmacro put-xml-document (address content)
  "Create XML document containing $content with address (including it's name) $address"
  `(with-drakma-http-request ,address :put :content-type "text/xml" :content ,(file-to-string content)))

(defmacro put-document (address content)
  "Create plain-text document containing $content with address (including it's name) $address"
  `(with-drakma-http-request ,address :put :content-type "text/plain" :content ,(file-to-string content)))

(defmacro put-document-from-string (address content-type content)
  "Create document containing $content with address (including it's name) $address and MIME-type specified by $content-type"
  `(with-drakma-http-request ,address :put :content-type ,content-type :content ,content))

(defmacro create-collection (address name)
  "Create a new collection with name $name as a child of $address collection."
  `(execute-query ,address ,`(concatenate 'string "xmldb:create-collection('/db/" ,address "', '" ,name "')")))

(defmacro delete-from-db (address)
  "Removes specified recourse or collection (with all contents) from the database."
  `(with-drakma-http-request ,address :delete))

(defmacro move-collection (source direction)
  "Moves the $source collection to the $direction collection"
  `(execute-query ,source ,`(concatenate 'string "xmldb:move('/db/" ,source "', '/db/" ,direction "')")))

(defmacro move-document (source direction)
  "Moves the $source resource to the $direction collection"
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,source)
       (execute-query ,source ,`(concatenate 'string "xmldb:move('/db/" ,src "', '/db/" ,direction "', '" ,document "')")))))

(defmacro copy-collection (source direction)
  "Copies the $source collection to the $direction collection"
  `(execute-query ,source ,`(concatenate 'string "xmldb:copy('/db/" ,source "', '/db/" ,direction "')")))

(defmacro copy-document (source direction)
  "Copies the $source resource to the $direction collection"
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,source)
       (execute-query ,source ,`(concatenate 'string "xmldb:copy('/db/" ,src "', '/db/" ,direction "', '" ,document "')")))))

(defmacro rename-collection (source new-name)
  "Renames the collection $source with new name $new-name."
  `(execute-query ,source ,`(concatenate 'string "xmldb:rename('/db/" ,source "', '" ,new-name "')")))

(defmacro rename-document (source new-name)
  "Renames the document $source with new name $new-name."
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,source)
       (execute-query ,source ,`(concatenate 'string "xmldb:rename('/db/" ,src "', '" ,document "', '" ,new-name "')")))))

(defmacro execute-query (source query &key return-text)
  "Runs query specified by $query string on $source address (accepts empty string) \n
keys parameters:
:return-text - if specified query returns it's output as XML-string, otherwise it returns t/nil depending on whether operation was successful"
  `(with-drakma-http-request ,source :get :parameters 
                             (make-request-parameters :query ,query) :return-text ,return-text))

(defmacro get-collection-permissions (address)
  "Returns the permissions assigned to the collection at $address "
  `(xmls:xmlrep-integer-child
    (parse-resulting-xml
     (execute-query ,address (concatenate 'string "xmldb:get-permissions('" ,address "')") :return-text t))))

(defmacro get-document-permissions (address)
  "Returns the permissions assigned to the document at $address "
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,address)
       (xmls:xmlrep-integer-child
        (parse-resulting-xml
         (execute-query ,address (concatenate 'string "xmldb:get-permissions('" ,src "', '" ,document "')") :return-text t))))))

(defmacro collection-available (address)
  "Returns t if the collection at $address exists and is available, otherwise nil"
  `(translate-boolean
    (xmls:xmlrep-string-child
     (parse-resulting-xml
      (execute-query ,address (concatenate 'string "xmldb:collection-available('" ,address "')") :return-text t)))))

(defmacro collection-created (address)
  "Returns the creation date of the collection at $address."
  `(xmls:xmlrep-string-child
    (parse-resulting-xml
     (execute-query ,address (concatenate 'string "xmldb:created('" ,address "')") :return-text t))))

(defmacro document-created (address)
  "Returns the creation date of the document at $address. "
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,address)
       (xmls:xmlrep-string-child
        (parse-resulting-xml
         (execute-query ,address (concatenate 'string "xmldb:created('" ,src "', '" ,document "')") :return-text t))))))

(defmacro document-has-lock (address)
  "Returns the user-id of the user that holds a write lock on the resource at $address."
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,address)
       (awhen (parse-resulting-xml
               (execute-query ,address (concatenate 'string "xmldb:document-has-lock('" ,src "', '" ,document "')") :return-text t)
               :can-be-null t)
         (xmls:xmlrep-string-child it)))))

(defmacro clear-lock (address)
  "Clears lock on the document specified by $address and returns the user-id of the user that used to hold it."
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,address)
       (awhen (parse-resulting-xml
               (execute-query ,address (concatenate 'string "xmldb:clear-lock('" ,src "', '" ,document "')") :return-text t)
               :can-be-null t)
         (xmls:xmlrep-string-child it)))))

(defmacro get-child-collections (address)
  "Returns the names of the child collections in the collection at $address. "
  `(mapcar #'xmls:xmlrep-string-child
           (parse-resulting-xml
            (execute-query ,address (concatenate 'string "xmldb:get-child-collections('" ,address "')") :return-text t)
            :multiple t)))

(defmacro get-child-resources (address)
  "Returns the names of the child resources in collection at $address. "
  `(mapcar #'xmls:xmlrep-string-child
           (parse-resulting-xml
            (execute-query ,address (concatenate 'string "xmldb:get-child-resources('" ,address "')") :return-text t)
            :multiple t)))

(defmacro last-modified (address)
  "Returns the last-modification date of resource at $address."
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,address)
       (xmls:xmlrep-string-child
        (parse-resulting-xml
         (execute-query ,address (concatenate 'string "xmldb:last-modified('" ,src "', '" ,document "')") :return-text t))))))

(defmacro size (address)
  "Returns the estimated size of the resource at $address (in bytes). 
The estimation is based on the number of pages occupied by the resource. If the document is serialized back to a string, 
its size may be different, since parts of the structural information are stored in compressed form. "
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,address)
       (xmls:xmlrep-integer-child
        (parse-resulting-xml
         (execute-query ,address (concatenate 'string "xmldb:size('" ,src "', '" ,document "')") :return-text t))))))

(defmacro reindex (address)
  "Reindex collection at $address. The XQuery owner must have appropriate privileges to do this, e.g. having DBA role."
  `(translate-boolean
    (xmls:xmlrep-string-child
     (parse-resulting-xml
      (execute-query ,address (concatenate 'string "xmldb:reindex('" ,address "')") :return-text t)))))

(defmacro get-document-owner (address)
  "Returns the owner user-id of the document at $address."
  (with-gensyms (src document)
    `(multiple-value-bind (,src ,document)
         (divide-path ,address)
       (xmls:xmlrep-string-child
        (parse-resulting-xml
         (execute-query ,address (concatenate 'string "xmldb:get-owner('" ,src "', '" ,document "')") :return-text t))))))

(defmacro get-collection-owner (address)
  "Returns the owner user-id of the collection at $address."
  `(xmls:xmlrep-string-child
    (parse-resulting-xml
     (execute-query ,address (concatenate 'string "xmldb:get-owner('" ,address "')") :return-text t))))

