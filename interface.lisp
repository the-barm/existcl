(in-package #:existcl)

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

(define-driver-func get-document (address)
    "Get document from the database. If MIME-type of the document allows it to be converted to string - it will be converted,
otherwise the document is returned as a sequence of octets"
    (:get
     :return-text t)
  request)

(define-driver-func put-xml-document (address content)
    "Create XML document containing $content with address (including it's name) $address"
    (:put :content-type "text/xml" :content (file-to-string content))
  request)

(define-driver-func put-document (address content)
    "Create plain-text document containing $content with address (including it's name) $address"
    (:put :content-type "text/plain" :content (file-to-string content))
  request)

(define-driver-func put-document-from-string (address content-type content)
    "Create document containing $content with address (including it's name) $address and MIME-type specified by $content-type"
    (:put :content-type content-type :content content)
  request)

(define-driver-func delete-from-db (address)
    "Removes specified recourse or collection (with all contents) from the database."
    (:delete)
  request)

(define-driver-func execute-query (source query &key return-text)
    "Runs query specified by $query string on $source address (accepts empty string) \n
keys parameters:
:return-text - if specified query returns it's output as XML-string, otherwise it returns t/nil depending on whether operation was successful"
    (:get
     :parameters (make-request-parameters :query query)
     :return-text return-text)
  request)

(define-driver-func create-collection (address name)
    "Create a new collection with name $name as a child of $address collection."
    (:get
     :parameters
     (make-request-parameters
      :query
      (concatenate 'string "xmldb:create-collection('/db/" address "', '" name "')")))
  request)

(define-driver-func move-collection (source direction)
    "Moves the $source collection to the $direction collection"
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:move('/db/" source "', '/db/" direction "')")))
  request)

(define-driver-func move-document (source direction)
    "Moves the $source resource to the $direction collection"
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path source)
                    (concatenate 'string "xmldb:move('/db/" src "', '/db/" direction "', '" document "')"))))
  request)

(define-driver-func copy-collection (source direction)
    "Copies the $source collection to the $direction collection"
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:copy('/db/" source "', '/db/" direction "')")))
  request)

(define-driver-func copy-document (source direction)
    "Copies the $source resource to the $direction collection"
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path source)
                    (concatenate 'string "xmldb:copy('/db/" src "', '/db/" direction "', '" document "')"))))
  request)

(define-driver-func rename-collection (source new-name)
    "Renames the collection $source with new name $new-name."
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:rename('/db/" source "', '" new-name "')")))
  request)

(define-driver-func rename-document (source new-name)
    "Renames the document $source with new name $new-name."
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path source)
                    (concatenate 'string "xmldb:rename('/db/" src "', '" document "', '" new-name "')"))))
  request)

(define-driver-func get-collection-permissions (address)
    "Returns the permissions assigned to the collection at $address "
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:get-permissions('" address "')"))
     :return-text t) 
  (xmls:xmlrep-integer-child
   (parse-resulting-xml
    request)))

(define-driver-func get-document-permissions (address)
    "Returns the permissions assigned to the document at $address "
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path address)
                    (concatenate 'string "xmldb:get-permissions('" src "', '" document "')")))
     :return-text t)
  (xmls:xmlrep-integer-child
   (parse-resulting-xml
    request)))

(define-driver-func collection-available (address)
    "Returns t if the collection at $address exists and is available, otherwise nil"
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:collection-available('" address "')"))
     :return-text t)
  (translate-boolean
   (xmls:xmlrep-string-child
    (parse-resulting-xml
     request))))

(define-driver-func collection-created (address)
  "Returns the creation date of the collection at $address."
   (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:created('" address "')"))
     :return-text t) 
  (xmls:xmlrep-string-child
    (parse-resulting-xml
     request)))

(define-driver-func document-created (address)
    "Returns the creation date of the document at $address. "
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path address)
                    (concatenate 'string "xmldb:created('" src "', '" document "')")))
     :return-text t)
  (xmls:xmlrep-string-child
   (parse-resulting-xml
    request)))

(define-driver-func document-has-lock (address)
    "Returns the user-id of the user that holds a write lock on the resource at $address."
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path address)
                    (concatenate 'string "xmldb:document-has-lock('" src "', '" document "')")))
     :return-text t)
  (awhen (parse-resulting-xml
          request
          :can-be-null t)
    (xmls:xmlrep-string-child it)))

(define-driver-func clear-lock (address)
    "Clears lock on the document specified by $address and returns the user-id of the user that used to hold it."
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path address)
                    (concatenate 'string "xmldb:clear-lock('" src "', '" document "')")))
     :return-text t)
  (awhen (parse-resulting-xml request :can-be-null t)
    (xmls:xmlrep-string-child it)))

(define-driver-func get-child-collections (address)
    "Returns the names of the child collections in the collection at $address. "
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:get-child-collections('" address "')"))
     :return-text t)
  (mapcar #'xmls:xmlrep-string-child
          (parse-resulting-xml request :multiple t)))

(define-driver-func get-child-resources (address)
    "Returns the names of the child resources in collection at $address. "
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:get-child-resources('" address "')"))
     :return-text t)
  (mapcar #'xmls:xmlrep-string-child
          (parse-resulting-xml request :multiple t)))

(define-driver-func last-modified (address)
    "Returns the last-modification date of resource at $address."
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path address)
                    (concatenate 'string "xmldb:last-modified('" src "', '" document "')")))
     :return-text t)
  (xmls:xmlrep-string-child
   (parse-resulting-xml
    request)))

(define-driver-func size (address)
    "Returns the estimated size of the resource at $address (in bytes). 
The estimation is based on the number of pages occupied by the resource. If the document is serialized back to a string, 
its size may be different, since parts of the structural information are stored in compressed form. "
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path address)
                    (concatenate 'string "xmldb:size('" src "', '" document "')")))
     :return-text t)
  (xmls:xmlrep-integer-child
   (parse-resulting-xml
    request)))

(define-driver-func reindex (address)
    "Reindex collection at $address. The XQuery owner must have appropriate privileges to do this, e.g. having DBA role."
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:reindex('" address "')"))
     :return-text t)
  (translate-boolean
   (xmls:xmlrep-string-child
    (parse-resulting-xml
     request))))

(define-driver-func get-document-owner (address)
    "Returns the owner user-id of the document at $address."
    (:get
     :parameters (make-request-parameters
                  :query
                  (multiple-value-bind (src document)
                      (divide-path address)
                    (concatenate 'string "xmldb:get-owner('" src "', '" document "')")))
     :return-text t)
  (xmls:xmlrep-string-child
   (parse-resulting-xml
    request)))

(define-driver-func get-collection-owner (address)
    "Returns the owner user-id of the collection at $address."
    (:get
     :parameters (make-request-parameters
                  :query
                  (concatenate 'string "xmldb:get-owner('" address "')"))
     :return-text t)
  (xmls:xmlrep-string-child
   (parse-resulting-xml
    request)))

