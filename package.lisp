(defpackage #:existcl
  (:documentation "eXist-db driver for Common Lisp.")
  (:use #:cl :asdf)
  (:export #:read-db-config
           #:make-config
           #:get-document
           #:put-xml-document
           #:put-document
           #:put-document-from-string
           #:create-collection
           #:delete-from-db
           #:move-collection
           #:move-document
           #:copy-collection
           #:copy-document
           #:rename-collection
           #:rename-document
           #:execute-query
           #:get-collection-permissions
           #:get-document-permissions
           #:collection-available
           #:collection-created
           #:document-created
           #:document-has-lock
           #:clear-lock
           #:get-child-collections
           #:get-child-resources
           #:last-modified
           #:size
           #:reindex
           #:get-document-owner
           #:get-collection-owner))

(in-package #:existcl)
