(in-package #:existcl)

(defun test-interface-functions (test-name tests &optional finally)
  "Small test-runner which accepts sequence of commands in format '(((command) result) ...)"
  (handler-bind ((warning #'ignore-warning))
    (unwind-protect
         (progn 
           (format t "~%RUNNING ~A~%" test-name)
           (loop for test in tests
              do (let ((result (eval (first test)))
                       (expected (eval (second test))))
                   (if (equalp result expected)
                       (format t "~A *done*~%" (caar test))
                       (format t "~A #failed# expected: ~A but got: ~A ~%" (caar test) expected result)))))
      (loop for expr in finally
         do (eval expr)))))

(defmacro define-test (name params description funcs &optional finally)
  `(defun ,name ,params
     (test-interface-functions ,description ,funcs ,finally)))

(make-config :address "localhost" :port "8080" :username "admin" :password "admin")


(defun file-test ()
  (with-open-file (test-file "file-test.xml"
                             :direction :output
                             :if-exists :error
                             :if-does-not-exist :create)
    (format test-file "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<note>
   <to>Tove</to>
   <from>Jani</from>
   <heading>Reminder</heading>
   <body>Don't forget me this weekend!</body>
</note>
"))
  (unwind-protect
       (test-interface-functions "File operations test"
                                 '(((put-xml-document "testcol/mydoc.xml" "file-test.xml") t)
                                   ((get-document "testcol/mydoc.xml")
                                    "<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget me this weekend!</body>
</note>")
                                   ((put-document "testcol/mydoc2.xml" "file-test.xml") t)
                                   ((get-document "testcol/mydoc2.xml")
                                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<note>
   <to>Tove</to>
   <from>Jani</from>
   <heading>Reminder</heading>
   <body>Don't forget me this weekend!</body>
</note>
"))
                                 '((delete-from-db "testcol")))
    (delete-file (probe-file "file-test.xml"))))


(define-test crud-test ()
  "CRUD operations test"
  '(((create-collection "" "testcol") t)
    ((create-collection "" "testcol2") t)
    ((put-document-from-string "testcol/testtest.xml" "text/plain" "<test>test</test>") t)
    ((get-document "testcol/testtest.xml") "<test>test</test>")
    ((put-document-from-string "testcol/testtest2.xml" "application/octet-stream" "<test>test</test>") t)
    ((get-document "testcol/testtest2.xml") "<test>test</test>")
    ((move-document "testcol/testtest.xml" "testcol2") t)
    ((copy-document "testcol/testtest2.xml" "testcol2") t)
    ((rename-document "testcol/testtest2.xml" "kkka.xml") t)
    ((copy-collection "testcol2" "testcol") t)
    ((rename-collection "testcol2" "123") t)
    ((move-collection "123" "testcol") t)
    ((delete-from-db "testcol") t))
  '((delete-from-db "testcol")
    (delete-from-db "testcol2")))

(define-test wrong-input ()
  "Wrong input test"
  '(((create-collection "wrongwrong" "testcol") nil)
    ((get-document "wrongwrong.xml") nil)
    ((move-document "wrongwrong.xml" "wrongwrong") nil)
    ((copy-document "wrongwrong.xml" "wrongwrong") nil)
    ((rename-document "wrongwrong.xml" "wrongwrong2.xml") nil)
    ((copy-collection "wrongwrong" "wrongwrong2") nil)
    ((rename-collection "wrongwrong" "123") nil)
    ((move-collection "123" "testcol") nil)
    ((delete-from-db "wrongwrong") nil)))

(define-test query-funcs ()
  "Query functions test"
  '(((create-collection "" "testcol") t)
    ((create-collection "testcol" "testcol2") t)
    ((get-collection-permissions "testcol") 493)
    ((put-document-from-string "testcol/testtest.xml" "text/plain" "<test>test</test>") t)
    ((get-document-permissions "testcol/testtest.xml") 420)
    ((document-has-lock "testcol/testtest.xml") nil)
    ((clear-lock "testcol/testtest.xml") nil)
    ((collection-available "testcol") t)
    ((get-child-collections "testcol") '("testcol2"))
    ((get-child-resources "testcol") '("testtest.xml"))
    ((size "testcol/testtest.xml") 17)
    ((reindex "testcol") t)
    ((get-document-owner "testcol/testtest.xml") (username *connection*))
    ((get-collection-owner "testcol/testcol2") (username *connection*))
    ((delete-from-db "testcol") t))
  '((delete-from-db "testcol"))) 
