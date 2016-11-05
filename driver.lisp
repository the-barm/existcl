;;quicklisp should be installed
(load "~/quicklisp/setup.lisp")
(ql:quickload :drakma)


;;(require "asdf")
;;(asdf:load-system :uiop)
;;(uiop:run-program "./exist/bin/client.sh -m /db/testcol77 -p /home/the-barm/exist/xml/shakespeare -P admin" :output *standard-output*)


;;in *most* cases get don't need authorization, but I should think about adding it permanently to any request just in case
(drakma:http-request "http://localhost:8080/exist/rest/db/shakespeare/hamlet.xml"
                         :method :get
                         :content-type "application/xml")
                   
;;if path to file is specified in :content -- binary contents of the file are sent (so I need to create some wrapper-func which will read file, mb
;;check it for validity and then send as string (mb not the bes idea for large files -- should think about something more fast

(drakma:http-request "http://localhost:8080/exist/rest/db/mycol2/mdoc.xml"
                         :method :put
                         :content-type "application/xml" :content "<test>test</test>"
                         :basic-authorization '("admin" "admin"))