(defpackage #:a2a-backend-jsonrpc
  (:use #:cl)
  (:import-from #:rpc-backend-http
                #:transport-url
                #:transport-headers
                #:transport-next-id)
  (:export #:jsonrpc-a2a-backend
           #:make-jsonrpc-a2a-backend
           #:use-jsonrpc-a2a-backend
           #:make-a2a-app
           #:jsonrpc-a2a-transport
           #:make-jsonrpc-a2a-transport
           #:backend-url
           #:backend-transport
           #:well-known-card-path-p))

(in-package #:a2a-backend-jsonrpc)
