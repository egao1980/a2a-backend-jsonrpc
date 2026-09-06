(defsystem "a2a-backend-jsonrpc"
  :version "0.2.2"
  :description "JSON-RPC 2.0 + SSE binding for a2a-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "a2a-protocol" "0.2.0")
               "rpc-protocol" "rpc-protocol-json"
               "rpc-backend-http" "sse-protocol" "http-protocol" "babel")
  :properties (:cl-repo (:ci (:with ("dissect"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "a2a-backend-jsonrpc/tests"))))

(defsystem "a2a-backend-jsonrpc/tests"
  :depends-on ("a2a-backend-jsonrpc"
               "rpc-backend-inprocess"
               "rpc-protocol-json"
               "http-server-protocol"
               "http-server-backend-hunchentoot"
               "http-backend-async"
               "event-backend-libuv"
               "event-protocol"
               "usocket"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
