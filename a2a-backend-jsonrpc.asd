(defsystem "a2a-backend-jsonrpc"
  :version "0.1.0"
  :description "JSON-RPC 2.0 + SSE binding for a2a-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("a2a-protocol" "rpc-protocol" "sse-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "a2a-backend-jsonrpc/tests"))))

(defsystem "a2a-backend-jsonrpc/tests"
  :depends-on ("a2a-backend-jsonrpc" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
