(in-package #:a2a-backend-jsonrpc/tests)

(deftest backend-class
  (ok (typep (a2a-backend-jsonrpc:make-jsonrpc-a2a-backend) 'a2a-backend-jsonrpc:jsonrpc-a2a-backend)))
