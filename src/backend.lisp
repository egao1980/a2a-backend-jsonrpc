(in-package #:a2a-backend-jsonrpc)

(defclass jsonrpc-a2a-backend (a2a-protocol:a2a-backend) ())

(defun make-jsonrpc-a2a-backend ()
  (make-instance 'jsonrpc-a2a-backend))

(defun use-jsonrpc-a2a-backend ()
  (setf a2a-protocol:*a2a-backend* (make-jsonrpc-a2a-backend)))
