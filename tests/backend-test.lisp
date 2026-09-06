(in-package #:a2a-backend-jsonrpc/tests)

(defun %agent ()
  (a2a-protocol:make-a2a-agent :name "echo"))

(defun %send-body (&optional (text "hi") (id 1))
  (rpc-protocol:encode-request
   "SendMessage"
   (a2a-protocol:json-object
    "message" (a2a-protocol:encode-message
               (a2a-protocol:make-a2a-message :text text)))
   :id id))

(deftest backend-class
  (ok (typep (a2a-backend-jsonrpc:make-jsonrpc-a2a-backend)
             'a2a-backend-jsonrpc:jsonrpc-a2a-backend)))

(deftest transport-is-http-rpc
  (ok (typep (a2a-backend-jsonrpc:make-jsonrpc-a2a-transport :url "http://127.0.0.1/")
             'rpc-backend-http:http-rpc-transport)))

(deftest a2a-version-header-once
  (let* ((tx (a2a-backend-jsonrpc:make-jsonrpc-a2a-transport :url "http://127.0.0.1/"))
         (hs (a2a-backend-jsonrpc::%a2a-headers tx))
         (vers (remove "A2A-Version" hs :key #'car :test-not #'string-equal)))
    (ok (= 1 (length vers)))
    (ok (equal a2a-protocol:+a2a-protocol-version+ (cdr (first vers))))))

(deftest well-known-paths
  (ok (a2a-backend-jsonrpc:well-known-card-path-p "/.well-known/agent-card.json"))
  (ok (a2a-backend-jsonrpc:well-known-card-path-p "/.well-known/agent.json"))
  (ok (null (a2a-backend-jsonrpc:well-known-card-path-p "/rpc"))))

(deftest clack-agent-card
  (let* ((agent (%agent))
         (app (a2a-backend-jsonrpc:make-a2a-app agent)))
    (dolist (path '("/.well-known/agent-card.json" "/.well-known/agent.json"))
      (let ((res (funcall app (list :request-method :get :path-info path))))
        (ok (eql 200 (first res)))
        (ok (search "application/json" (getf (second res) :content-type)))
        (let ((card (a2a-protocol:decode-json (first (third res)))))
          (ok (equal "echo" (gethash "name" card)))
          (ok (null (gethash "protocolVersion" card)))
          (ok (equal "1.0"
                     (gethash "protocolVersion"
                              (elt (gethash "supportedInterfaces" card) 0)))))))))

(deftest clack-send-message
  (let* ((agent (%agent))
         (app (a2a-backend-jsonrpc:make-a2a-app agent))
         (res (funcall app (list :request-method :post
                                 :path-info "/"
                                 :raw-body (%send-body "pong")
                                 :headers (let ((h (make-hash-table :test 'equal)))
                                            (setf (gethash "a2a-version" h) "1.0")
                                            h)))))
    (ok (eql 200 (first res)))
    (let* ((msg (rpc-protocol:decode-message (first (third res))))
           (task (a2a-protocol:decode-send-result (gethash "result" msg))))
      (ok (eq :completed (a2a-protocol:a2a-task-state task)))
      (ok (equal "pong"
                 (a2a-protocol:a2a-part-text
                  (first (a2a-protocol:a2a-artifact-parts
                          (first (a2a-protocol:a2a-task-artifacts task))))))))))

(deftest clack-stream-message
  (let* ((agent (%agent))
         (app (a2a-backend-jsonrpc:make-a2a-app agent))
         (res (funcall app
                       (list :request-method :post
                             :path-info "/"
                             :raw-body (rpc-protocol:encode-request
                                        "SendStreamingMessage"
                                        (a2a-protocol:json-object
                                         "message" (a2a-protocol:encode-message
                                                    (a2a-protocol:make-a2a-message
                                                     :text "stream")))
                                        :id 7)))))
    (ok (eql 200 (first res)))
    (ok (search "text/event-stream" (getf (second res) :content-type)))
    (let ((events (with-input-from-string (s (first (third res)))
                    (sse-protocol:collect-sse-events s))))
      (ok (= 3 (length events)))
      (let ((first-msg (rpc-protocol:decode-message
                        (sse-protocol:sse-event-data (first events)))))
        (ok (eql 7 (gethash "id" first-msg)))
        (ok (gethash "task" (gethash "result" first-msg)))))))

(deftest clack-get-405-on-rpc-path
  (let* ((app (a2a-backend-jsonrpc:make-a2a-app (%agent)))
         (res (funcall app (list :request-method :get :path-info "/"))))
    (ok (eql 405 (first res)))))

(deftest inprocess-backend-gfs
  (let* ((agent (%agent))
         (transport (rpc-backend-inprocess:make-inprocess-rpc-transport))
         (backend (a2a-backend-jsonrpc:make-jsonrpc-a2a-backend :transport transport)))
    (a2a-protocol:serve-a2a agent :transport transport)
    (let ((task (a2a-protocol:send-message
                 backend (a2a-protocol:make-a2a-message :text "via-gf"))))
      (ok (eq :completed (a2a-protocol:a2a-task-state task)))
      (ok (equal "via-gf"
                 (a2a-protocol:a2a-part-text
                  (first (a2a-protocol:a2a-artifact-parts
                          (first (a2a-protocol:a2a-task-artifacts task)))))))
      (let ((got (a2a-protocol:get-task backend (a2a-protocol:a2a-task-id task))))
        (ok (equal (a2a-protocol:a2a-task-id task)
                   (a2a-protocol:a2a-task-id got)))))))

(deftest version-header-rejected
  (let* ((app (a2a-backend-jsonrpc:make-a2a-app (%agent)))
         (headers (let ((h (make-hash-table :test 'equal)))
                    (setf (gethash "a2a-version" h) "9.9")
                    h))
         (res (funcall app (list :request-method :post
                                 :path-info "/"
                                 :raw-body (%send-body)
                                 :headers headers)))
         (msg (rpc-protocol:decode-message (first (third res)))))
    (ok (gethash "error" msg))
    (ok (eql a2a-protocol:+a2a-error-version-not-supported+
             (gethash "code" (gethash "error" msg))))))

(deftest wrap-rpc-task-not-found
  (ok (signals
       (a2a-backend-jsonrpc::%wrap-a2a-error
        (lambda ()
          (error 'rpc-protocol:rpc-error
                 :code a2a-protocol:+a2a-error-task-not-found+
                 :message "missing")))
       'a2a-protocol:a2a-task-not-found)))

(deftest missing-task-via-backend-is-typed
  (let* ((agent (%agent))
         (transport (rpc-backend-inprocess:make-inprocess-rpc-transport))
         (backend (a2a-backend-jsonrpc:make-jsonrpc-a2a-backend :transport transport)))
    (a2a-protocol:serve-a2a agent :transport transport)
    (ok (signals (a2a-protocol:get-task backend "missing")
                 'a2a-protocol:a2a-task-not-found))))

(deftest clack-push-refused
  (let* ((app (a2a-backend-jsonrpc:make-a2a-app (%agent)))
         (res (funcall app
                       (list :request-method :post
                             :path-info "/"
                             :raw-body (rpc-protocol:encode-request
                                        "CreateTaskPushNotificationConfig"
                                        (a2a-protocol:json-object)
                                        :id 1))))
         (msg (rpc-protocol:decode-message (first (third res)))))
    (ok (gethash "error" msg))
    (ok (eql a2a-protocol:+a2a-error-push-not-supported+
             (gethash "code" (gethash "error" msg))))))

(defun %free-port ()
  (let* ((sock (usocket:socket-listen "127.0.0.1" 0 :reuseaddress t))
         (port (usocket:get-local-port sock)))
    (usocket:socket-close sock)
    port))

(defun %bind-async-libuv ()
  (handler-case
      (progn
        (asdf:load-system "event-backend-libuv")
        (let* ((maker (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv))
               (eb (funcall maker))
               (el (event-protocol:make-event-loop eb)))
          (setf http-backend-async:*event-backend-maker* (lambda () eb)
                event-protocol:*event-backend* eb
                event-protocol:*event-loop* el
                http-protocol:*http-backend* (http-backend-async:make-async-backend))
          t))
    (error () nil)))

(defmacro with-live-http (&body body)
  `(progn
     (http-server-backend-hunchentoot:use-hunchentoot-backend)
     (if (%bind-async-libuv)
         (event-protocol:with-event-backend (event-protocol:*event-backend*)
           (event-protocol:with-event-loop-var (event-protocol:*event-loop*)
             ,@body))
         (let ((http-protocol:*http-backend*
                 (http-backend-dexador:make-dexador-backend)))
           ,@body))))

(deftest live-http-stream-message
  (with-live-http
    (let ((port (%free-port)))
      (http-server-protocol:with-server
          (s (a2a-backend-jsonrpc:make-a2a-app (%agent))
             :host "127.0.0.1" :port port)
        (sleep 0.2)
        (let* ((backend (a2a-backend-jsonrpc:make-jsonrpc-a2a-backend
                         :url (format nil "http://127.0.0.1:~a/" port)))
               (result (a2a-protocol:stream-message
                        backend (a2a-protocol:make-a2a-message :text "stream")))
               (events (a2a-protocol:a2a-stream-events result)))
          (ok (= 3 (length events)))
          (ok (gethash "task" (first events)))
          (ok (gethash "artifactUpdate" (second events)))
          (ok (gethash "statusUpdate" (third events))))))))

(deftest inprocess-stream-message
  (let* ((agent (%agent))
         (transport (rpc-backend-inprocess:make-inprocess-rpc-transport))
         (backend (a2a-backend-jsonrpc:make-jsonrpc-a2a-backend :transport transport)))
    (a2a-protocol:serve-a2a agent :transport transport)
    (let ((result (a2a-protocol:stream-message
                   backend (a2a-protocol:make-a2a-message :text "via-gf"))))
      (ok (typep result 'a2a-protocol:a2a-stream-result))
      (ok (= 3 (length (a2a-protocol:a2a-stream-events result)))))))
