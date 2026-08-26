(in-package #:a2a-backend-jsonrpc)

(defclass jsonrpc-a2a-backend (a2a-protocol:a2a-backend)
  ((url :initarg :url :accessor backend-url :initform nil)
   (transport :initarg :transport :accessor backend-transport :initform nil)
   (card :initarg :card :accessor backend-card :initform nil)
   (protocol-version :initarg :protocol-version
                     :accessor backend-protocol-version
                     :initform a2a-protocol:+a2a-protocol-version+)))

(defun make-jsonrpc-a2a-backend (&key url transport card
                                   (protocol-version a2a-protocol:+a2a-protocol-version+))
  (make-instance 'jsonrpc-a2a-backend
                 :url url
                 :transport transport
                 :card card
                 :protocol-version protocol-version))

(defun use-jsonrpc-a2a-backend (&rest args &key &allow-other-keys)
  (setf a2a-protocol:*a2a-backend* (apply #'make-jsonrpc-a2a-backend args)))

(defclass jsonrpc-a2a-transport (rpc-backend-http:http-rpc-transport)
  ((protocol-version :initarg :protocol-version
                     :accessor transport-protocol-version
                     :initform a2a-protocol:+a2a-protocol-version+))
  (:documentation "http-rpc-transport plus A2A-Version / SSE accept."))

(defun %a2a-transport-headers (protocol-version)
  `(("A2A-Version" . ,protocol-version)
    ("accept" . "application/json, text/event-stream")))

(defun make-jsonrpc-a2a-transport
    (&key url (protocol-version a2a-protocol:+a2a-protocol-version+))
  (make-instance 'jsonrpc-a2a-transport
                 :url url
                 :protocol-version protocol-version
                 :headers (%a2a-transport-headers protocol-version)))

(defun %transport (backend)
  (or (backend-transport backend)
      (when (backend-url backend)
        (setf (backend-transport backend)
              (make-jsonrpc-a2a-transport
               :url (backend-url backend)
               :protocol-version (backend-protocol-version backend))))
      rpc-protocol:*rpc-transport*
      (a2a-protocol:signal-a2a-error
       :message "jsonrpc backend has no transport or :url")))

(defun %header (env name)
  (let ((headers (getf env :headers)))
    (cond
      ((hash-table-p headers)
       (or (gethash name headers)
           (gethash (string-downcase name) headers)))
      ((listp headers)
       (cdr (assoc name headers :test #'string-equal)))
      (t nil))))

(defun well-known-card-path-p (path)
  (member path '("/.well-known/agent-card.json" "/.well-known/agent.json")
          :test #'string=))

(defun %octets-to-string (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun %body-string (response)
  (let ((b (http-protocol:response-body response)))
    (cond
      ((stringp b) b)
      ((and (vectorp b) (not (stringp b)))
       (%octets-to-string b))
      (t ""))))

(defun %raise-rpc (msg)
  (let ((err (gethash "error" msg)))
    (if err
        (error 'rpc-protocol:rpc-error
               :code (or (gethash "code" err) rpc-protocol:+internal-error+)
               :message (gethash "message" err)
               :data (gethash "data" err))
        (gethash "result" msg))))

(defun %slurp-stream (stream)
  (if (and (open-stream-p stream)
           (ignore-errors
             (let ((et (stream-element-type stream)))
               (and et (subtypep et 'character)))))
      (with-output-to-string (out)
        (loop for c = (read-char stream nil :eof)
              until (eq c :eof)
              do (write-char c out)))
      (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
        (loop for b = (read-byte stream nil :eof)
              until (eq b :eof)
              do (vector-push-extend b bytes))
        (%octets-to-string bytes))))

(defun slurp-env-body (env)
  (let ((raw (getf env :raw-body)))
    (cond
      ((null raw) "")
      ((stringp raw) raw)
      ((and (vectorp raw) (not (stringp raw)))
       (%octets-to-string raw))
      ((streamp raw) (%slurp-stream raw))
      (t (princ-to-string raw)))))

(defun %decode-body (body content-type)
  (if (and (stringp content-type)
           (search "text/event-stream" content-type :test #'char-equal))
      (mapcar (lambda (ev)
                (%raise-rpc (rpc-protocol:decode-message
                             (sse-protocol:sse-event-data ev))))
              (with-input-from-string (s body)
                (sse-protocol:collect-sse-events s)))
      (%raise-rpc (rpc-protocol:decode-message body))))

(defun %a2a-headers (transport)
  ;; transport-headers already has A2A-Version; do not append a second copy.
  ;; Node joins duplicates into "1.0, 1.0" and rejects the version (-32009).
  (let ((extras (remove "A2A-Version" (rpc-backend-http:transport-headers transport)
                        :key #'car :test #'string-equal)))
    (append extras
            `(("content-type" . "application/json")
              ("accept" . "application/json, text/event-stream")
              ("A2A-Version" . ,(transport-protocol-version transport))))))

(defun %ensure-url (transport)
  (or (transport-url transport)
      (error 'rpc-protocol:rpc-error
             :message "jsonrpc A2A transport has no :url"
             :code rpc-protocol:+internal-error+)))

(defun %wrap-a2a-error (fn)
  (handler-case
      (funcall fn)
    (rpc-protocol:rpc-error (c)
      (a2a-protocol:signal-a2a-error
       :message (rpc-protocol:rpc-error-message c)
       :code (rpc-protocol:rpc-error-code c)
       :data (rpc-protocol:rpc-error-data c)))))

(defun %rpc (backend method params)
  (%wrap-a2a-error
   (lambda ()
     (rpc-protocol:rpc-call method params :transport (%transport backend)))))

(defun %card-response (card)
  (list 200
        '(:content-type "application/json; charset=utf-8")
        (list (a2a-protocol:encode-json
               (a2a-protocol:encode-agent-card card)))))

(defun %sse-rpc-events (id events)
  (apply #'concatenate 'string
         (mapcar (lambda (ev)
                   (sse-protocol:encode-sse-event
                    (sse-protocol:make-sse-event
                     :data (rpc-protocol:encode-response ev :id id))))
                 events)))

(defun %dispatch (agent method params &key protocol-version)
  (handler-case
      (a2a-protocol:dispatch-a2a-method agent method params
                                        :protocol-version protocol-version)
    (a2a-protocol:a2a-error (c)
      (error 'rpc-protocol:rpc-error
             :message (or (a2a-protocol:a2a-error-message c) "a2a error")
             :code (or (a2a-protocol:a2a-error-code c)
                       rpc-protocol:+internal-error+)
             :data (a2a-protocol:a2a-error-data c)))))

(defun make-a2a-app (agent &key path card)
  "Clack app: GET well-known Agent Card; POST JSON-RPC.
   SendStreamingMessage / SubscribeToTask → SSE of JSON-RPC results."
  (lambda (env)
    (block app
      (let ((path-info (or (getf env :path-info) "/"))
            (method (getf env :request-method)))
        (when (and (eq method :get) (well-known-card-path-p path-info))
          (return-from app
            (%card-response (or card (a2a-protocol:a2a-agent-card agent)))))
        (when (and path (not (string= path-info path))
                   (not (well-known-card-path-p path-info)))
          (return-from app
            '(404 (:content-type "text/plain") ("not found"))))
        (unless (eq method :post)
          (return-from app
            '(405 (:content-type "text/plain" :allow "GET, POST") ("GET or POST"))))
        (let* ((body (slurp-env-body env))
               (msg (rpc-protocol:decode-message body))
               (rpc-method (gethash "method" msg))
               (params (gethash "params" msg))
               (id (gethash "id" msg))
               (ver (or (%header env "a2a-version")
                        (%header env "A2A-Version"))))
          (unless rpc-method
            (return-from app
              (list 200 '(:content-type "application/json; charset=utf-8")
                    (list (rpc-protocol:encode-error-response
                           rpc-protocol:+invalid-request+ "missing method"
                           :id id)))))
          (handler-case
              (let ((result (%dispatch agent rpc-method params
                                       :protocol-version ver)))
                (if (typep result 'a2a-protocol:a2a-stream-result)
                    (list 200
                          '(:content-type "text/event-stream; charset=utf-8"
                            :cache-control "no-cache")
                          (list (%sse-rpc-events
                                 (or id 1)
                                 (a2a-protocol:a2a-stream-events result))))
                    (list 200
                          '(:content-type "application/json; charset=utf-8")
                          (list (if id
                                    (rpc-protocol:encode-response result :id id)
                                    "")))))
            (rpc-protocol:rpc-error (c)
              (list 200
                    '(:content-type "application/json; charset=utf-8")
                    (list (rpc-protocol:encode-error-response
                           (rpc-protocol:rpc-error-code c)
                           (or (rpc-protocol:rpc-error-message c) "rpc error")
                           :id id :data (rpc-protocol:rpc-error-data c)))))))))))

(defun %ensure-http ()
  (unless http-protocol:*http-backend*
    (a2a-protocol:signal-a2a-error
     :message "*http-backend* is nil — bind an http-protocol backend")))

(defun %card-url (url)
  (cond
    ((search "/.well-known/" url) url)
    (t (format nil "~a/.well-known/agent-card.json"
               (string-right-trim "/" url)))))

(defmethod a2a-protocol:fetch-agent-card ((backend jsonrpc-a2a-backend) url &key)
  (%ensure-http)
  (let* ((card-url (%card-url url))
         (res (http:get card-url
                        :headers `(("accept" . "application/json")
                                   ("A2A-Version" . ,(backend-protocol-version backend)))))
         (status (http-protocol:response-status res))
         (text (%body-string res)))
    (unless (<= 200 status 299)
      (a2a-protocol:signal-a2a-error
       :message (format nil "HTTP ~a fetching agent card" status)))
    (let ((card (a2a-protocol:decode-agent-card (a2a-protocol:decode-json text))))
      (setf (backend-card backend) card)
      card)))

(defmethod a2a-protocol:serve-agent-card ((backend jsonrpc-a2a-backend) card &key)
  (setf (backend-card backend) card))

(defmethod a2a-protocol:send-message ((backend jsonrpc-a2a-backend) message
                                      &key task-id (blocking t))
  (when task-id
    (setf (a2a-protocol:a2a-message-task-id message) task-id))
  (a2a-protocol:decode-send-result
   (%rpc backend "SendMessage"
         (a2a-protocol:json-object
          "message" (a2a-protocol:encode-message message)
          "configuration" (if blocking
                              :omit
                              (a2a-protocol:json-object "returnImmediately" t))))))

(defmethod a2a-protocol:stream-message ((backend jsonrpc-a2a-backend) message
                                        &key on-event)
  (let ((result (%rpc backend "SendStreamingMessage"
                      (a2a-protocol:json-object
                       "message" (a2a-protocol:encode-message message)))))
    (let ((events (cond
                    ((typep result 'a2a-protocol:a2a-stream-result)
                     (a2a-protocol:a2a-stream-events result))
                    ((listp result) result)
                    (t (list result)))))
      (when on-event
        (mapc on-event events))
      (a2a-protocol:make-a2a-stream-result events))))

(defmethod a2a-protocol:get-task ((backend jsonrpc-a2a-backend) task-id
                                  &key history-length)
  (a2a-protocol:decode-task
   (%rpc backend "GetTask"
         (a2a-protocol:json-object
          "id" task-id
          "historyLength" (or history-length :omit)))))

(defmethod a2a-protocol:list-tasks ((backend jsonrpc-a2a-backend)
                                    &key context-id status page-size page-token
                                      history-length include-artifacts
                                      status-timestamp-after)
  (%rpc backend "ListTasks"
        (a2a-protocol:json-object
         "contextId" (or context-id :omit)
         "status" (if status (a2a-protocol:task-state-to-wire status) :omit)
         "pageSize" (or page-size :omit)
         "pageToken" (or page-token :omit)
         "historyLength" (or history-length :omit)
         "includeArtifacts" (if include-artifacts t :omit)
         "statusTimestampAfter" (or status-timestamp-after :omit))))

(defmethod a2a-protocol:cancel-task ((backend jsonrpc-a2a-backend) task-id &key)
  (a2a-protocol:decode-task
   (%rpc backend "CancelTask" (a2a-protocol:json-object "id" task-id))))

(defmethod a2a-protocol:resubscribe-task ((backend jsonrpc-a2a-backend) task-id
                                          &key on-event)
  (let ((result (%rpc backend "SubscribeToTask"
                      (a2a-protocol:json-object "id" task-id))))
    (let ((events (cond
                    ((typep result 'a2a-protocol:a2a-stream-result)
                     (a2a-protocol:a2a-stream-events result))
                    ((listp result) result)
                    (t (list result)))))
      (when on-event
        (mapc on-event events))
      (a2a-protocol:make-a2a-stream-result events))))

(defun %post-rpc (transport method params &key timeout id notify)
  (%ensure-http)
  (let* (         (url (%ensure-url transport))
         (id (or id (incf (transport-next-id transport))))
         (body (if notify
                   (rpc-protocol:encode-notification method params)
                   (rpc-protocol:encode-request method params :id id)))
         (res (apply #'http:post url
                     :content body
                     :headers (%a2a-headers transport)
                     (when timeout (list :timeout timeout))))
         (status (http-protocol:response-status res))
         (ctype (http-protocol:response-header res "content-type"))
         (text (%body-string res)))
    (cond
      ((<= 200 status 299)
       (if notify
           t
           (%decode-body text ctype)))
      (t
       (let ((msg (ignore-errors (rpc-protocol:decode-message text))))
         (if (and msg (hash-table-p msg) (gethash "error" msg))
             (%raise-rpc msg)
             (error 'rpc-protocol:rpc-error
                    :message (format nil "HTTP ~a~@[ ~a~]" status
                                     (and (plusp (length text)) text))
                    :code rpc-protocol:+internal-error+)))))))

(defmethod rpc-protocol:backend-rpc-call
    ((transport jsonrpc-a2a-transport) method params &key timeout id)
  (%post-rpc transport method params :timeout timeout :id id))

(defmethod rpc-protocol:backend-rpc-notify
    ((transport jsonrpc-a2a-transport) method params)
  (%post-rpc transport method params :notify t)
  t)

(defmethod rpc-protocol:backend-rpc-serve
    ((transport jsonrpc-a2a-transport) handler &key)
  (declare (ignore handler))
  (error 'rpc-protocol:rpc-error
         :message "use make-a2a-app, not rpc-serve on this transport"
         :code rpc-protocol:+internal-error+))

(use-jsonrpc-a2a-backend)
