# a2a-backend-jsonrpc

JSON-RPC 2.0 + SSE binding for [`a2a-protocol`](https://github.com/egao1980/a2a-protocol) (A2A **1.0**).

```lisp
(asdf:load-system "a2a-backend-jsonrpc")

(let* ((agent (a2a-protocol:make-a2a-agent :name "echo"))
       (app (a2a-backend-jsonrpc:make-a2a-app agent)))
  ;; GET  /.well-known/agent-card.json  (alias: agent.json)
  ;; POST JSON-RPC  SendMessage / GetTask / …
  ;; SendStreamingMessage / SubscribeToTask → text/event-stream
  app)
```

Client GFs (`send-message`, `get-task`, …) talk JSON-RPC over `rpc-protocol` (`:transport` or HTTP `:url` + `A2A-Version`).

Part of [cl-stack](https://github.com/egao1980/cl-stack) agent-wire. Tracks [#186](https://github.com/egao1980/cl-stack/issues/186).

CI: `setup-client` + `setup-roswell` + `scripts/ci-install.lisp` / `ci-test.lisp` (OCI only).

## License

MIT
