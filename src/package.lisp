(in-package :cl-user)

(defpackage :cl-hrac
  (:use :cl :alexandria)
  (:nicknames :hrac)
  (:export #:*connection*
           #:make-connection-spec
           #:drakma-connection
           #:overview
           #:enabled-protocols
           #:protocol-ports
           #:nodes
           #:node-info
           #:extensions
           #:definitions
           #:definitions.upload
           #:connections
           #:connection-info
           #:connection.close
           #:channels
           #:channel-info
           #:exchanges
           #:exchange-info
           #:exchange.declare
           #:exchange.delete
           #:list-bindings-by-source
           #:list-bindings-by-destination
           #:queues
           #:queue-info
           #:queue-bindings
           #:queue-messages
           #:queue-exchange-bindings
           #:queue-binding-info
           #:queue.purge
           #:queue.bind
           #:queue.unbind
           #:queue.declare
           #:queue.delete
           #:bindings
           #:vhosts
           #:vhost-info
           #:vhost.create
           #:vhost.delete
           #:permissions
           #:users
           #:user-info
           #:user-permissions
           #:user-vhost-permissions
           #:user.clear-vhost-permissions
           #:user.update
           #:user.delete
           #:whoami
           #:policies
           #:policy-info
           #:policy.update
           #:policy.clear
           #:parameters
           #:paramter-info
           #:paramter.update
           #:parameter.clear
           #:alive-p))
