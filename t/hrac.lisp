(in-package :cl-hrac.test)

(ia-hash-table:enable-ia-syntax)

(plan 5)

(subtest "GET /api/overview"
  (let* ((r (hrac:overview))
         (ts (map 'list (lambda (h) #Ih.name) #Ir.exchange_types)))
    (loop for type in '("direct" "fanout" "headers" "topic") do
             (ok (find type ts :test #'equal)))))

(subtest "enabled-protocols"
  (let ((r (hrac:enabled-protocols)))
    (ok (find "amqp" r :test #'equal))))

(subtest "protocol-ports"
  (let ((r (hrac:protocol-ports)))
    (is #Ir.amqp 5672)))

(subtest "GET /api/nodes"
  (let* ((nodes (hrac:nodes))
         (first (elt nodes 0))
         (rabbit (find-if (lambda (app)
                            (equal #Iapp.name "rabbit"))
                          #Ifirst.applications)))

    (is #Irabbit.description "RabbitMQ")
    (ok (alexandria:starts-with-subseq "rabbit" #Ifirst.name))))

(subtest "GET /api/nodes/:name"
  (let* ((nodes (hrac:nodes))
         (n (hrac:node-info #Inodes.[0].name))
         (rabbit (find-if (lambda (app)
                            (equal #Iapp.name "rabbit"))
                          #In.applications)))

    (is #Irabbit.description "RabbitMQ")
    (ok (alexandria:starts-with-subseq "rabbit" #In.name))))

(subtest "GET /api/extensions"
  (is-type (hrac:extensions) 'sequence))

(subtest "GET /api/definitions"
  (let ((defs (hrac:definitions)))

    (is-type #Idefs.bindings 'sequence)
    (is-type #Idefs.queues 'sequence)
    (is-type #Idefs.exchanges 'sequence)
    (is-type #Idefs.users 'sequence)
    (is-type #Idefs.vhosts 'sequence)))

(subtest "POST /api/definitions"
  (prove:skip 1 "Not sure how to restore state reliably after this test")) ;; TODO: Not sure how to restore state reliably after this test

(subtest "GET /api/connections"
  (let ((old-count (length (hrac:connections))))
    (bunny:with-connection ()
      (is (length (hrac:connections)) (1+ old-count)))))

;; (subtest "GET /api/connections/:name"
;;   (let* ((connections (hrac:connections))
;;          (cname #Iconnections.[0].name)
;;          (cl-bunny (find-if (lambda (c)
;;                                (equal #Ic.client_properties.product "CL-BUNNY"))
;;                             connections)))
;;     (ok (search "127.0.0.1" cname))
;;     (ok cl-bunny)))

(subtest "DELETE /api/connections/:name"
  (bunny:with-connection ()
    (let* ((connections (hrac:connections))
           (cname #Iconnections.[0].name))
      (hrac:connection.close cname)
      (sleep 1)
      (is-error (bunny:channel.new.open) 'bunny:connection-closed-error))))

(subtest "GET /api/channels"
  (let ((old-count (length (hrac:channels))))
    (bunny:with-connection ()
      (bunny:with-channel ()
        (is (length (hrac:channels)) (1+ old-count))))))

(subtest "GET /api/channels/:name"
  (bunny:with-connection ()
    (bunny:with-channel ()
      (let* ((channels (hrac:channels))
             (cname #Ichannels.[0].name)
             (c (hrac:channel-info cname)))
        (ok (>= #Ic.number 1))
        (ok (>= #Ic.prefetch_count 0))))))

(subtest "GET /api/exchanges"
  (let* ((exchanges (hrac:exchanges))
         (e #Iexchanges.[0]))
    (ok #Ie.type)
    (ok #Ie.name)
    (ok #Ie.vhost)
    (ok #Ie.durable)
    (ok #Ie.auto_delete)))

(subtest "GET /api/exchanges/:vhost"
  (let* ((exchanges (hrac:exchanges))
         (first #Iexchanges.[0]))
    (is #Ifirst.vhost "/")))

(subtest "GET /api/exchanges/:vhost/:name"
  (let ((e (hrac:exchange-info "amq.fanout")))
    (is #Ie.type "fanout")))

(subtest "PUT /api/exchanges/:vhost/:name"
  (bunny:with-connection ()
    (bunny:with-channel ()
      (hrac::exchange.declare "httpdeclared" '(("durable" . nil) ("type" . "fanout")))
      (let ((x (bunny:exchange.declare "httpdeclared" :type "fanout")))
	(ok (bunny:exchange.delete x))))))

(subtest "DELETE /api/exchanges/:vhost/:name"
  (subtest "Deletes an exchange"
    (bunny:with-connection ()
      (bunny:with-channel ()
	(let ((x (bunny:exchange.fanout "httpdeclared")))
	  (hrac:exchange.delete (bunny:exchange-name x))
	  (is-error (bunny:exchange.fanout "httpdeclared" :passive t) 'amqp:amqp-error-not-found)))))

  (subtest "Doesn't delete used exchange"
    (bunny:with-connection ()
      (bunny:with-channel ()
	(let* ((x (bunny:exchange.fanout "httpdeclared"))
	       (q (bunny:queue.declare-temp)))
	  (bunny:queue.bind q x)
	  (is (hrac:exchange.delete (bunny:exchange-name x) :if-unused t) nil)
	  (is (bunny:exchange.fanout "httpdeclared" :passive t) x)
	  (bunny:exchange.delete x))))))

(subtest "GET api/whoami"
  (let ((r (hrac:whoami)))
    (is #Ir.name "guest")))

(subtest "GET api/permissions"
  (let ((permissions (hrac:permissions)))
    (ok #Ipermissions.[0].read)))

(subtest "GET /api/parameters"
  (let ((parameters (hrac:parameters)))
    (is-type parameters 'sequence)))

(finalize)
