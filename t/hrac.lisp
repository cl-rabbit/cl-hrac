(in-package :cl-hrac.test)

(ia-hash-table:enable-ia-syntax)

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
