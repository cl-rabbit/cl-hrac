(in-package :cl-hrac)

(ia-hash-table:enable-ia-syntax)
(cl-interpol:enable-interpol-syntax)

(defvar *connection* nil "CL-HRAC connection")

(defun overview (&optional (connection *connection*))
  (connection.get connection "overview"))

(defun enabled-protocols (&optional (connection *connection*))
  (let ((overview (overview connection)))
    (map 'list (lambda (listener)
                 #Ilistener.protocol)
         #Ioverview.listeners)))

(defun protocol-ports (&optional (connection *connection*))
  (let ((overview (overview connection)))
    (ia-hash-table:alist-ia-hash-table
     (map 'list (lambda (listener)
                  (cons #Ilistener.protocol #Ilistener.port))
          #Ioverview.listeners))))

(defun nodes (&optional (connection *connection*))
  (connection.get connection "nodes"))

(defun node-info (name &optional (connection *connection*))
  (connection.get connection #?"nodes/${(escape name)}"))

(defun extensions (&optional (connection *connection*))
  (connection.get connection "extensions"))

(defun definitions (&optional (connection *connection*))
  (connection.get connection "definitions"))

(defun definitions.upload (definitions &optional (connection *connection*))
  (connection.post connection "definitions" (json-encode-to-string definitions)))

(defun connections (&optional (connection *connection*))
  (connection.get connection "connections"))

(defun connection-info (name &optional (connection *connection*))
  (connection.get connection #?"connections/${(escape name)}"))

(defun connection.close (name &optional (connection *connection*))
  (connection.delete connection #?"connections/${(escape name)}"))

(defun channels (&optional (connection *connection*))
  (connection.get connection "channels"))

(defun channel-info (name &optional (connection *connection*))
  (connection.get connection #?"channels/${(escape name)}"))

(defun exchanges (&key vhost (connection *connection*))
  (let ((path (if vhost
                  #?"exchanges/${(escape vhost)}"
                  "exchanges")))
    (connection.get connection path)))

(defun exchange-info (name &key (vhost "/") (connection *connection*))
  (connection.get connection #?"exchanges/${(escape vhost)}/${(escape name)}"))

(defun exchange.declare (name attributes &key (vhost "/") (connection *connection*))
  (let ((opts (merge-hash-tables #.(ia-hash-table:alist-ia-hash-table '(("type" . "direct")
                                                                        ("auto-delete" . yason:false)
                                                                        ("durable" . t)
                                                                        ("arguments" . #())))
                                 (ia-hash-table:alist-ia-hash-table attributes))))
    (connection.put connection #?"exchanges/${(escape vhost)}/${(escape name)}" (json-encode-to-string opts))))

(defun exchange.delete (name &key (vhost "/") if-unused (connection *connection*))
  (let ((if-unused-query (if if-unused
                             "?if-unused=true"
                             "")))
    (connection.delete connection #?"exchanges/${(escape vhost)}/${(escape name)}${if-unused-query}")))

(defun list-bindings-by-source (name &key (vhost "/") (connection *connection*))
  (connection.get connection #?"exchanges/${(escape vhost)}/${(escape name)}/bindings/source"))

(defun list-bindings-by-destination (name &key (vhost "/") (connection *connection*))
  (connection.get connection #?"exchanges/${(escape vhost)}/${(escape name)}/bindings/destination"))

(defun queues (&key vhost (connection *connection*))
  (let ((path (if vhost
                  #?"queues/${(escape vhost)}"
                  "queues")))
    (connection.get connection path)))

(defun queue-info (name &key (vhost "/") (connection *connection*))
  (connection.get connection #?"queues/${(escape vhost)}/${(escape name)}"))

(defun queue-bindings (name &key (vhost "/") (connection *connection*))
  (connection.get connection #?"queues/${(escape vhost)}/${(escape name)}/bindings"))

(defun queue-messages (name &key (vhost "/") (connection *connection*))
  (connection.post connection #?"queues/${(escape vhost)}/${(escape name)}/get" nil))

(defun queue.declare (name attributes &key (vhost "/") (connection *connection*))
  (connection.put connection #?"queues/${(escape vhost)}/${(escape name)}" (json-encode-to-string attributes)))

(defun queue.delete (name &key (vhost "/") (connection *connection*))
  (connection.delete connection #?"queues/${(escape vhost)}/${(escape name)}"))

(defun queue.purge (name &key (vhost "/") (connection *connection*))
  (connection.delete connection #?"queues/${(escape vhost)}/${(escape name)}/contents"))

(defun bindings (&key vhost (connection *connection*))
  (let ((path (if vhost
                  "bindings"
                  #?"bindings/${(escape vhost)}")))
    (connection.get connection path)))

(defun queue-exchange-bindings (queue exchange &key (vhost "/") (connection *connection*))
  (connection.get connection #?"bindings/${(escape vhost)}/e/${(escape exchange)}/q/${(escape queue)}"))

(defun queue-binding-info (queue exchange properties-key &key (vhost "/") (connection *connection*))
  (connection.get connection #?"bindings/${(escape vhost)}/e/${(escape exchange)}/q/${(escape queue)}/${(escape properties-key)}"))

(defun queue.bind (queue exchange routing-key &key (arguments) (vhost "/") (connection *connection*))
  (connection.post connection #?"bindings/${(escape vhost)}/e/${(escape exchange)}/q/${(escape queue)}"
                   (json-encode-to-string `(("routing_key" . ,routing-key)
                                            ("arguments" . ,arguments)))))

(defun queue.unbind (queue exchange properties-key &key (vhost "/") (connection *connection*))
  (connection.delete connection #?"bindings/${(escape vhost)}/e/${(escape exchange)}/q/${(escape queue)}/${(escape properties-key)}"))



(defun vhosts (&optional (connection *connection*))
  (connection.get connection "vhosts"))

(defun vhost-info (name &optional (connection *connection*))
  (connection.get connection #?"vhosts/${(escape name)}"))

(defun vhost.create (name &optional (connection *connection*))
  (connection.put (connection.get connection #?"vhosts/${(escape name)}") nil))

(defun vhost.delete (name &optional (connection *connection*))
  (connection.delete connection #?"vhosts/${(escape name)}"))



(defun permissions (&key vhost (connection *connection*))
  (let ((path (if vhost
                  #?"permissions/${(escape vhost)}"
                  "permissions")))
    (connection.get connection path)))

(defun users (&optional (connection *connection*))
  (connection.get connection "users"))

(defun user-info (user &optional (connection *connection*))
  (connection.get connection "users/${(escape user)}"))

(defun user-vhost-permissions (&key (vhost "/") (connection *connection*))
  (connection.get connection #?"permissions/${(escape vhost)}/${(escape user)}"))

(defun user.clear-vhost-permissions (&key (vhost "/") (connection *connection*))
  (connection.delete connection #?"permissions/${(escape vhost)}/${(escape user)}"))

(defun user.update (name attributes &optional (connection *connection*))
  (connection.put #?"users/${(escape user)}" (json-encode-to-string attributes)))

(defun user.delete (name attributes &optional (connection *connection*))
  (connection.delete #?"users/${(escape user)}"))

(defun user-permissions (user &optional (connection *connection*))
  (connection.get connection "users/${(escape user)}/permissions"))

(defun whoami (&optional (connection *connection*))
  (connection.get connection "whoami"))



(defun policies (&key vhost (connection *connection*))
  (let ((path (if vhost
                  #?"policies/${(escape vhost)}"
                  "policies")))
    (connection.get connection path)))

(defun policy-info (name &key vhost (connection *connection*))
  (connection.get connection #?"policies/${(escape vhost)}/${(escape name)}"))

(defun policy.update (name attributes &key vhost (connection *connection*))
  (connection.put connection #?"policies/${(escape vhost)}/${(escape name)}" (json-encode-to-string attributes)))

(defun policy.clear (name &key vhost (connection *connection*))
  (connection.delete connection #?"policies/${(escape vhost)}/${(escape name)}"))



(defun parameters (&key component (connection *connection*))
  (let ((path (if component
                  #?"parameters/${(escape component)}"
                  "parameters")))
    (connection.get connection path)))

(defun parameter-info (name &key vhost (connection *connection*))
  (connection.get connection #?"parameters/${(escape vhost)}/${(escape name)}"))

(defun parameter.update (name attributes &key vhost (connection *connection*))
  (connection.put connection #?"parameters/${(escape vhost)}/${(escape name)}" (json-encode-to-string attributes)))

(defun parameter.clear (name &key vhost (connection *connection*))
  (connection.delete connection #?"parameters/${(escape vhost)}/${(escape name)}"))

(defun alive-p (&key (vhost "/") (connection *connection*))
  (let ((r (connection.get connection #?"aliveness-test/${(escape vhost)}")))
    (equal "ok" #Ir?.status)))
