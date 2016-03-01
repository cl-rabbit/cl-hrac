(in-package :cl-hrac)

(defun escape (str &optional (safe ""))
  "URI encodes/escapes the given string."
  (with-output-to-string (s)
    (loop for c across (flexi-streams:string-to-octets str :external-format :utf-8)
          do (if (or (find (code-char c) safe)
                     (<= 48 c 57)
                     (<= 65 c 90)
                     (<= 97 c 122)
                     (find c '(45 95 46 126)))
              (write-char (code-char c) s)
              (format s "%~2,'0x" c)))))

(defun json-encode-to-string (object)
  (with-output-to-string (stream)
    (yason:encode object stream)))

(defun merge-hash-tables (&rest hash-tables)
  "Each subsequentional hash-table override existing key-values"
  (let ((final-hash-table (ia-hash-table:make-ia-hash-table)))
    (loop for hash-table in hash-tables do
          (with-hash-table-iterator (next hash-table)
            (loop
              (multiple-value-bind (more? key value) (next)
                (unless more?
                  (return))
                (setf (gethash key final-hash-table) value)))))
    final-hash-table))
