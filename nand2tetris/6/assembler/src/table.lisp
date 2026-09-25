
(in-package :asm)

(defclass symbol-table ()
  ((symbols
    :initform (make-hash-table :test #'equalp)
    :accessor symbols
    :documentation "Hash table for symbols")))

(defmethod add-entry ((table symbol-table) symbol address)
  "Add entry to symbol table.
SYMBOL should be a string. ADDRESS can be a string or integer,
but will be inserted into table as an integer."
  (let ((int-a (if (integerp address)
                   address
                   (parse-integer address)))
        (s-table (symbols table)))
    (setf (gethash symbol s-table) int-a)))

(defmethod get-address ((table symbol-table) symbol)
  "Does the table contain the SYMBOL? if so, return address. Else, nil.
Doubles as a 'contains' method."
  (gethash symbol (symbols table)))
