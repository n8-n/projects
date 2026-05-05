(require 'usocket)

(defun create-server (port)
  (let* ((socket (usocket:socket-listen "127.0.0.1" port))
         (connection (usocket:socket-accept socket :element-type 'character)))
    (unwind-protect
         (progn
           (format (usocket:socket-stream connection) "Hello World~%")
           (force-output (usocket:socket-stream connection)))
      (progn
        (format t "Closing sockets~%")
        (usocket:socket-close connection)
        (usocket:socket-close socket)))))

(defun write-and-flush (string stream)
  (write-string string stream)
  (force-output stream))

