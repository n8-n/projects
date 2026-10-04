
(in-package #:hack-asm)

(defun hack-file-name (filename)
  "Create a hack filename from the provided FILENAME."
  (let ((name (pathname-name filename))
        (directory (pathname-directory filename)))
    (make-pathname :directory directory
                   :name name
                   :type "hack")))
    
(defun process-line (command)
  "Determine command type and send to correct function."
  (let ((type (command-type command)))
    (case type
      (:A (process-a-command command))
      (:C (process-c-command command))
      (:L 'TODO))))

;; TODO: it's writing an extra newline at end of hack file
(defun assemble (file &optional (out-file (hack-file-name file)))
  "Reads the provided ASM FILE and assembles contents into
Hack binary code stored in OUT-FILE. If no OUT-FILE provided,
create one based on input file name."
  (let* ((in-file (truename file))
         (contents (read-asm-file in-file)))
    (if (null contents)
        (error "Error reading contents of ASM file: empty file")
        (let ((results (mapcar #'process-line contents)))
          (write-hack-file results out-file)))))
