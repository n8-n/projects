
(in-package #:hack-asm)

(defparameter *current-program-counter* 0
  "Used to keep track of program counter number during first-pass
processing of labels.")


(defun init-assembler ()
  "Run initialising functions for symbol table, code mappers etc."
  (init-comp-mappings)
  (init-jmp-mappings)
  (add-default-symbols-to-table))

(init-assembler)

(defun hack-file-name (filename)
  "Create a hack filename from the provided FILENAME."
  (let ((name (pathname-name filename))
        (directory (pathname-directory filename)))
    (make-pathname :directory directory
                   :name name
                   :type "hack")))

(defun process-labels (contents)
  "First pass to process all labels."
  (flet ((inc-pc ()
           (setf *current-program-counter*
                 (+ *current-program-counter* 1))))
    (dolist (command contents)
      (let ((type (command-type command)))
        (case type
          (:A (inc-pc))
          (:C (inc-pc))
          (:L (process-l-command command
                                 *current-program-counter*)))))))


(defun process-line (cmd)
  "Determine command type and send to correct function."
  (let* ((command (string-trim " " cmd))
         (type (command-type command)))
    (case type
      (:A (process-a-command command))
      (:C (process-c-command command)))))

;; TODO: it's writing an extra newline at end of hack file
(defun assemble (file &optional (out-file (hack-file-name file)))
  "Reads the provided ASM FILE and assembles contents into
Hack binary code stored in OUT-FILE. If no OUT-FILE provided,
create one based on input file name."
  (let* ((in-file (truename file))
         (contents (read-asm-file in-file)))
    (if (null contents)
        (error "Error reading contents of ASM file: empty file")
        (progn
          (process-labels contents)
          (let ((results (mapcar #'process-line contents)))
            (write-hack-file results out-file))))
    (format t "Assembly complete.")
    out-file))
