
(in-package #:translator)

(defun get-files (input)
  "Returns a list of .vm files to process based on input.
Will return an empty list if no applicable files."
  (let ((name (truename input)))
    (flet ((is-vm (f) (equalp "vm" (pathname-type f))))
      (cond ((uiop:file-exists-p name)
             (when (is-vm name)
               (list name)))
            ((uiop:directory-exists-p name)
             (remove-if-not #'is-vm (uiop:directory-files name)))
            (t (error "Cannot process the input file ~A" input))))))

(defun asm-file-name (filename)
  "Create an ASM filename from the provided FILENAME. If FILENAME is a
directory, use the directory name."
  (let ((initial-name (pathname-name filename))
        (directory (pathname-directory filename)))
    (let ((name (if initial-name
                    initial-name
                    (car (last directory)))))
      (make-pathname :directory directory
                     :name name
                     :type "asm"))))

(defun translate (input &optional (output (asm-file-name input)))
  "Translate the INPUT VM files into an ASM OUTPUT file. INPUT can be a single .vm file
or else a directory, in which case it will process all .vm files."
  (let ((files (get-files input)))
    (write-asm-file files output)
    (format t "Translation of VM input files is complete.")))
