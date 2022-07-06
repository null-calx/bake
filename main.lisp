(in-package :bake)

(defun file-last-modification (path)
  (let ((stat (osicat-posix:stat path)))
    (osicat-posix:stat-mtime stat)))

(defun build-again? (file &rest dependencies)
  (let ((lastmod (file-last-modification file)))
    (when dependencies
      (loop
	for dp-file in dependencies
	when (< lastmod (file-last-modification dp-file))
	  do (return-from build-again? t))
      nil)))

;; (defun log ())

(defun exec (command)
  (uiop:run-program command
		    :ignore-error-status nil
		    :output t
		    :error-output t))

(defun bake (tree)
  (loop
    for branch in tree
    when (apply #'build-again? (getf branch :file) (getf branch :depends-on))
      do (loop
	   for cmd in (getf branch :exec)
	   do (exec cmd))))
