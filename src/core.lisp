(defpackage #:plug/core
  (:nicknames #:plug)
  (:use :cl)
  (:import-from #:alexandria-2 #:line-up-last)
  (:import-from #:plug/utils #:git-name #:rmdir)
  (:export #:list-plugins
           #:clone-plugin
           #:delete-plugin
           #:load-plugin-system
           #:initialize-plug))
(in-package :plug/core)

(defparameter *plugin-directory* #p"/tmp/plug/")

(defun make-plugin-path (name)
  "Construct a full path from a plugin name."
  (merge-pathnames *plugin-directory* name))

(defun initialize-plug ()
  "Create plugin directory, and ensure truename."
  (ensure-directories-exist *plugin-directory*)
  (setf *plugin-directory* (truename *plugin-directory*)))

(defun clone-plugin (url)
  "Clone plugin at git URL."
  (uiop:run-program
   (format nil "git clone ~a ~a"
           url
           (make-plugin-path (git-name url)))))

(defun list-plugins ()
  "List the names of all plugins in the plugin directory."
  (line-up-last
   (uiop:subdirectories *plugin-directory*)
   (mapcar #'uiop:lispize-pathname)
   (mapcar #'pathname-directory)
   (mapcar #'last)
   (mapcar #'car)))

(defun delete-plugin (name)
  "Delete plugin by name."
  (rmdir (make-plugin-path name)))

(defun load-plugin-system (system)
  "Add plugins to the central registry."
  (quicklisp:register-local-projects)
  (let ((ql:*local-project-directories*
          (cons *plugin-directory* ql:*local-project-directories*)))
    (ql:quickload system)))
