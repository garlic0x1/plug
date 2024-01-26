(defpackage #:plug/core
  (:nicknames #:plug)
  (:use :cl)
  (:import-from #:alexandria-2 #:line-up-last)
  (:import-from #:plug/utils #:git-name #:rmdir)
  (:export #:*plugin-directory*
           #:list-plugins
           #:clone-plugin
           #:update-plugin
           #:update-all-plugins
           #:delete-plugin
           #:load-plugin
           #:initialize-plug))
(in-package :plug/core)

(defparameter *plugin-directory* #p"/tmp/plug/"
  "Directory to clone plugins into.")

(defun make-plugin-path (name)
  "Construct a full path from a plugin name."
  (merge-pathnames *plugin-directory* name))

(defun initialize-plug ()
  "Create plugin directory, and ensure truename."
  (ensure-directories-exist *plugin-directory*)
  (setf *plugin-directory* (truename *plugin-directory*)))

(defun clone-plugin (url)
  "Clone plugin at git URL."
  (uiop:with-current-directory ((make-plugin-path (git-name url)))
    (uiop:run-program (format nil "git clone ~a" url))))

(defun delete-plugin (name)
  "Delete plugin by name."
  (rmdir (make-plugin-path name) :ensure-below *plugin-directory*))

(defun update-plugin (name)
  "Update plugin by name, this uses `git pull`."
  (uiop:with-current-directory ((make-plugin-path name))
    (uiop:run-program "git pull")))

(defun list-plugins ()
  "List the names of all plugins in the plugin directory."
  (line-up-last
   (uiop:subdirectories *plugin-directory*)
   (mapcar #'uiop:lispize-pathname)
   (mapcar #'pathname-directory)
   (mapcar #'last)
   (mapcar #'car)))

(defun update-all-plugins ()
  "Update all plugins installed to *plugin-directory*, uses `git pull`."
  (loop :for plugin :in (list-plugins)
        :do (update-plugin plugin)))

(defun load-plugin (system)
  "Add plugins to the central registry."
  (let ((ql:*local-project-directories*
          (cons *plugin-directory*
                ql:*local-project-directories*)))
    (ql:quickload system)))
