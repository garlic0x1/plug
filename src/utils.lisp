(defpackage #:plug/utils
  (:use :cl)
  (:import-from #:alexandria-2 #:rcurry #:line-up-first)
  (:export #:rmdir #:git-name))
(in-package :plug/utils)

(defun walk-to-root (path)
  "Walk up the file tree collecting directories."
  (let ((dir (uiop:pathname-directory-pathname path)))
    (if (equal #P"/" dir)
        (list dir)
        (cons dir (walk-to-root (uiop:pathname-parent-directory-pathname dir))))))

(defun subdirectory-p (path parent)
  "Path is a child of parent."
  (find parent (cdr (walk-to-root path))))

(defun rmdir (path &key ensure-below)
  "Safely remove a directory below `ensure-below`."
  (line-up-first
   path
   (uiop:ensure-directory-pathname)
   (uiop:merge-pathnames*)
   (uiop:delete-directory-tree :validate (rcurry #'subdirectory-p ensure-below))))

(defun git-name (url)
  "Extract name from a git URL."
  (let ((filename (car (last (str:split "/" url)))))
    (if (str:ends-with? ".git" filename)
        (str:substring 0 (- (length filename) 4) filename)
        filename)))
