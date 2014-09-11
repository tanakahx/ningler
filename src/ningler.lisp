#|
  ningler.lisp - main program

  The MIT License (MIT)
  
  Copyright (c) 2014 Hiroyuki Tanaka <tanakahx@gmail.com>
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  
  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package :cl-user)
(defpackage ningler
  (:use :cl))
(in-package :ningler)

(defvar *app* (make-instance 'ningle:<app>))
(defparameter *username* "admin")
(defparameter *password* "default")
(defparameter *layout*
  (merge-pathnames #P"src/template/layout.html" app-config:*base-directory*))
(defparameter *show-entries*
  (merge-pathnames #P"src/template/show-entries.html" app-config:*base-directory*))
(defparameter *login*
  (merge-pathnames #P"src/template/login.html" app-config:*base-directory*))
(defparameter *db*
  (merge-pathnames #P"src/ningler.db" app-config:*base-directory*))
(defparameter *schema*
  (merge-pathnames #P"src/schema.sql" app-config:*base-directory*))
(defparameter *static*
  (merge-pathnames #P"src/static/style.css" app-config:*base-directory*))

(defun init-db ()
  "Initialize database with a schema description which is specified in *schema*. This requires SQLite."
  (dbi:with-connection (conn :sqlite3 :database-name *db*)
    (let ((queries (remove-if #'(lambda (x) (= 0 (length (string-trim '(#\Space #\Tab #\Newline) x))))
                              (split-sequence:split-sequence #\; (read-file *schema*)))))
      (loop for q in queries
         do (dbi:do-sql conn q)))))

(defun add-entry (title text)
  "Adds an entry to the database."
  (dbi:with-connection (conn :sqlite3 :database-name *db*)
    (dbi:do-sql conn "insert into entries (title, text) values (?, ?)" title text)))

(defun fetch-entries ()
  "Returns all entries in a database with a list."
  (dbi:with-connection (conn :sqlite3 :database-name *db*)
    (let* ((query (dbi:prepare conn "select title, text from entries order by id desc"))
           (result (dbi:execute query)))
      (loop for row = (dbi:fetch result)
         while row
         collect row))))

(defmacro get-session (key)
  `(gethash ,key (ningle:context :session)))

(defmacro flash (s)
  "Holds messages in the session until the next request."
  `(push ,s (get-session :flash)))

(defun get-flashed-messages ()
  "Pulls all flashed messages from the session and return them with a list."
  (let ((lst (reverse (get-session :flash))))
    (setf (get-session :flash) nil)
    (mapcar #'(lambda (x) (list :message x)) lst)))

(defmacro redirect (location &optional (status 302) (body "Redirecting..."))
  `'(,status (:location ,location) (,body)))

(defmacro with-layout (&body body)
  (let ((s `(concatenate 'string ,@body)))
    `(cl-emb:execute-emb *layout*
                         :env (list :logged-in (get-session :logged-in)
                                    :messages (get-flashed-messages)
                                    :body ,s))))

(defun authorize (username password)
  "Trivial authorization. Don't use this method in actual applications."
  (and (equal username *username*)
       (equal password *password*)))

(defun read-file (path)
  (with-open-file (in path)
    (let ((seq (make-array (file-length in) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq in))
      seq)))

(setf (ningle:route *app* "/")
      #'(lambda (params)
          (declare (ignore params))
          (with-layout 
            (cl-emb:execute-emb *show-entries*
                                :env (list :logged-in (get-session :logged-in)
                                           :entries (fetch-entries))))))

(setf (ningle:route *app* "/add" :method :POST)
      #'(lambda (params)
          (add-entry (getf params :|title|) (getf params :|text|))
          (flash "New entry was successfully posted.")
          (redirect "/")))

(setf (ningle:route *app* "/login" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (with-layout (cl-emb:execute-emb *login* :env '(:logged-in nil)))))

(setf (ningle:route *app* "/login" :method :POST)
      #'(lambda (params)
          (if (authorize (getf params :|username|)
                         (getf params :|password|))
              (progn (setf (get-session :logged-in) t)
                     (flash "You were logged in.")
                     (redirect "/"))
              (with-layout 
                (cl-emb:execute-emb *login*
                                    :env '(:logged-in nil :error "Invalid username or password"))))))

(setf (ningle:route *app* "/logout")
      #'(lambda (params)
          (declare (ignore params))
          (setf (get-session :logged-in) nil)
          (flash "You were logged out.")
          (redirect "/")))

(setf (ningle:route *app* "/static/*")
      #'(lambda (params)
          (declare (ignore params))
          (list 200 '(:content-type "text/css") (cl-emb:execute-emb *static*))))

(defvar *handler*
  (clack:clackup 
   (clack.builder:builder
    clack.middleware.session:<clack-middleware-session>
    *app*)))
