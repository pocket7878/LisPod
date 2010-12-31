;;; -* Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;    Lispod: A simple Podcast manager written in Common Lisp.
;;;    Copyright (C) 2010 Masato Sogame (poketo7878@yahoo.co.jp)
;;;
;;;    This program is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :lispod)

(defun html-tag-remover (str)
  (regex-replace-all "<.*?>" str ""))

(defun get-specific-length-of-list (n lst)
  (labels ((%fn (n lst acc)
		(if (or (zerop n) (null lst))
		  acc
		  (%fn (1- n) (cdr lst) (cons (car lst) acc)))))
    (nreverse (%fn n lst nil))))

;;Podcast class
(defclass Podcast ()
  ((name :initarg :name :initform "" :accessor name)
   (url :initarg :url :initform "" :accessor url)))

(defmethod get-cast-file-url-list ((cast Podcast))
  (mapcar #'(lambda (x) 
	      (multiple-value-bind (matchstring groups)
		(scan-to-strings "<enclosure.*?url=\"(.*?)\".*?/>" x)
		(declare (ignore matchstring))
		(aref groups 0)))
	  (all-matches-as-strings "<enclosure.*?/>" (http-request (url cast)))))

(defmethod get-latest-cast ((p Podcast))
  (car (get-cast-file-url-list p)))

;;Container class of podcast
(defclass Podcast-container ()
  ((podcasts :initarg :podcasts :initform nil :accessor podcasts)))

;;Constructor with rc file
(defun make-podcast-container ()
  (let ((rc-file-contents 
	  (with-open-file (fp "~/.lispod" 
			      :direction :input)
	    (read fp)))
	(new-instance (make-instance 'Podcast-container)))
    (loop for cast-data in rc-file-contents
	  do
	  (add-podcast new-instance
		       (make-instance 'Podcast
				      :name (car cast-data)
				      :url (cadr cast-data))))
    new-instance))
(defmethod add-podcast ((pc Podcast-container) (pd Podcast))
  (setf (podcasts pc) (cons pd (podcasts pc))))

(defmethod remove-podcast ((pc Podcast-container) (pd Podcast))
  (setf (podcasts pc) (remove-if #'(lambda (x) (eq pd x)) (podcasts pc))))

(defmethod get-all-cast-file-url-list ((pc Podcast-container))
  (mapcar #'(lambda (x) (get-cast-file-url-list x))
	  (podcasts pc)))

(defmethod get-all-latest-cast-file-url-list ((pc Podcast-container))
  (mapcar #'(lambda (x) (get-latest-cast x))
	  (podcasts pc)))

;;Progress bar
(defclass progress-bar ()
  ((total :initform 0 :initarg :total :accessor total)
   (current :initform 0 :initarg :current :accessor current)))

(defgeneric print-bar (pg stream))

(defmethod print-bar ((pg progress-bar) stream)
  (with-slots (current total) pg
    (format stream "~5,1f%: ~A>"
	    (float (* 100 (/ current total)))
	    (make-string (* 4 (floor (* 100 (/ current total)) 10)) :initial-element #\=))))

(defgeneric set-value (pg value))

(defmethod set-value ((pg progress-bar) value)
  (setf (current pg) value))

(defgeneric set-total (pg value))

(defmethod set-total ((pg progress-bar) value)
  (setf (total pg) value))


;;View for list of podcast
(defclass podcast-list-view (view) ())
(defparameter *podcast-list-view* (make-instance 'podcast-list-view))

;;View for podcast cast list or some
(defclass podcast-view (view) 
  ((%podcast :initarg :podcast :reader podcast)
   (%num :initarg :num :reader num)))

;;View for downloading progress
(defclass download-view (view)
  ((%url :initarg :url :reader url)
   (%file-path :initarg :file-path :reader file-path)))

(define-application-frame lispod-main ()
  ((my-podcast :initform (make-podcast-container)
	       :accessor my-podcast))
  (:menu-bar menubar-command-table)
  (:pointer-documentation t)
  (:panes
    (app :application :height 400 :width 600
	 :display-function 'display-application
	 :default-view *podcast-list-view*)
    (int :interactor :height 200 :width 600))
  (:layouts
    (default (vertically () app int))))

(defgeneric display-pane-with-view (frame pane view))

(defun display-application (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

(defmethod display-pane-with-view (frame pane (view podcast-list-view))
  (let ((podcasts (podcasts (my-podcast frame))))
    (loop for cast in podcasts
	  do
	  (with-output-as-presentation (pane cast 'Podcast)
	       (with-output-as-presentation (pane (name cast) 'name-of-podcast)
		    (format pane "~A" (name cast)))
	       (format pane "")
	       (with-output-as-presentation (pane (url cast) 'url)
		    (format pane "   ~A~%"
				    (url cast)))))))

(defmethod display-pane-with-view (frame pane (view podcast-view))
  (let ((podcast (podcast view))
	(num (num view)))
    (with-output-as-presentation (pane (name podcast) 'name-of-podcast)
      (format pane "Name: ~A~%"
	      (name podcast)))
    (loop for url in (get-specific-length-of-list num (get-cast-file-url-list podcast))
	  do
	  (with-output-as-presentation (pane url 'url)
		(format pane "~A~%" url)))))

(defmethod display-pane-with-view (frame pane (view download-view))
  (let ((url (url view))
	(file-path (file-path view)))
    (with-open-file (out file-path
			 :direction :output
			 :if-exists :overwrite
			 :if-does-not-exist :create
			 :element-type '(unsigned-byte 8))
      (let ((v (make-array 2048 :element-type '(unsigned-byte 8)))
	    (total-size nil)
	    (downloaded-byte 0)
	    (pgbar (make-instance 'progress-bar)))
	(multiple-value-bind (input num headers)
	  (http-request url :want-stream t)
	  (declare (ignore num))
	  (setq total-size (parse-integer (header-value :content-length headers)))
	  (when (not (null total-size))
	    (set-total pgbar total-size))
	 (loop for size = (read-sequence v input)
	       until (zerop size)
	       do 
	       (progn
		 (when (not (null total-size))
		   (incf downloaded-byte size)
		   (set-value pgbar downloaded-byte)
		   (window-clear (get-frame-pane frame 'app))
		   (format pane "Downloading...~%")
		   (format pane "URL: ~A~%" url)
		   (format pane "Save To: ~A~%" file-path)
		   (format pane "~A~%" (print-bar pgbar nil))
		   (finish-output))
		 (write-sequence v out :end size))))))))

(defun lispod (&key (new-process nil) (process-name "LisPod"))
  (let ((app-frame (make-application-frame 'lispod-main)))
    (flet ((run () (run-frame-top-level app-frame)))
      (if new-process
	(clim-sys:make-process #'run :name process-name)
	(run)))))

(define-lispod-main-command (com-quit :name t
				      :keystroke (#\q :meta)) ()
    (when (not (null (podcasts (my-podcast *application-frame*))))
      (with-open-file (out "~/.lispod"
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	(format out "~A" (generate-pretty-print-list (my-podcast *application-frame*)))))
    (frame-exit *application-frame*))

(define-lispod-main-command (com-change-url :name t) ((pd 'Podcast) (url 'url))
  (setf (url pd) url))

(define-lispod-main-command (com-change-name :name t) ((pd 'Podcast) (name 'name-of-podcast))
  (setf (name pd) name))

(define-presentation-type name-of-podcast ()
			  :inherit-from 'string)

(define-presentation-type url ()
			  :inherit-from 'string)

(define-presentation-type file-path ()
			  :inherit-from 'pathname)

(define-lispod-main-command (com-add-podcast :name t) ((name 'name-of-podcast) (url 'url))
  (add-podcast (my-podcast *application-frame*) 
	       (make-instance 'Podcast
			      :name name
			      :url url)))

(define-lispod-main-command (com-remove-podcast :name t) ((pd 'Podcast :gesture :select))
  (remove-podcast (my-podcast *application-frame*)
		  pd))

(define-lispod-main-command (com-show-all :name t) ()
  (setf (stream-default-view *standard-output*) *podcast-list-view*))

(define-lispod-main-command (com-show-podcast :name t) ((pd 'Podcast) (num 'integer))
  (setf (stream-default-view *standard-output*)
	(make-instance 'podcast-view 
		       :podcast pd
		       :num num)))

(define-lispod-main-command (com-download :name t) ((url 'url) (file-name 'file-path
									  :default (user-homedir-pathname)
									  :insert-default t))
  (setf (stream-default-view *standard-output*)
	(make-instance 'download-view
		       :url url
		       :file-path file-name)))

(define-lispod-main-command (com-save-podcast-list :name t) ((file-path 'file-path
									:default (user-homedir-pathname)
									:insert-default t))
    (when (not (null (podcasts (my-podcast *application-frame*))))
	      (with-open-file (out file-path
				   :direction :output
				   :if-exists :overwrite
				   :if-does-not-exist :create)
		(format out "~A" (generate-pretty-print-list (my-podcast *application-frame*))))))

(defmethod equal-data ((pd1 Podcast) (pd2 Podcast))
  (and (string= (name pd1) (name pd2))
       (string= (url pd1) (url pd2))))

(define-lispod-main-command (com-load-podcast-list :name t) ((file-path 'file-path
									:default (user-homedir-pathname)
									:insert-default t))
			    (let ((rc-file-contents 
				    (with-open-file (fp file-path
							:direction :input)
				      (read fp))))
			      (setf (podcasts (my-podcast *application-frame*))
				    (remove-duplicates
				      (append (mapcar #'(lambda (cast-data) 
							  (make-instance 'Podcast 
									 :name (car cast-data) 
									 :url (cadr cast-data)))
						      rc-file-contents)
					      (podcasts (my-podcast *application-frame*)))
				      :test #'equal-data))))

;;Generate pretty-print list of podcast
(defmethod generate-pretty-print-list ((pd Podcast-container))
  (reverse (loop for cast in (podcasts pd)
		 collect (list (write-to-string (name cast)) (write-to-string (url cast))))))

(make-command-table 'lispod-file-menu
		    :errorp nil
		    :menu '(("Save" :command com-save-podcast-list)
			    ("Load" :command com-load-podcast-list)
			    ("Quit" :command com-quit)))

(make-command-table 'lispod-edit-menu
		    :errorp nil
		    :menu '(("Change Name" :command com-change-name)
			    ("Change URL" :command com-change-url)
			    ("Add Podcast" :command com-add-podcast)
			    ("Remove Podcast" :command com-remove-podcast)))

(make-command-table 'menubar-command-table 
		    :errorp nil
		    :menu '(("File" :menu lispod-file-menu)
			    ("Edit" :menu lispod-edit-menu)))
