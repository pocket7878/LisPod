(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "mcclim")
  (ql:quickload "drakma")
  (ql:quickload "cl-ppcre"))

(defpackage :lispod
  (:use :clim :clim-lisp :drakma :cl-ppcre))

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
    (format stream "[~A>~A] ~5,1f"
	    (make-string (* 4 (floor (* 100 (/ current total)) 10)) :initial-element #\=)
	    (make-string (* 4 (- 10 (floor (* 100 (/ current total)) 10))) :initial-element #\Space)
	    (float (* 100 (/ current total))))))

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
		   (format pane "~A~%" (print-bar pgbar nil))
		   (finish-output))
		 (write-sequence v out :end size))))))))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'lispod-main)))

(define-lispod-main-command (com-quit :name t
				      :keystroke (#\q :meta)) ()
    (when (not (null (podcasts (my-podcast *application-frame*))))
      (with-open-file (out "~/.lispod"
			   :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
	(format out "~A" (generate-pretty-print-list (my-podcast *application-frame*)))))
    (frame-exit *application-frame*))

(define-presentation-type name-of-podcast ()
			  :inherit-from 'string)

(define-presentation-type url ()
			  :inherit-from 'string)

(define-presentation-type file-path ()
			  :inherit-from 'string)

(define-lispod-main-command (com-add-podcast :name t) ((name 'name-of-podcast) (url 'url))
  (add-podcast (my-podcast *application-frame*) 
	       (make-instance 'Podcast
			      :name name
			      :url url)))

(define-lispod-main-command (com-remove-podcast :name t) ((pd 'Podcast))
  (remove-podcast (my-podcast *application-frame*)
		  pd))

(define-lispod-main-command (com-show-all :name t) ()
  (setf (stream-default-view *standard-output*) *podcast-list-view*))

(define-lispod-main-command (com-show-podcast :name t) ((pd 'Podcast) (num 'integer))
  (setf (stream-default-view *standard-output*)
	(make-instance 'podcast-view 
		       :podcast pd
		       :num num)))

(define-lispod-main-command (com-download :name t) ((url 'url) (file-name 'file-path))
  (setf (stream-default-view *standard-output*)
	(make-instance 'download-view
		       :url url
		       :file-path file-name)))

(define-lispod-main-command (com-save-podcast-list :name t) ((file-path 'file-path))
    (when (not (null (podcasts (my-podcast *application-frame*))))
	      (with-open-file (out file-path
				   :direction :output
				   :if-exists :overwrite
				   :if-does-not-exist :create)
		(format out "~A" (generate-pretty-print-list (my-podcast *application-frame*)))))
    (frame-exit *application-frame*))

(defmethod equal-data ((pd1 Podcast) (pd2 Podcast))
  (and (string= (name pd1) (name pd2))
       (string= (url pd1) (url pd2))))

(define-lispod-main-command (com-load-podcast-list :name t) ((file-path 'file-path))
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

(make-command-table 'menubar-command-table 
		    :errorp nil
		    :menu '(("File" :menu lispod-file-menu)))
