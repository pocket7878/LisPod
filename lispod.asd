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

(defpackage :lispod.system
  (:use :cl :asdf))

(in-package :lispod.system)

(defsystem :lispod
  :depends-on (:mcclim :drakma :cl-ppcre)
  :components
  ((:file "package")
   (:file "lispod" :depends-on ("package"))))
