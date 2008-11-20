;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:(>>>ASD-PKG<<<)
  (:use :cl :asdf))

(in-package #:(>>>ASD-PKG<<<))

(defsystem (>>>PKG<<<)
  :name "(>>>PKG<<<)"
  :depends-on ()
  :components ((:file "package")
               (>>>POINT<<<)))
>>>TEMPLATE-DEFINITION-SECTION<<<
("ASD-PKG"
 (insert (format "%s-asd"
                 (file-name-sans-extension
                  (file-name-nondirectory (buffer-file-name))))))
("PKG" (insert (file-name-sans-extension
                (file-name-nondirectory (buffer-file-name)))))
