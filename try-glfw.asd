;;;; try-glfw.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:try-glfw
  :description "Describe try-glfw here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:cl-glfw3 #:cl-opengl #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "try-glfw")))

