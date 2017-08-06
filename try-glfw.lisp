;;;; try-glfw.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:try-glfw)

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun render ())

(def-window-size-callback update-viewport (window width height)
  (declare (ignore window))
  (gl:viewport 0 0 width height))

(defun main ()
  ;; Graphics calls on OS X must occur in the main thread
  (trivial-main-thread:with-body-in-main-thread ()
    (with-init-window (:title "Window test"
                              :width 1600
                              :height 1200
                              :monitor (glfw:get-primary-monitor)
                              :opengl-profile :opengl-core-profile
                              :context-version-major 3
                              :context-version-minor 2
                              :opengl-forward-compat t
                              :resizable nil
                              )

      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)

      (loop until (window-should-close-p)
         do (render)
         do (swap-buffers)
         do (poll-events)))))
