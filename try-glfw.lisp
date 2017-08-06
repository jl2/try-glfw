;;;; try-glfw.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:try-glfw)

(defvar *shader-vao-vertex-program*
  "#version 330
vec4 clipCoord = gl_ModelViewProjectionMatrix * gl_Vertex;
gl_Position = clipCoord;
gl_FrontColor = gl_Color;
vec3 ndc = clipCoord.xyz / clipCoord.w;
gl_SecondaryColor = (ndc * 0.5) + 0.5;
}
")

(defvar *shader-vao-fragment-program*
  "#version 330
void main(void)
{
gl_FragColor = mix(gl_Color, vec4(vec3(gl_SecondaryColor), 1.0), 0.5);
}
")

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun render ()
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:rect -25 -25 25 25)))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun main ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "Window test"
                              :width 600
                              :height 400
                              :opengl-profile :opengl-core-profile
                              :context-version-major 3
                              :context-version-minor 2
                              :opengl-forward-compat t
                              :resizable nil)

      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport 600 400)
      (loop until (window-should-close-p)
         do (render)
         do (swap-buffers)
         do (poll-events)))))

