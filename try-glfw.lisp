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

(defun initialize-opengl ()
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test)

  (let ((buffers (gl:gen-buffers 2)))
    (setf (vertex-buffer  shader-example) (elt buffers 0)
          (index-buffer  shader-example) (elt buffers 1)))
  (gl:bind-buffer :array-buffer (vertex-buffer  shader-example))
  (let ((arr (gl:alloc-gl-array :float 12))
        (verts #(-0.5f0 -0.5f0 0.0f0 
                 -0.5f0 0.5f0 0.0f0 
                 0.5f0 -0.5f0 0.0f0 
                 0.5f0 0.5f0 0.0f0)))
    (dotimes (i (length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; An element array buffer stores vertex indices. We fill it in the
  ;; same way as an array buffer.
  (gl:bind-buffer :element-array-buffer (index-buffer  shader-example))
  (let ((arr (gl:alloc-gl-array :unsigned-short 6))
        (indexes #(0 2 1 1 2 3)))
    (dotimes (i (length indexes))
      (setf (gl:glaref arr i) (aref indexes i)))
    (gl:buffer-data :element-array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :element-array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (setf (vertex-array  shader-example) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vertex-array  shader-example))

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer (vertex-buffer  shader-example))
  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

  ;; To associate an element array with this VAO, all we need to do is
  ;; bind the element array buffer we want to use.
  (gl:bind-buffer :element-array-buffer (index-buffer  shader-example))

  ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
  (gl:bind-vertex-array 0)

  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader)))
    (setf (vertex-shader  shader-example) vs)
    (setf (fragment-shader  shader-example) fs)
    (gl:shader-source vs *shader-vao-vertex-program*)
    (gl:compile-shader vs)
    (gl:shader-source fs *shader-vao-fragment-program*)
    (gl:compile-shader fs)
    ;; If the shader doesn't compile, you can print errors with:
    ;; (print (gl:get-shader-info-log vs))
    ;; (print (gl:get-shader-info-log fs))

    (setf (program shader-example) (gl:create-program))
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader (program shader-example) vs)
    (gl:attach-shader (program shader-example) fs)
    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shader together
    ;; to form the program.
    (gl:link-program (program shader-example))
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
    (gl:use-program (program shader-example))))

(defun main ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "Window test" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (initialize-opengl)

      (gl:clear-color 0 0 0 0)
      (set-viewport 600 400)
      (loop until (window-should-close-p)
         do (render)
         do (swap-buffers)
         do (poll-events)))))

