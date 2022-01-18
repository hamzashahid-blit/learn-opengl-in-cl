(defpackage #:cl-learn-gl.1.2
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package #:cl-learn-gl.1.2)

(defparameter *win-width* 600)
(defparameter *win-height* 400)

(defparameter *tri-verts*
  #(-0.5 -0.5 0.0
     0.5 -0.5 0.0
     0.0  0.5 0.0))

(defparameter *quad-verts-old*
  #( 0.5  0.5 0.0   ; top right
     0.5 -0.5 0.0   ; bot right
    -0.5  0.5 0.0   ; top left

     0.5 -0.5 0.0   ; bot right
    -0.5 -0.5 0.0   ; bot left
    -0.5  0.5 0.0)) ; top left

(defparameter *quad-verts*
  #(-0.5  0.5 0.0   ; top left
     0.5  0.5 0.0   ; top right
     0.5 -0.5 0.0   ; bot right
    -0.5 -0.5 0.0)) ; bot left

(defparameter *indices*
  #(0 1 3
    1 2 3)) ; Second Triangle

(defvar *vao* nil)
(defvar *vbo* nil)
(defvar *ebo* nil)

(defparameter *gl-vbo* nil)
(defparameter *gl-ebo* nil)

(defun relative-pathname (rel-path)
  (asdf:system-relative-pathname :learn-clgl rel-path))

;; TODO: Fix Pathnames depending on Current Directory
(defparameter *vertex-shader-source*
  (a:read-file-into-string
    (relative-pathname
      #p"src/learn-opengl/hello-triangle.vert")))

(defparameter *fragment-shader-source*
  (a:read-file-into-string
    (relative-pathname
      #p"src/learn-opengl/hello-triangle.frag")))

;; (defparameter *my-file-name* #.(or *compile-file-truename* *load-truename*))
;; (defparameter *my-file-name2* (uiop:current-lisp-file-pathname))

(defvar *vert-shader* nil)
(defvar *frag-shader* nil)
(defvar *shader-prog* -1)

;; Resize Viewport when we reisze GLFW Window
(glfw:def-window-size-callback resize-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h))

(define-condition compile-error (error)
  ((message
     :initform nil
     :initarg :message
     :reader compile-error-message
     :documentation "The reason given for this error")))

;; TODO: Tell type of shader the error originated from 
(defun check-shader-error (shader-id)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader-id)))
    (unless (equalp error-string "")
      (format t "~A~%" error-string)
      (error
        'compile-error
        :message error-string))))

;; TODO: Check if this can be checking NULL
(defun invalid-prog? (program-id)
  (= program-id -1))

(defun setup-shaders ()
  (loop :while (invalid-prog? *shader-prog*) :do
    (with-simple-restart (retry "Retry compiling shaders.")
      (setf *vert-shader* (gl:create-shader :vertex-shader))
      (setf *frag-shader* (gl:create-shader :fragment-shader))

      ;; Copy our shader source to the OpenGL shader
      (gl:shader-source *vert-shader* *vertex-shader-source*)
      (gl:shader-source *frag-shader* *fragment-shader-source*)

      ;; Compile our shader sources into GPU bytecode
      (gl:compile-shader *vert-shader*)
      (gl:compile-shader *frag-shader*)

      (check-shader-error *vert-shader*)
      (check-shader-error *frag-shader*)

      ;;; Shader Program
      (setf *shader-prog* (gl:create-program))

      (gl:attach-shader *shader-prog* *vert-shader*)
      (gl:attach-shader *shader-prog* *frag-shader*)

      (gl:link-program *shader-prog*)

      ;; Delete unneeded shaders
      (gl:delete-shader *vert-shader*)
      (gl:delete-shader *frag-shader*))))

(defun setup-model ()
  ;;; VAO; Will keep track of buffer & vertex attribute connection
  (setf *vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vao*)

  ;;; VBO
  (setf *vbo* (gl:gen-buffer))
  (gl:bind-buffer :array-buffer *vbo*)


  ;; Allocate a GL Array and store vertices in it
  (setf *gl-vbo* (gl:alloc-gl-array :float (length *quad-verts*)))

  (loop :for vert :across *quad-verts*
        :for i :from 0 :do
    (setf (gl:glaref *gl-vbo* i) vert))

  ;; Store GL Array of Vertices into VBO
  (gl:buffer-data :array-buffer :static-draw *gl-vbo*)

  ;;; EBO
  (setf *ebo* (gl:gen-buffer))
  (gl:bind-buffer :element-array-buffer *ebo*)

  (setf *gl-ebo* (gl:alloc-gl-array :uint (length *indices*)))
  (loop :for index :across *indices*
        :for i :from 0 :do
    (setf (gl:glaref *gl-ebo* i) index))

  (gl:buffer-data :element-array-buffer :static-draw *gl-ebo*)

  ;; Tell OpenGL how to interpret vertex data per vertex attribute
  (gl:vertex-attrib-pointer 0 3 :float nil
                            (* (cffi:foreign-type-size :float) 3) 0)

  ;; Enable "aPos" vertex attribute at location 0 in Vertex Shader 
  (gl:enable-vertex-attrib-array 0)

  
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :element-array-buffer 0)
  (gl:bind-vertex-array 0))

(defun clean-up ()
  (gl:use-program 0)

  (gl:bind-vertex-array 0)
  (gl:delete-vertex-arrays (list *vao*))
  (setf *vao* nil)

  (gl:delete-buffers (list *vbo*))
  (setf *vbo* nil)

  (gl:free-gl-array *gl-vbo*)
  (setf *gl-vbo* nil)

  (gl:delete-buffers (list *ebo*))
  (setf *ebo* nil)

  (gl:free-gl-array *gl-ebo*)
  (setf *gl-ebo* nil)

  (gl:delete-program *shader-prog*)
  (setf *shader-prog* -1)

  (return-from clean-up 0))

(defun render ()
  (gl:clear :depth-buffer-bit :color-buffer-bit)
  (gl:use-program *shader-prog*)

  ;; Bring back state for model
  (gl:bind-vertex-array *vao*)

  ;; (gl:polygon-mode :front-and-back :line)

  ;; (gl:bind-buffer :element-array-buffer *ebo*)
  ;; Draw 0th Vertex attribute in VAO; 3 verts
  (gl:draw-elements :triangles *gl-ebo*) 

  ;; (gl:bind-buffer :element-array-buffer 0)
  (gl:bind-vertex-array 0)

  ;; 30 FPS; TODO: Need Full Game Loop!!!
  (sleep (/ 1 30))) 

(defun hello-triangle ()
  ;; OSX needs draw calls to be on the main thread
  ;; (tmt:with-body-in-main-thread ()
  (glfw:with-init-window (:title "P2 - Hello Triangle"
                          :width *win-width*
                          :height *win-height*
                          :context-version-major 3
                          :context-version-minor 3
                          :opengl-profile :opengl-core-profile)
    (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

    (glfw:set-window-size-callback 'resize-viewport)

    (setup-model)
    (setup-shaders)

    (gl:clear-color 0.0 1.0 1.0 1.0)
    (gl:viewport 0 0 *win-width* *win-height*)

    (loop :until (glfw:window-should-close-p) :do
      (render)
      (glfw:swap-buffers)
      (glfw:poll-events))

    (clean-up)));)
