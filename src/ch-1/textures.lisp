;;;; Press K & J for FUN!

(defpackage :cl-learn-gl.1.4
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:c #:cffi)))
(in-package :cl-learn-gl.1.4)

;;;; TODO: Seperate shaders into a class

(defparameter *win-width* 600)
(defparameter *win-height* 400)

(defparameter *tri-verts*
  #(-0.5 -0.5 0.0
     0.5 -0.5 0.0
     0.0  0.5 0.0))

(defparameter *verts*
  ;; ;; Positions     Colors       Tex Coords
  ;; #(-0.5 -0.5 0.0  1.0 0.0 0.0  0.0 0.0     ; lower-left
  ;;    0.5 -0.5 0.0  0.0 1.0 0.0  1.0 0.0     ; lower-right
  ;;    0.0  0.5 0.0  0.0 0.0 1.0  0.5 1.0))   ; top-center

  ;; positions      Colors        Tex Coords
  #( 0.5  0.5 0.0   1.0 0.0 0.0   1.0 0.0    ; top right
     0.5 -0.5 0.0   0.0 1.0 0.0   1.0 1.0    ; bottom right
    -0.5 -0.5 0.0   0.0 0.0 1.0   0.0 1.0    ; bottom left
    -0.5  0.5 0.0   1.0 1.0 0.0   0.0 0.0))  ; top left

(defparameter *indices*
  #(0 1 3
    1 2 3))

(defparameter *tex-coords*
  #(0.0 0.0   ; lower-left
    1.0 0.0   ; lower-right
    0.0 0.5)) ; top-center

(defvar *vao* nil)
(defvar *vbo* nil)
(defvar *ebo* nil)
(defparameter *gl-vbo* nil)
(defparameter *gl-ebo* nil)

;;; Textures
(defun rel-path (path)
  (asdf:system-relative-pathname :learn-clgl path))

(defvar *tex* nil)
(defvar *tex2* nil)

;; ;; Also gives Unhandled Memory Error...
;; (multiple-value-bind (data w h)
;;     (cl-soil:load-image (rel-path #p "res/container.jpg"))
;;   (defparameter *tex-data* data)
;;   (defparameter *tex-width* w)
;;   (defparameter *tex-height* h))

;;; GLFW

(defparameter *keys-pressed* nil)
(defparameter *tex-mix* 0)

(glfw:def-window-size-callback resize-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h))

(glfw:def-key-callback handle-keys (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (if (not (eq action :release))
    (pushnew key *keys-pressed*)
    (a:deletef *keys-pressed* key)))

(defun set-tex-mix ()
  (print *keys-pressed*)
  (map nil
    #'(lambda (key)
        (case key
          (:k (incf *tex-mix* 0.025))
          (:j (decf *tex-mix* 0.025))))
    *keys-pressed*))

;;; Shaders

;; TODO: Fix Pathnames depending on Current Directory
(defparameter *vertex-shader-source*
  (a:read-file-into-string
    (rel-path #p"src/learn-opengl/textures.vert")))

(defparameter *fragment-shader-source*
  (a:read-file-into-string
    (rel-path #p"src/learn-opengl/textures.frag")))

(defvar *vert-shader* nil)
(defvar *frag-shader* nil)
(defvar *shader-prog* -1)

(define-condition compile-error (error)
  ((message
     :initform nil
     :initarg :message
     :reader compile-error-message
     :documentation "The reason given for this error")))

(defun check-shader-error (shader-id)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader-id)))
    (unless (equalp error-string "")
      (format t "~A~%" error-string)
      (error
        'compile-error
        :message error-string))))

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

(defun setup-textures ()
  (setf *tex*  (make-texture (rel-path #p"res/container.jpg")))
  (setf *tex2* (make-texture (rel-path #p"res/awesomeface.png"))))

;; TODO: Handle if texture could not be loaded
;; TODO: Seperate texture loading to another function
(defun make-texture (filename)
  (multiple-value-bind (tex-data tex-width tex-height tex-channels tex-load-options)
      (cl-soil:load-image filename)

    (let ((channel (cond
                     ((= tex-channels 3) :rgb)
                     ((= tex-channels 4) :rgba)))
          (load-option (cond
                         ((= tex-load-options 0) :auto)
                         ((= tex-load-options 1) :l)
                         ((= tex-load-options 2) :ela)
                         ((= tex-load-options 3) :rgb)
                         ((= tex-load-options 4) :rgba)))
          (tex-var (gl:gen-texture)))

      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d tex-var)

      ;; Set the texture wrapping/filtering options
      ;; (on the currently bound texture object)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

      (gl:tex-image-2d :texture-2d 0 channel tex-width tex-height
        0 load-option :unsigned-byte tex-data)

      (gl:generate-mipmap :texture-2d)

      ;; Causes Unhandled Memory Error When load-image scope changes
      (c:foreign-free tex-data)

      tex-var)))

(defun setup-model ()
  (setf *vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vao*)

  (setf *vbo* (gl:gen-buffer))
  (gl:bind-buffer :array-buffer *vbo*)

  ;; Allocate a GL Array and store vertices in it
  (setf *gl-vbo* (gl:alloc-gl-array :float (length *verts*)))
  (loop :for vert :across *verts*
    :for i :from 0 :do
    (setf (gl:glaref *gl-vbo* i) vert))

  ;; Store GL Array of Vertices into VBO
  (gl:buffer-data :array-buffer :static-draw *gl-vbo*)



  (setf *ebo* (gl:gen-buffer))
  (gl:bind-buffer :element-array-buffer *ebo*)

  ;; Allocate a GL Array and store vertices in it
  (setf *gl-ebo* (gl:alloc-gl-array :uint (length *indices*)))
  (loop :for index :across *indices*
    :for i :from 0 :do
    (setf (gl:glaref *gl-ebo* i) index))

  (gl:buffer-data :element-array-buffer :static-draw *gl-ebo*)


  ;; Tell OpenGL how to interpret vertex data per vertex attribute
  (gl:vertex-attrib-pointer 0 3 :float nil
    (* (c:foreign-type-size :float) 8) 0)

  ;; Enable "aPos" vertex attribute at location 0 in Vertex Shader 
  (gl:enable-vertex-attrib-array 0)

  (gl:vertex-attrib-pointer 1 3 :float nil
    (* (c:foreign-type-size :float) 8)
    (* (c:foreign-type-size :float) 3))

  (gl:enable-vertex-attrib-array 1)

  (gl:vertex-attrib-pointer 2 2 :float nil
    (* (c:foreign-type-size :float) 8)
    (* (c:foreign-type-size :float) 6))

  (gl:enable-vertex-attrib-array 2)

  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :element-array-buffer 0)
  (gl:bind-vertex-array 0))

(defun clean-up ()
  (gl:use-program 0)

  ;; VAO
  (gl:bind-vertex-array 0)
  (gl:delete-vertex-arrays (list *vao*))
  (setf *vao* nil)

  ;; VBO
  (gl:delete-buffers (list *vbo*))
  (setf *vbo* nil)

  ;; gl-VBO
  (gl:free-gl-array *gl-vbo*)
  (setf *gl-vbo* nil)

  ;; EBO
  (gl:delete-buffers (list *ebo*))
  (setf *ebo* nil)

  ;; gl-VBO
  (gl:free-gl-array *gl-ebo*)
  (setf *gl-ebo* nil)

  (gl:delete-texture *tex*)
  (setf *tex* nil)

  ;; Shader Program
  (gl:delete-program *shader-prog*)
  (setf *shader-prog* -1)

  (setf *keys-pressed* nil)
  (setf *tex-mix* 0)

  (return-from clean-up 0))

(defun render ()
  (gl:clear :depth-buffer-bit :color-buffer-bit)

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *tex*)
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d *tex2*)

  (gl:use-program *shader-prog*)
  (gl:uniformi (gl:get-uniform-location *shader-prog* "ourTexture") 0)
  (gl:uniformi (gl:get-uniform-location *shader-prog* "ourTexture2") 1)

  (gl:uniformf (gl:get-uniform-location *shader-prog* "texMix") *tex-mix*)

  ;; Update the Uniform Color
  (let* ((time-value (glfw:get-time))
         (value (/ (+ 0.5 (sin time-value))
                        (+ 2.0 0.5)))
         (vert-color-loc (gl:get-uniform-location *shader-prog* "myColor")))
    (gl:uniformf vert-color-loc value value value 1.0))

  ;; Bring back state for model
  (gl:bind-vertex-array *vao*)

  ;; Draw 0th Vertex attribute in VAO; 3 verts
  ;; (gl:draw-arrays :triangles 0 3)
  (gl:draw-elements :triangles *gl-ebo*)

  (gl:bind-vertex-array 0)

  ;; 30 FPS; TODO: Need Full Game Loop!!!
  (sleep (/ 1 60)))

(defun hello-textures ()
  ;; OSX needs draw calls to be on the main thread
  ;; (tmt:with-body-in-main-thread ()
  (glfw:with-init-window (:title "P3 - Hello Shaders"
                           :width *win-width*
                           :height *win-height*
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-profile :opengl-core-profile)
    (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
    (glfw:set-window-size-callback 'resize-viewport)
    (glfw:set-key-callback 'handle-keys)
    (glfw:set-input-mode :sticky-keys 1) ;; Don't miss an event

    (setup-model)
    (setup-textures)
    (setup-shaders)

    (gl:clear-color 0.4 0.7 0.7 1.0)
    (gl:viewport 0 0 *win-width* *win-height*)

    (loop :until (glfw:window-should-close-p) :do
      (livesupport:continuable
        (livesupport:update-repl-link)
        (set-tex-mix)
        (render)
        (glfw:swap-buffers)
        (glfw:poll-events)))

    (clean-up))) ;)
