(defpackage #:cl-learn-gl.1.5
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:c #:cffi)
                    (#:rm #:rtg-math)
                    (#:rm4 #:rtg-math.matrix4)
                    (#:rmv3 #:rtg-math.vector3)))
(in-package #:cl-learn-gl.1.5)

;;;; This file includes the last 4 Lessons in the Getting Started Chapter (1)
;;;; Move around with WASD, Space, Shift and the mouse movement and Scrolling (zoom)

;;; TODO: Seperate shaders into a class

(defparameter *win-width* 800)
(defparameter *win-height* 600)
;; (defparameter *win-x* 0)
;; (defparameter *win-y* 0)

(defparameter *tri-verts*
  #(-0.5 -0.5 0.0
     0.5 -0.5 0.0
     0.0  0.5 0.0))

(defparameter *verts2*
  ;; ;; Positions     Colors       Tex Coords
  ;; #(-0.5 -0.5 0.0  1.0 0.0 0.0  0.0 0.0     ; lower-left
  ;;    0.5 -0.5 0.0  0.0 1.0 0.0  1.0 0.0     ; lower-right
  ;;    0.0  0.5 0.0  0.0 0.0 1.0  0.5 1.0))   ; top-center

  ;; positions      Colors        Tex Coords
  #( 0.5  0.5 0.0   1.0 0.0 0.0   1.0 0.0    ; top right
     0.5 -0.5 0.0   0.0 1.0 0.0   1.0 1.0    ; bottom right
    -0.5 -0.5 0.0   0.0 0.0 1.0   0.0 1.0    ; bottom left
    -0.5  0.5 0.0   1.0 1.0 0.0   0.0 0.0))  ; top left

(defparameter *verts*
  #(-0.5 -0.5 -0.5  0.0 0.0
     0.5 -0.5 -0.5  1.0 0.0
     0.5  0.5 -0.5  1.0 1.0
     0.5  0.5 -0.5  1.0 1.0
    -0.5  0.5 -0.5  0.0 1.0
    -0.5 -0.5 -0.5  0.0 0.0

    -0.5 -0.5  0.5  0.0 0.0
     0.5 -0.5  0.5  1.0 0.0
     0.5  0.5  0.5  1.0 1.0
     0.5  0.5  0.5  1.0 1.0
    -0.5  0.5  0.5  0.0 1.0
    -0.5 -0.5  0.5  0.0 0.0

    -0.5  0.5  0.5  1.0 0.0
    -0.5  0.5 -0.5  1.0 1.0
    -0.5 -0.5 -0.5  0.0 1.0
    -0.5 -0.5 -0.5  0.0 1.0
    -0.5 -0.5  0.5  0.0 0.0
    -0.5  0.5  0.5  1.0 0.0

     0.5  0.5  0.5  1.0 0.0
     0.5  0.5 -0.5  1.0 1.0
     0.5 -0.5 -0.5  0.0 1.0
     0.5 -0.5 -0.5  0.0 1.0
     0.5 -0.5  0.5  0.0 0.0
     0.5  0.5  0.5  1.0 0.0

    -0.5 -0.5 -0.5  0.0 1.0
     0.5 -0.5 -0.5  1.0 1.0
     0.5 -0.5  0.5  1.0 0.0
     0.5 -0.5  0.5  1.0 0.0
    -0.5 -0.5  0.5  0.0 0.0
    -0.5 -0.5 -0.5  0.0 1.0

    -0.5  0.5 -0.5  0.0 1.0
     0.5  0.5 -0.5  1.0 1.0
     0.5  0.5  0.5  1.0 0.0
     0.5  0.5  0.5  1.0 0.0
     
    -0.5  0.5  0.5  0.0 0.0
    -0.5  0.5 -0.5  0.0 1.0))

(defparameter *cube-pos*
  (list (rm:v3!  0.0  0.0  0.0)
        (rm:v3!  2.0  5.0 -15.0)
        (rm:v3! -1.5 -2.2 -2.5)
        (rm:v3! -3.8 -2.0 -12.3)
        (rm:v3!  2.4 -0.4 -3.5)
        (rm:v3! -1.7  3.0 -7.5)
        (rm:v3!  1.3 -2.0 -2.5)
        (rm:v3!  1.5  2.0 -2.5)
        (rm:v3!  1.5  0.2 -1.5)
        (rm:v3! -1.3  1.0 -1.5)))

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

;;; Camera
(defparameter *cam-pitch* 0.0f0)
(defparameter *cam-yaw* -90.0f0)

(defparameter *cam-direction*
  ;; (rm:v3!
  ;;   (* (cos (rm:radians *cam-yaw*)) (cos (rm:radians *cam-pitch*)))
  ;;   (sin (rm:radians *cam-pitch*))
  ;;   (* (sin (rm:radians *cam-yaw*)) (cos (rm:radians *cam-pitch*))))
  (rm:v3! 0 0 0))

(defparameter *cam-pos*   (rm:v3! 0 0 3))
(defparameter *cam-front* (rmv3:normalize *cam-direction*)) ;(rm:v3! 0 0 -1)
(defparameter *cam-up*    (rm:v3! 0 1 0))

;;; GLFW
(glfw:def-window-size-callback resize-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h)
  (setf *win-width* w)
  (setf *win-height* h))

(defparameter *last-mouse-x* (/ *win-width* 2))
(defparameter *last-mouse-y* (/ *win-width* 2))
(defparameter *mouse-x-offset* 0.0)
(defparameter *mouse-y-offset* 0.0)
(defparameter *mouse-sensitivity* 0.15)
(defparameter *first-mouse-move* t)

(glfw:def-cursor-pos-callback mouse-move (window x y)
  (declare (ignore window))

  (let ((xpos (coerce x 'single-float))
        (ypos (coerce y 'single-float)))

    ;; Prevent camera jump from mouse teleport to center of window
    (when *first-mouse-move*
      (setf *last-mouse-x* xpos)
      (setf *last-mouse-y* ypos)
      (setf *first-mouse-move* nil))

    ;; y-offset reversed since mouse y-coords range from bottom to top
    (setf *mouse-x-offset* (- xpos *last-mouse-x*))
    (setf *mouse-y-offset* (- *last-mouse-y* ypos)) 

    (setf *last-mouse-x* xpos)
    (setf *last-mouse-y* ypos)

    ;; TODO: Think; There is a point in multiplying later (I think)
    (setf *mouse-x-offset* (* *mouse-sensitivity* *mouse-x-offset*))
    (setf *mouse-y-offset* (* *mouse-sensitivity* *mouse-y-offset*))

    (incf *cam-yaw* *mouse-x-offset*)
    (incf *cam-pitch* *mouse-y-offset*)

    ;; To prevent LookAt matrix Flipping
    (cond
      ((> *cam-pitch* 89.0) (setf *cam-pitch* 89.0))
      ((< *cam-pitch* -89.0) (setf *cam-pitch* -89.0)))

    ;; (pprint (list *mouse-x-offset* *mouse-y-offset*))
    ;; (pprint (list *cam-yaw* *cam-pitch*))

    (setf *cam-direction*
      (rm:v3!
        (* (cos (rm:radians *cam-yaw*)) (cos (rm:radians *cam-pitch*)))
        (sin (rm:radians *cam-pitch*))
        (* (sin (rm:radians *cam-yaw*)) (cos (rm:radians *cam-pitch*)))))

    ;; (rm:v3! 0 0 -1)
    (setf *cam-front* (rmv3:normalize *cam-direction*))))

(defparameter *fov* 70.0f0)

(glfw:def-scroll-callback zoom (window x-offset y-offset)
  (declare (ignore window x-offset))
  (let ((y-off (coerce y-offset 'single-float)))
    (decf *fov* y-off)
    (setf *mouse-sensitivity* (* 0.5 (/ (- *fov* 1.0) (- 70.0 1.0)))) ; Normalization formula

    (cond
      ((< *fov* 1.2f0) (setf *fov* 1.2f0))
      ((> *fov* 70.0f0) (setf *fov* 70.0f0)))))

(defparameter *delta-time* 0.0)
(defparameter *last-frame* 0.0)

(defparameter *keys-pressed* nil)

;; (defmacro on-key-press (&rest clauses)
;;   `(cond
;;     ,@(loop :for clause :in clauses :collect
;;         `((and (eq action :press) (eq key ,(car clause))) ,(cadr clause)))))

(glfw:def-key-callback handle-keys (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (if (not (eq action :release))
    (pushnew key *keys-pressed*)
    (a:deletef *keys-pressed* key))

  ;; (cond
  ;;   ((and (eq action :press) (eq key :f11)) (fullscreen))
  ;;   ((and (eq action :press) (eq key :escape)) (escape)))

  (macrolet ((on-key-press (&rest clauses)
               `(cond ,@(loop :for clause :in clauses :collect
                          `((and (eq action :press) (eq key ,(car clause)))
                             ,(cadr clause))))))

    (on-key-press
      (:escape (glfw:set-window-should-close))
      (:q (toggle-pointer))))

  (pprint *keys-pressed*))

(defun move-camera ()
  (let ((speed (rm:v3! (* 3 *delta-time*)))
        (cam-right (rmv3:normalize (rmv3:cross *cam-front* *cam-up*))))

    (map nil
      #'(lambda (key)
          (case key
            (:w (rmv3:incf *cam-pos* (rmv3:* *cam-front* speed)))
            (:s (rmv3:decf *cam-pos* (rmv3:* *cam-front* speed)))
            (:a (rmv3:decf *cam-pos* (rmv3:* cam-right speed)))
            (:d (rmv3:incf *cam-pos* (rmv3:* cam-right speed)))

            (:space (rmv3:incf *cam-pos* (rmv3:* *cam-up* speed)))
            (:left-shift (rmv3:decf *cam-pos* (rmv3:* *cam-up* speed)))))
      *keys-pressed*)))

(defun toggle-pointer ()
  (if (eq (glfw:get-input-mode :cursor) :disabled)
    (glfw:set-input-mode :cursor :normal)
    (glfw:set-input-mode :cursor :disabled))

  (setf *first-mouse-move* t))

;; TODO: Can't get fullscreen to work
;; (defparameter *fullscreen?* nil)
;; (defparameter *old-win-size* (list *win-width* *win-height*))

;; (glfw:def-key-callback fullscreen (window key scancode action mod-keys)
;;   (declare (ignore window scancode mod-keys))
;;   (when (and (eq action :press) (eq key :f11))
;;     (pprint "askdjflas")))

;; (defun fullscreen ()
;;   (let* ((monitor (glfw:get-primary-monitor))
;;          (screen-size (cddr (glfw:get-monitor-work-area monitor)))
;;          (old-width (car *old-win-size*))
;;          (old-height (cadr *old-win-size*))
;;          (width (car screen-size))
;;          (height (cadr screen-size)))

;;     (if (not *fullscreen?*)
;;       (progn
;;         (setf *old-win-size* (glfw:get-window-size))
;;         ;; (%glfw:window-hint :decorated 0)
;;         (glfw:set-window-monitor monitor 1920 1080)
;;         (glfw:set-window-size width height)
;;         (setf *win-width* width)
;;         (setf *win-height* height)
;;         (setf *fullscreen?* t))
;;       (progn
;;         ;; (%glfw:window-hint :decorated 1)
;;         (glfw:set-window-monitor nil old-width old-height)
;;         (glfw:set-window-size old-width old-height)
;;         (setf *win-width* old-width)
;;         (setf *win-height* old-height)
;;         (setf *fullscreen?* nil)))))

;;; Shaders

;; TODO: Fix Pathnames depending on Current Directory
(defparameter *vertex-shader-source*
  (a:read-file-into-string
    (rel-path #p"src/learn-opengl/transform.vert")))

(defparameter *fragment-shader-source*
  (a:read-file-into-string
    (rel-path #p"src/learn-opengl/transform.frag")))

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
;; DONE: Seperate texture loading to another function
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



  ;; (setf *ebo* (gl:gen-buffer))
  ;; (gl:bind-buffer :element-array-buffer *ebo*)

  ;; ;; Allocate a GL Array and store vertices in it
  ;; (setf *gl-ebo* (gl:alloc-gl-array :uint (length *indices*)))
  ;; (loop :for index :across *indices*
  ;;   :for i :from 0 :do
  ;;   (setf (gl:glaref *gl-ebo* i) index))

  ;; (gl:buffer-data :element-array-buffer :static-draw *gl-ebo*)


  ;; Tell OpenGL how to interpret vertex data per vertex attribute
  (gl:vertex-attrib-pointer 0 3 :float nil
    (* (c:foreign-type-size :float) 5) 0)

  ;; Enable "aPos" vertex attribute at location 0 in Vertex Shader 
  (gl:enable-vertex-attrib-array 0)

  (gl:vertex-attrib-pointer 1 2 :float nil
    (* (c:foreign-type-size :float) 5)
    (* (c:foreign-type-size :float) 3))

  (gl:enable-vertex-attrib-array 1)

  (gl:bind-buffer :array-buffer 0)
  ;; (gl:bind-buffer :element-array-buffer 0)
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
  ;; (gl:delete-buffers (list *ebo*))
  ;; (setf *ebo* nil)

  ;; gl-EBO
  ;; (gl:free-gl-array *gl-ebo*)
  ;; (setf *gl-ebo* nil)

  (gl:delete-texture *tex*)
  (setf *tex* nil)

  ;; Shader Program
  (gl:delete-program *shader-prog*)
  (setf *shader-prog* -1)

  (setf *cam-pos* (rm:v3! 0 0 3))
  (setf *cam-direction* (rm:v3! 0 0 0))

  (setf *mouse-sensitivity* 0.15)
  (setf *fov* 70.0)

  (setf *keys-pressed* nil)

  (return-from clean-up 0))

(defun render ()
  (gl:clear :depth-buffer-bit :color-buffer-bit)

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *tex*)
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d *tex2*)

  (gl:use-program *shader-prog*)

  ;; (setf view 
  ;;   (rm4:transpose
  ;;     (rtg-math.matrix4:make
  ;;       1.0 0.0 0.0  0.0
  ;;       0.0 1.0 0.0  0.0
  ;;       0.0 0.0 1.0 -3.0
  ;;       0.0 0.0 0.0  1.0)))

  ;; IMPORTANT: Order of Mat4 Multiplication Matters!!!!!!
  (let* (;; (model (rm4:* (rm4:translation (rm:v3! 0 0 -2))
         ;;               (rm4:rotation-from-axis-angle
         ;;                 (rm:v3! -1.0 -1.0 0.0)
         ;;                 (coerce (glfw:get-time) 'single-float))))

         ;; (radius 10.0)
         ;; (cam-x (* (sin (glfw:get-time)) radius))
         ;; (cam-z (* (cos (glfw:get-time)) radius))

         ;; (view ;(rm4:translation (rm:v3! 0.0 0.0 -3.0))
         ;;   (rtg-math.matrix4:look-at 
		 ;; 	  (rm:v3! 0 1 0)
		 ;; 	  (rm:v3! cam-x 0 cam-z)
		 ;; 	  (rm:v3! 0 0 0)))

         (view (rm4:look-at
                 *cam-up* *cam-pos* (rmv3:+ *cam-pos* *cam-front*))))

    ;; IMPORTANT: NOTE THE NIL AT THE END!!!
    ;; (gl:uniform-matrix-4fv (gl:get-uniform-location *shader-prog* "model") model nil)
    (gl:uniform-matrix-4fv (gl:get-uniform-location *shader-prog* "view") view nil))
    (gl:uniform-matrix-4fv (gl:get-uniform-location *shader-prog* "projection")
      (rtg-math.projection:perspective
        (float *win-width*) (float *win-height*)
        0.1 100.0 *fov*)
      nil)


  ;; Bring back state for model

  (gl:bind-vertex-array *vao*)

  (loop :for pos :in *cube-pos*
    :for i :from 0 :do
    (let* ((angle (* 20 i))
            (model (rm4:*
                     (rm4:translation pos)
                     (rm4:rotation-from-axis-angle
                       (rm:v3! 0 0 1.3)
                       (rm:radians angle))
                     (rm4:rotation-from-axis-angle
                       (rm:v3! -1.0 -1.0 0.0)
                       (coerce (glfw:get-time) 'single-float)))))

      (gl:uniform-matrix-4fv (gl:get-uniform-location *shader-prog* "model") model nil)

      ;; Draw 0th Vertex attribute in VAO; 3 verts
      (gl:draw-arrays :triangles 0 36)))

  (gl:bind-vertex-array 0)

  (sleep (/ 1 60)))

(defun update ()
  (let ((current-frame (glfw:get-time)))
    (setf *delta-time* (- current-frame *last-frame*))
    (setf *last-frame* current-frame))

  ;; Frame time
  ;; (pprint *delta-time*)

  (move-camera))

;; TODO: Set the default window pos
(defun hello-transform ()
  ;; OSX needs draw calls to be on the main thread
  ;tmt:with-body-in-main-thread ()
  (glfw:with-init-window (:title "P4 - Hello Transforms"
                           :width *win-width*
                           :height *win-height*
                           :context-version-major 3
                           :context-version-minor 3
                           :opengl-profile :opengl-core-profile)
    (livesupport:continuable
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
      (glfw:set-window-size-callback 'resize-viewport)
      (glfw:set-key-callback 'handle-keys)
      (glfw:set-cursor-position-callback 'mouse-move)
      (glfw:set-scroll-callback 'zoom)
      (glfw:set-input-mode :sticky-keys 1) ;; Don't miss an event
      (glfw:set-input-mode :cursor :disabled)

      (gl:enable :depth-test)

      (setup-model)
      (setup-textures)
      (setup-shaders)

      (gl:clear-color 0.4 0.7 0.7 1.0)
      (gl:viewport 0 0 *win-width* *win-height*)
      (gl:enable :depth-test)
      
      (gl:use-program *shader-prog*)
      (gl:uniformi (gl:get-uniform-location *shader-prog* "ourTexture") 0)
      (gl:uniformi (gl:get-uniform-location *shader-prog* "ourTexture2") 1)

      ;; No need to do per frame but we do because of zoom scroll-callback!
      ;; (gl:uniform-matrix-4fv (gl:get-uniform-location *shader-prog* "projection")
      ;;   (rtg-math.projection:perspective
      ;;     (float *win-width*) (float *win-height*)
      ;;     0.1 100.0 (rm:radians *fov*))
      ;;   nil)

      ;; (glfw:set-window-monitor (glfw:get-primary-monitor) 1920 1080)
      ;; (glfw:show-window)

      (loop :until (glfw:window-should-close-p) :do
        (livesupport:update-repl-link)
        (update)
        (render)
        (glfw:swap-buffers)
        (glfw:poll-events)))

    (clean-up)))
