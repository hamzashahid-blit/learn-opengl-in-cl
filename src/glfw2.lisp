(defpackage #:cl-learn-gl.glfw2
  (:use :cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package #:cl-learn-gl.glfw2)

;;;; Shows cl-glfw3 with Legacy OpenGL for emphasis on GLFW

(defvar *shader-prog* -1)
(defvar *frag-shader* nil)
(defvar *vert-shader* nil)
(defvar *shader-time* 0)

(define-condition compile-error (error)
  ((message
     :initform nil
     :initarg :message
     :reader compile-error-message
     :documentation "The reason given for this error")))

(defparameter +vertex-shader-source+ "
// Input our our time variable
uniform float time;
// Pass to frag shader
varying vec2 vUv;
attribute vec4 vert;
void main()
{
    vUv = vert.xy;
    vec4 offsets = vec4(cos(time), sin(time), 1., 1.);
    gl_Position = gl_ModelViewProjectionMatrix * vert * offsets;
}
")

(defparameter +fragment-shader-source+ "
varying vec2 vUv;
void main()
{
    gl_FragColor = vec4(vUv.x, vUv.y, 1., 1.);
}
")

(defun render ()
  (gl:clear :color-buffer)

  ;; Update our time variable in the shader
  (gl:uniformf
    (gl:get-uniform-location *shader-prog* "time")
    (incf *shader-time* 0.01))

  (gl:with-pushed-matrix
    ;; Draw a box far back from the camera
    (gl:translate 0 0 -800)
    (gl:rect -25 -25 25 25)))

(defun check-shader-error (shader)
    "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      ;; Print to console & then throw error
      (progn
        (format t "~A~%" error-string)
        (error
          'compile-error
          :message error-string)))))

(defun invalid-shader? (shader)
  (= shader -1))

(defun setup-shader ()
  ;; Keep trying to load our shader
  ;; (Allow user to fix compile errors)
  (loop :while (invalid-shader? *shader-prog*) :do
    (with-simple-restart
      (retry "Retry compiling shaders.")

      ;; Create the OpenGL shader in memory
      (setf *vert-shader* (gl:create-shader :vertex-shader))
      (setf *frag-shader* (gl:create-shader :fragment-shader))

      ;; Copy our shader source to the OpenGL shader
      (gl:shader-source *vert-shader* +vertex-shader-source+)
      (gl:shader-source *frag-shader* +fragment-shader-source+)

      ;; Compile our shader sources into GPU bytecode
      (gl:compile-shader *vert-shader*)
      (gl:compile-shader *frag-shader*)

      (check-shader-error *vert-shader*)
      (check-shader-error *frag-shader*)

      ;; Create the program which controls the shader activation
      (setf *shader-prog* (gl:create-program))

      ;; Then add our shaders to that program
      ;; The same shader can be attached to different programs
      (gl:attach-shader *shader-prog* *vert-shader*)
      (gl:attach-shader *shader-prog* *frag-shader*)

      ;; Linking our shader puts everything together
      (gl:link-program *shader-prog*)

      ;; We'll now draw with this shader in the future
      (gl:use-program *shader-prog*))))

;; Standard window setup below this line
;; -------------------------------------

(glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(defun set-viewport (width height)
  (gl:clear-color 0.2 0.2 0.2 0.2)

  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)

  ;; We'll be using a perspective projection
  ;; R is the ratio. In 1920x1080, it is 9/16
  (let ((r (/ height width)))
    (gl:frustum -1 1 (- r) r 9 50000))

  (gl:matrix-mode :modelview)
  (gl:load-identity))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun fragment-shader-example ()
  ;; Graphics calls on OSX must occur in the main thread
  (tmt:with-body-in-main-thread ()
    (glfw:with-init-window (:title "Blit Kit" :width 600 :height 400)
      (glfw:set-key-callback 'quit-on-escape)

      ;; Callback for window resize events
      (glfw:set-window-size-callback 'update-viewport)
      (set-viewport 800 400)

      ;; Compile our shaders and use the program
      (setup-shader)

      ;; Our render-loop
      (loop :until (glfw:window-should-close-p)
        :do (render)
        :do (glfw:swap-buffers)
        :do (glfw:poll-events))))
  (tmt:stop-main-runner))
