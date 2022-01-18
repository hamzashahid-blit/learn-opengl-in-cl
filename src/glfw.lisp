(defpackage #:cl-learn-gl.glfw
  (:local-nicknames (#:a #:alexandria)))
(in-package #:cl-learn-gl.glfw)

;;;; Shows cl-glfw3 with Legacy OpenGL for emphasis on GLFW

(defparameter +y-pos+ 0)
(defparameter +x-pos+ 0)
(defparameter +keys-pressed+ nil)

(glfw:def-key-callback handle-keys (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (if (not (eq action :release))
    (pushnew key +keys-pressed+)
    (a:deletef +keys-pressed+ key)))

(defun move ()
  (map nil
    #'(lambda (key)
        (case key
          (:w (incf +y-pos+ 0.01))
          (:a (decf +x-pos+ 0.01))
          (:s (decf +y-pos+ 0.01))
          (:d (incf +x-pos+ 0.01))

          (:escape (glfw:set-window-should-close))))
    +keys-pressed+))

(defun render ()
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 0 .8 .8)
    (gl:rect (+ -0.25 +x-pos+) (+ -0.25 +y-pos+)
             (+  0.25 +x-pos+) (+  0.25 +y-pos+))))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; So a square isn't stretched, R is aspect ratio like 9/16
  (let ((r (/ height width)))
    (gl:ortho -1 1 (- r) r -1 1))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun close-app ()
  (setf +x-pos+ 0)
  (setf +y-pos+ 0)
  (setf +keys-pressed+ nil))

(defun basic-window-example ()
  ;; Graphics calls on OS X must occur in the main thread
  (tmt:with-body-in-main-thread ()
    (glfw:with-init-window (:title "Window test" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

      (glfw:set-key-callback 'handle-keys)
      (glfw:set-input-mode :sticky-keys 1) ;; Don't miss an event
      (glfw:set-window-size-callback 'update-viewport)

      (gl:clear-color .2 .2 .2 .2)
      (set-viewport 600 400)

      (loop :until (glfw:window-should-close-p) :do
        (move)
        (render)
        (glfw:swap-buffers)
        (glfw:poll-events))

      (close-app))))

