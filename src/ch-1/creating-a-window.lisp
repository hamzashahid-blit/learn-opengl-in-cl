(defpackage #:cl-learn-gl.1.1
  (:use :cl))
(in-package #:cl-learn-gl.1.1)

;;;; using Legacy OpenGL for quick setup (this is the last legacy)

(defparameter *window-width* 600)
(defparameter *window-height* 400)

(glfw:def-window-size-callback resize-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((r (/ height width)))
    (gl:ortho -1 1 (- r) r -1 1))

  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun render ()
  (gl:clear :color-buffer)
  ;; (gl:with-pushed-matrix
  ;;   (gl:color .2 .8 .8)
  ;;   (gl:rect -.25 -.25 .25 .25))
  )

(defun creating-a-window ()
  ;; TMT needed for OSX which requires draw calls on main thread
  (tmt:with-body-in-main-thread ()
    (glfw:with-init-window (:title "P1 - Creating A Window"
                            :width *window-width*
                            :height *window-height*)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

      (glfw:set-window-size-callback 'resize-viewport)

      (gl:clear-color .2 .3 .3 1)
      (set-viewport *window-width* *window-height*)
      (loop :until (glfw:window-should-close-p) :do
        (render)
        (glfw:swap-buffers)
        (glfw:poll-events)))))
