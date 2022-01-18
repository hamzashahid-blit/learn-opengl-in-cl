(asdf:defsystem "cl-learn-gl"
  :version "0.1.0"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("cl-opengl"
               "cl-glfw3"
               "alexandria"
               "cl-soil"
               "rtg-math"
               "trivial-main-thread"
               "livesupport")
  :components ((:module "src"
                :components
                ((:module "ch-1"
                  :components
                  ((:file "creating-a-window")
                   (:file "hello-triangle")
                   (:file "shaders")
                   (:file "textures")
                   (:file "transforms"))))))
  :description "learnopengl.com in Common Lisp"
  :in-order-to ((asdf:test-op (asdf:test-op "cl-learn-gl/tests"))))

(asdf:defsystem "cl-learn-gl/tests"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("cl-learn-gl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-learn-gl"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))
