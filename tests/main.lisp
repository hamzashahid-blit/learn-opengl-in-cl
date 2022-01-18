(defpackage cl-learn-gl/tests/main
  (:use :cl
        :cl-learn-gl
        :rove))
(in-package :cl-learn-gl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-learn-gl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
