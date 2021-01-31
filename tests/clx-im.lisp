(defpackage clx-im/tests/clx-im
  (:use :cl
        :clx-im
        :rove))
(in-package :clx-im/tests/clx-im)

;; NOTE: To run this test file, execute `(asdf:test-system :clx-im)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
