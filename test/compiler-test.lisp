(load "./test-util.lisp")

;;;; Test for Compiler

(run-test
 (test "escape-char[A]" "A" (escape-char "A"))
 (test "escape-char[\\]" "\\\\" (escape-char "\\"))
 (test "escape-char[\"]" "\\\"" (escape-char "\""))

 (test "escape-sym-char[-]" "$2d" (escape-sym-char "-"))
 )
