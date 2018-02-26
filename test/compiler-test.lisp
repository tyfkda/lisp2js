(load "./test-util.lisp")

;;;; Test for Compiler

(run-test
 (test "get-receiver with dot" 'foo (get-receiver 'foo.bar.baz))
 (test "get-receiver without dot" 'foo (get-receiver 'foo))

 (test "escape-char[A]" "A" (escape-char "A"))
 (test "escape-char[\\]" "\\\\" (escape-char "\\"))
 (test "escape-char[\"]" "\\\"" (escape-char "\""))

 (test "escape-sym-char[-]" "$2d" (escape-sym-char "-"))
 )
