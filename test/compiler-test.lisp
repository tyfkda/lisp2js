(load "./test.lisp")

(test "get-receiver with dot" 'foo (get-receiver 'foo.bar.baz))
(test "get-receiver without dot" 'foo (get-receiver 'foo))

(let1 scope (create-scope nil '(x y &rest z))
  (test "local-var?[x]" t (local-var? 'x scope))
  (test "local-var?[no]" nil (local-var? 'no scope))
  (test "local-var?[rest]" t (local-var? 'z scope))
  (test "local-var?[&rest]" nil (local-var? '&rest scope))
  )

(test "escape-char[A]" "A" (escape-char "A"))
(test "escape-char[\\]" "\\\\" (escape-char "\\"))
(test "escape-char[\"]" "\\\"" (escape-char "\""))

(test "escape-sym-char[-]" "$2d" (escape-sym-char "-"))
