(load "./test.lisp")

(let1 scope (create-scope nil '(x y &rest z))
  (test "local-var?[x]" t (local-var? scope 'x))
  (test "local-var?[no]" nil (local-var? scope 'no))
  (test "local-var?[rest]" t (local-var? scope 'z))
  (test "local-var?[&rest]" nil (local-var? scope '&rest))
  )

(test "get-receiver with dot" 'foo (get-receiver 'foo.bar.baz))
(test "get-receiver without dot" 'foo (get-receiver 'foo))

(test "escape-char[A]" "A" (escape-char "A"))
(test "escape-char[\\]" "\\\\" (escape-char "\\"))
(test "escape-char[\"]" "\\\"" (escape-char "\""))

(test "escape-sym-char[-]" "$2d" (escape-sym-char "-"))
