(load "./test-util.lisp")

(run-test
 (let1 scope (create-scope nil '(x y &rest z))
   (test "local-var?[x]" t (local-var? scope 'x))
   (test "local-var?[no]" nil (local-var? scope 'no))
   (test "local-var?[rest]" t (local-var? scope 'z))
   (test "local-var?[&rest]" nil (local-var? scope '&rest))
   )

;;;; Test for Traverse

 (let1 scope (create-scope nil '(lvar))
   (test "traverse* const" #(:CONST 123) (traverse* 123 scope))
   (test "traverse* g-ref" #(:REF x) (traverse* 'x scope))
   (test "traverse* l-ref" #(:REF lvar) (traverse* 'lvar scope))
   (test "traverse* if" #(:IF #(:REF x) #(:REF y) #(:CONST 3)) (traverse* '(if x y 3) scope))
   (test "traverse* vector"
         #(:FUNCALL #(:REF vector) (#(:CONST 1) #(:FUNCALL #(:REF list) (#(:CONST quote) #(:CONST a)))))
         (traverse* #(1 'a) scope))
   )

;;;; Test for Compiler

 (test "get-receiver with dot" 'foo (get-receiver 'foo.bar.baz))
 (test "get-receiver without dot" 'foo (get-receiver 'foo))

 (test "escape-char[A]" "A" (escape-char "A"))
 (test "escape-char[\\]" "\\\\" (escape-char "\\"))
 (test "escape-char[\"]" "\\\"" (escape-char "\""))

 (test "escape-sym-char[-]" "$2d" (escape-sym-char "-"))
 )
