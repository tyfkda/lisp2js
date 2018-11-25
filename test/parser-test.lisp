(load "./test-util.lisp")

(run-test
 (let1 scope (create-scope nil '(x y &rest z))
   (test "local-var?[x]" t (local-var? scope 'x))
   (test "local-var?[no]" nil (local-var? scope 'no))
   (test "local-var?[rest]" t (local-var? scope 'z))
   (test "local-var?[&rest]" nil (local-var? scope '&rest))

   (test "special-var?[x]" nil (special-var? scope 'x))
   (test "special-var?[this]" t (not (not (special-var? scope 'this))))
   )

 (test "get-receiver with dot" 'foo (get-receiver 'foo.bar.baz))
 (test "get-receiver without dot" 'foo (get-receiver 'foo))

;;;; Test for Parse

 (let1 scope (create-scope nil '(lvar))
   (test "parse* const" #(:CONST 123) (parse* 123 scope))
   (test "parse* g-ref" #(:REF x) (parse* 'x scope))
   (test "parse* l-ref" #(:REF lvar) (parse* 'lvar scope))
   (test "parse* if" #(:IF #(:REF x) #(:REF y) #(:CONST 3)) (parse* '(if x y 3) scope))
   (test "parse* vector"
         #(:FUNCALL #(:REF vector) (#(:CONST 1) #(:FUNCALL #(:REF list) (#(:CONST quote) #(:CONST a)))))
         (parse* #(1 'a) scope))
   )
 )
