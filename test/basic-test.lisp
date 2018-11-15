(load "./test-util.lisp")

(run-test
 ;; not
 (test "not-nil" t (not nil))
 (test "not-t" nil (not t))

 (test "progn" 3 (progn 1 2 3))
 (test "single-progn" 1 (progn 1))
 (test "empty-progn" () (progn))

 ;; nil
 (test "nil isn't symbol" nil (symbol? nil))
 (test "nil is null" t (null? nil))
 (test "not nil is t" t (not nil))
 (test "nil isn't a pair" nil (pair? nil))
 (test "car of nil is nil" nil (car nil))
 (test "cdr of nil is nil" nil (cdr nil))

 ;; symbol
 (test "t isn't a symbol" nil (symbol? t))
 (test "others are symbol" t (symbol? 'others))

 ;; cons
 (test "cons" '(1 . 2) (cons 1 2))
 (test "car" 1 (car '(1 . 2)))
 (test "cdr" 2 (cdr '(1 . 2)))
 (test "car-non-cons" 123 (car 123))
 (test "cdr-non-cons" nil (cdr 123))

 ;; pair
 (test "list is a pair" t (pair? (list 1 2 3)))

 (test "proper-list?" t (proper-list? (list 1 2 3)))
 (test "proper-list?" nil (proper-list? (list* 1 2 3)))
 (test "nil isnt proper-list" nil (proper-list? nil))
 (test "abbrev" "'a" (x->string ''a))

 ;; number
(test "number->string" "123" (number->string 123))
(test "number->char" "A" (number->char 65))

 ;; string
(test "char->number" 65 (char->number "A"))
(test "string->number" 123 (string->number "123"))
(test "string->number (float)" 123.45 (string->number "123.45"))
(test "string->number (hex)" 291 (string->number "123" 16))

 ;; type
 (test "type of nil" 'bool (type nil))
 (test "type of t" 'bool (type t))
 (test "type of int" 'number (type 123))
 (test "type of float" 'number (type 1.23))
 (test "type of pair" 'pair (type '(1 2 3)))
 (test "type of string" 'string (type "123"))
 (test "type of vector" 'vector (type #(1 2 3)))
 (test "type of table" 'table (type (make-hash-table)))
 (test "type of lambda" 'function (type (lambda () 123)))

 ;; lambda
 (test "lambda" 123 ((lambda () 123)))
 (test "empty-lambda" nil ((lambda ())))

 ;; equal?
 (test "eq? string" t (eq? "123" "123"))
 (test "eq? vector" nil (eq? #(1 2 3) #(1 2 3)))
 (test "equal? vector" t (equal? #(1 2 3) #(1 2 3)))

 ;; length
 (test "length" 3 (length '(1 2 3)))
 (test "length-dotted" 2 (length '(1 2 . 3)))

 ;; last-pair
 (test "last-pair" '(3) (last-pair '(1 2 3)))
 (test "last-pair-dotted" '(2 . 3) (last-pair '(1 2 . 3)))
 (test "last-pair-atom" 123 (last-pair 123))

 ;; vector
 (test "vector->list" '(1 2 3) (vector->list #(1 2 3)))
 (test "empty vector->list" '() (vector->list #()))

 ;; hash-table
 (test "hash-table-put!-get" 123 (let1 h (make-hash-table)
                                   (hash-table-put! h 'x 123)
                                   (hash-table-get h 'x)))
 (test "hash-table-exists?" t (let1 h (make-hash-table)
                                (hash-table-put! h 'x 123)
                                (hash-table-exists? h 'x)))
 (test "hash-table-exists? not" nil (let1 h (make-hash-table)
                                    (hash-table-exists? h 'x)))
 (test "hash-table-put! dot" 123 (let1 h (make-hash-table)
                                   (set! h.x 123)
                                   (hash-table-get h 'x)))
 (test "hash-table-get dot" 123 (let1 h (make-hash-table)
                                  (hash-table-put! h 'x 123)
                                  h.x))
 (test "hash-table-put! dot2" 123 (let1 h (make-hash-table)
                                    (set! h.x- 123)
                                    (hash-table-get h 'x-)))
 (test "hash-table-get dot2" 123 (let1 h (make-hash-table)
                                   (hash-table-put! h 'x- 123)
                                   h.x-))

 ;; arithmetic operations
 (test "+" 6 (+ 1 2 3))
 (test "overwrite-inlined-function" '(1 2 3) (let ((+ list)) (+ 1 2 3)))

 ;; eval
 (test "simple-eval" 6 (eval '(+ 1 2 3)))
 (test "eval-can't-see-local" 1 (progn (def x 1)
                                       (let ((x 2))
                                         (eval 'x))))

 ;; macro
 (defmacro foo (x) `(+ ,x 1))
 (test "local overcome macro" 222 (progn (let ((foo (lambda (x) (* x 2))))
                                           (foo 111))))
 )
