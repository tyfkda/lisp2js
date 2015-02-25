(defmacro test (name expect actual)
  `(do (print (string-append ,name
                             " ... "))
       (let ((expect-value ,expect)
             (actual-value ,actual))
         (if (equal? expect-value actual-value)
             (print "ok\n")
           (do (print (string-append "ERROR [" expect-value "] expected, but [" actual-value "]\n"))
               (exit 1))))))

;; not
(test "not-nil" t (not nil))
(test "not-t" nil (not t))

(test "do" 3 (do 1 2 3))
(test "single-do" 1 (do 1))
(test "empty-do" () (do))

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

;; type
(test "type of nil" 'bool (type nil))
(test "type of t" 'bool (type t))
(test "type of int" 'number (type 123))
(test "type of float" 'number (type 1.23))
(test "type of pair" 'pair (type '(1 2 3)))
(test "type of string" 'string (type "123"))
(test "type of vector" 'vector (type #(1 2 3)))
(test "type of table" 'table (type (make-hash-table)))

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

;; arithmetic operations
(test "+" 6 (+ 1 2 3))
(test "overwrite-inlined-function" '(1 2 3) (let ((+ list)) (+ 1 2 3)))

;; eval
(test "simple-eval" 6 (eval '(+ 1 2 3)))
(test "eval-can't-see-local" 1 (do (def x 1)
                                   (let ((x 2))
                                     (eval 'x))))

(print "\x1b[1;32mTEST ALL SUCCEEDED!\x1b[0;39m\n")
