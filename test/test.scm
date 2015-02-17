(define-macro (test name expect actual)
  `(begin (print (string-append ,name
                                " ... "))
          (let ((expect-value ,expect)
                (actual-value ,actual))
            (if (equal? expect-value actual-value)
                (print "ok\n")
              (begin (print (string-append "ERROR [" expect-value "] expected, but [" actual-value "]\n"))
                     (exit 1))))))

;; not
(test "not-nil" t (not nil))
(test "not-t" nil (not t))

(test "begin" 3 (begin 1 2 3))

;; type
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

(print "\x1b[1;32mTEST ALL SUCCEEDED!\x1b[0;39m\n")
