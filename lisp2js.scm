(define (compile s)
  (if (pair? s)
      (cond ((special-form? s) => (lambda (fn) (fn s)))
            (else (compile-funcall s)))
    (compile-literal s)))

(define (compile-literal s)
  (cond ((number? s) (number->string s))
        ((symbol? s) (compile-symbol s))
        ((null? s)   "false")
        (else (error #`"compile-literal: [,s]"))))

(define (compile-symbol s)
  (string-append "LISP."
                 (symbol->string s)))

(define (compile-funcall s)
  (define (expand-args args)
    (string-join (map (lambda (x) (compile x))
                      args)
                 ", "))
  (let ((fn (car s))
        (args (cdr s)))
    (string-append (compile fn)
                   "("
                   (expand-args args)
                   ")")))

(define (compile-quote s)
  (let ((x (cadr s)))
    (if (pair? x)
        (compile `(cons (quote ,(car x)) (quote ,(cdr x))))
      (compile-literal x))))

(define *special-forms*
  `((quote . ,compile-quote)
    ))

(define (special-form? s)
  (cond ((assoc (car s) *special-forms*) => cdr)
        (else #f)))

(define (main args)
  (let ((ss (port->sexp-list (current-input-port))))
    (dolist (s ss)
      (display (compile s))
      (display ";\n"))))
