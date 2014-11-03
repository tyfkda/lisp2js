(define (compile s)
  (if (pair? s)
      (compile-funcall s)
    (compile-literal s)))

(define (compile-literal s)
  (cond ((number? s) (number->string s))
        ((symbol? s) (symbol->string s))
        (else (error #`"compile-literal: [,s]"))))

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


(define (main args)
  (let ((ss (port->sexp-list (current-input-port))))
    (dolist (s ss)
      (display (compile s))
      (display ";\n"))))
