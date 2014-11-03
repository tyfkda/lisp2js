(define (compile-literal s)
  (cond ((number? s) (number->string s))
        (else (error "compile-literal"))))

(define (main args)
  (let ((ss (port->sexp-list (current-input-port))))
    (dolist (s ss)
      (display (compile-literal s))
      (display ";\n"))))
