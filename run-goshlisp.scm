(define *run-on-js* #f)
(define *macro-table* (make-hash-table))
(define (register-macro name func)
  (hash-table-put! *macro-table* name func))
(define (do-compile-defmacro name exp)
  (register-macro name (eval exp (interaction-environment)))
  (string-append "/*" (symbol->string name) "*/ LISP.nil"))
(define (macroexpand-1 s)
  (let ((macrofn (and (pair? s)
                      (hash-table-get *macro-table* (car s) #f))))
    (if macrofn
        (apply macrofn (cdr s))
      s)))

(include "./src/lisp2js")  ;; Cannot use `require` to refer `*run-on-js*` in it.

(define (main args)
  (let ((ss (port->sexp-list (current-input-port))))
    (dolist (s ss)
      (display (compile s))
      (display ";\n")))
  0)
