(define *run-on-js* #f)
(define *macro-table* (make-hash-table))

(include "./src/lisp2js")  ;; Cannot use `require` to refer `*run-on-js*` in it.

(define (main args)
  (let ((ss (port->sexp-list (current-input-port))))
    (dolist (s ss)
      (display (compile s))
      (display ";\n")))
  0)
