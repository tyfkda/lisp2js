(define (expand-args args env)
  (string-join (map (lambda (x) (compile x env))
                    args)
               ", "))

(define (expand-body body env)
  (if (null? body)
      "LISP.nil"
    (expand-args body env)))

(define (symbol->js-string sym)
  (define (escape-char c)
    (string-append "$"
                   (integer->hex-string (char->integer c) "00")))
  (define (integer->hex-string x padding)
    (let* ((s (string-append padding
                             (number->string x 16)))
           (sl (string-length s))
           (pl (string-length padding)))
      (substring s (- sl pl) sl)))
  (regexp-replace-all #/[^0-9A-Za-z_]/ (symbol->string sym)
                      (lambda (m) (escape-char (string-ref (m) 0)))))

(define (compile-symbol sym env)
  (define (local-var? sym env)
    (member sym env))
  (string-append (if (local-var? sym env)
                     ""
                   "LISP.")
                 (symbol->js-string sym)))

(define (escape-char c)
  (cond ((string=? c "\\") "\\\\")
        ((string=? c "\t") "\\t")
        ((string=? c "\n") "\\n")
        ((string=? c "\"") "\\\"")
        (else c)))

(define (escape-string s)
  (regexp-replace-all #/[\\\t\n"]/ s
                      (lambda (m) (escape-char (m)))))

(define (compile-string str)
  #"\"~(escape-string str)\"")

(define (compile-regexp regex)
  #"/~(regexp->string regex)/")

(define (compile-literal s env)
  (cond ((number? s) (number->string s))
        ((symbol? s) (compile-symbol s env))
        ((string? s) (compile-string s))
        ((regexp? s) (compile-regexp s))
        ((null? s)   "LISP.nil")
        ((eq? s #t)  "LISP.t")
        ((eq? s #f)  "LISP.nil")
        (else (error #"compile-literal: [~s]"))))

(define (compile-funcall s env)
  (let ((fn (car s))
        (args (cdr s)))
    (string-append (compile fn env)
                   "("
                   (expand-args args env)
                   ")")))

(define (compile-quote s env)
  (let ((x (cadr s)))
    (if (pair? x)
        (compile `(cons (quote ,(car x)) (quote ,(cdr x))) env)
      (if (symbol? x)
          (string-append "LISP.intern(\""
                         (symbol->js-string x)
                         "\")")
        (compile-literal x env)))))

(define (compile-if s env)
  (let ((p (cadr s))
        (then-node (caddr s))
        (else? (cdddr s)))
    (string-append "(("
                   (compile p env)
                   ") !== LISP.nil ? ("
                   (compile then-node env)
                   ") : ("
                   (if (not (null? else?))
                       (compile (car else?) env)
                     "LISP.nil")
                   "))")))

(define (compile-lambda s env)
  (define (extend-env env params)
    (append params env))
  (let ((params (cadr s))
        (bodies (cddr s)))
    (let1 newenv (extend-env env params)
      (string-append "(function("
                     (expand-args params newenv)
                     "){return ("
                     (expand-body bodies newenv)
                     ");})"))))

(define (compile-define s env)
  (let ((name (cadr s))
        (body (cddr s)))
    (if (pair? name)
        (compile-define `(define ,(car name) (lambda ,(cdr name) ,@body)) env)
      (string-append (compile-symbol name env)
                     " = "
                     (compile (car body) env)))))

(define *macro-table* (make-hash-table))
(define (compile-defmacro s env)
  (let ((name (cadr s))
        (params (caddr s))
        (body (cdddr s)))
    (hash-table-put! *macro-table* name (eval `(lambda ,params ,@body)
                                              (interaction-environment)))
    "LISP.nil"))
(define (macro? symbol)
  (hash-table-exists? *macro-table* symbol))
(define (expand-macro s)
  (apply (hash-table-get *macro-table* (car s))
         (cdr s)))

(define *special-forms*
  (list (cons 'quote compile-quote)
        (cons 'if  compile-if)
        (cons 'lambda compile-lambda)
        (cons 'define  compile-define)
        (cons 'defmacro  compile-defmacro)
        ))

(define (special-form? s)
  (cond ((assoc (car s) *special-forms*) => cdr)
        (else #f)))

(define (compile s env)
  (if (pair? s)
      (cond ((special-form? s) => (lambda (fn) (fn s env)))
            ((macro? (car s)) (compile (expand-macro s) env))
            (else (compile-funcall s env)))
    (compile-literal s env)))

(define (main args)
  (let ((ss (port->sexp-list (current-input-port))))
    (dolist (s ss)
      (display (compile s '()))
      (display ";\n"))
    0))
