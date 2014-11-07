(define (expand-args args env)
  (string-join (map (lambda (x) (compile* x env))
                    args)
               ", "))

(define (expand-body body env)
  (if (null? body)
      "LISP.nil"
    (expand-args body env)))

(define (escape-char c)
  (cond ((string=? c "\\") "\\\\")
        ((string=? c "\t") "\\t")
        ((string=? c "\n") "\\n")
        ((string=? c "\"") "\\\"")
        (else c)))

(define (escape-string s)
  (regexp-replace-all #/[\\\t\n"]/ s
                      (lambda (m) (escape-char (m)))))

(define (escape-symbol sym)
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
  (if (local-var? sym env)
      (escape-symbol sym)
    (let ((s (symbol->string sym)))
      (if (rxmatch #/^[0-9A-Za-z_]*$/ s)
          (string-append "LISP."
                         s)
        (string-append "LISP[\""
                       (escape-string s)
                       "\"]")))))

(define (compile-string str)
  (string-append "\""
                 (escape-string str)
                 "\""))

(define (compile-regexp regex)
  (string-append "/"
                 (regexp->string regex)
                 "/"))

(define (compile-literal s env)
  (cond ((number? s) (number->string s))
        ((symbol? s) (compile-symbol s env))
        ((string? s) (compile-string s))
        ((regexp? s) (compile-regexp s))
        ((null? s)   "LISP.nil")
        ((eq? s #t)  "LISP.t")
        ((eq? s #f)  "LISP.nil")
        (else (error (string-append "compile-literal: [" s "]")))))

(define (compile-funcall s env)
  (let ((fn (car s))
        (args (cdr s)))
    (string-append (compile* fn env)
                   "("
                   (expand-args args env)
                   ")")))

(define (compile-quote s env)
  (let ((x (car s)))
    (if (pair? x)
        ;(compile `(cons (quote ,(car x)) (quote ,(cdr x))) env)
        (compile* (list 'cons
                             (list 'quote (car x))
                             (list 'quote (cdr x)))
                       env)
      (if (symbol? x)
          (string-append "LISP.intern(\""
                         (escape-string (symbol->string x))
                         "\")")
        (compile-literal x env)))))

(define (compile-if s env)
  (let ((p (car s))
        (then-node (cadr s))
        (else? (cddr s)))
    (string-append "(("
                   (compile* p env)
                   ") !== LISP.nil ? ("
                   (compile* then-node env)
                   ") : ("
                   (if (null? else?)
                       "LISP.nil"
                     (compile* (car else?) env))
                   "))")))

(define (compile-set! s env)
  (let ((sym (car s))
        (val (cadr s)))
    (string-append (compile* sym env)
                   " = "
                   (compile* val env))))

(define (compile-begin s env)
  (cond ((null? s) "LISP.nil")
        ((null? (cdr s)) (compile* (car s) env))
        (else (string-append "("
                             (expand-body s env)
                             ")"))))

(define (compile-lambda s env)
  (define (extend-env env params)
    (append params env))
  (let ((raw-params (car s))
        (bodies (cdr s)))
    (let ((params (if (proper-list? raw-params)
                      raw-params
                    (reverse! (reverse raw-params))))  ; Remove dotted part.
          (rest (if (pair? raw-params)
                    (cdr (last-pair raw-params))
                  raw-params)))
      (let1 newenv (extend-env env (if (null? rest)
                                       params
                                     (append (list rest)
                                             params)))
        (string-append "(function("
                       (expand-args params newenv)
                       "){"
                       (if (null? rest)
                           ""
                         (string-append "var "
                                        (symbol->string rest)
                                        " = LISP._getRestArgs(arguments, "
                                        (number->string (length params))
                                        "); "))
                       "return ("
                       (expand-body bodies newenv)
                       ");})")))))

(define (compile-define s env)
  (let ((name (car s))
        (body (cdr s)))
    (if (pair? name)
        ;(compile-define `(define ,(car name) (lambda ,(cdr name) ,@body)) env)
        (compile-define (list (car name)
                              (list* 'lambda (cdr name) body))
                        env)
      (string-append (compile-symbol name env)
                     " = "
                     (compile* (car body) env)))))

(define *macro-table* (make-hash-table))
(define (register-macro name func)
  (hash-table-put! *macro-table* name func))
(define (compile-defmacro s env)
  (let ((name (caar s))
        (params (cdar s))
        (body (cdr s)))
    ;(hash-table-put! *macro-table* name (eval `(lambda ,params ,@body)
    ;                                          (interaction-environment)))
    (let ((exp (list* 'lambda params body)))
      (if *run-on-js*
          (let ((compiled (compile exp)))
            (register-macro name (jseval compiled))
            (string-append "LISP['register-macro']("
                           compiled
                           ")"))
        (begin (hash-table-put! *macro-table* name (eval exp
                                                         (interaction-environment)))
               (string-append "/*" (symbol->string name) "*/ LISP.nil"))))))

(define (macro? symbol)
  (hash-table-exists? *macro-table* symbol))
(define (macroexpand-1 s)
  (let ((f (and (pair? s)
                (hash-table-get *macro-table* (car s) #f))))
    (if f
        (apply f (cdr s))
      s)))
(define (macroexpand exp)
  (let ((expanded (macroexpand-1 exp)))
    (if (equal? expanded exp)
        exp
      (macroexpand expanded))))

(define *special-forms*
  (list (cons 'quote compile-quote)
        (cons 'if  compile-if)
        (cons 'begin  compile-begin)
        (cons 'set! compile-set!)
        (cons 'lambda compile-lambda)
        (cons 'define  compile-define)
        (cons 'define-macro  compile-defmacro)
        ))

(define (special-form? s)
  (cond ((assoc (car s) *special-forms*) => cdr)
        (else #f)))

(define (compile* s env)
  (if (pair? s)
      (cond ((macro? (car s)) (compile* (macroexpand s) env))
            ((special-form? s) => (lambda (fn) (fn (cdr s) env)))
            (else (compile-funcall (macroexpand s) env)))
    (compile-literal s env)))

(define (compile s)
  (compile* s '()))
