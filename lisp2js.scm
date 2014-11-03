(define (compile s env)
  (if (pair? s)
      (cond ((special-form? s) => (lambda (fn) (fn s env)))
            (else (compile-funcall s env)))
    (compile-literal s env)))

(define (compile-literal s env)
  (cond ((number? s) (number->string s))
        ((symbol? s) (compile-symbol s env))
        ((string? s) (compile-string s))
        ((char? s)   (compile-char s))
        ((null? s)   "LISP.nil")
        (else (error #`"compile-literal: [,s]"))))

(define (compile-symbol sym env)
  (define (local-var? sym env)
    (member sym env))
  (string-append (if (local-var? sym env)
                     ""
                   "LISP.")
                 (symbol->js-string sym)))

(define (symbol->js-string sym)
  (define (char->js-str c)
    (if (sym-char? c)
        (string c)
      (string-append "_"
                     (integer->hex-string (char->integer c) 2))))
  (define (sym-char? c)
    (alnum? c))
  (define (integer->hex-string x keta)
    (let* ((s (number->string x 16))
           (l (string-length s)))
      (if (< l keta)
          (let1 zeros (make-string (- keta l) #\0)
            (string-append zeros s))
        s)))
  (apply string-append (map char->js-str
                            (string->list (symbol->string sym)))))

(define (compile-string str)
  #`"\",str\"")

(define (compile-char char)
  #`"\",char\"")

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
      (compile-literal x env))))

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

(define (expand-body body env)
  (if (null? body)
      "LISP.nil"
    (expand-args body env)))

(define (expand-args args env)
  (string-join (map (lambda (x) (compile x env))
                    args)
               ", "))

(define (compile-define s env)
  (let ((name (cadr s))
        (body (cddr s)))
    (if (pair? name)
        (compile-define `(define ,(car name) (lambda ,(cdr name) ,@body)) env)
      (string-append (compile-symbol name env)
                     " = "
                     (compile (car body) env)))))

(define *special-forms*
  (list (cons 'quote compile-quote)
        (cons 'if  compile-if)
        (cons 'lambda compile-lambda)
        (cons 'define  compile-define)
        ))

(define (special-form? s)
  (cond ((assoc (car s) *special-forms*) => cdr)
        (else #f)))

(define (alnum? c)
  (or (alpha? c) (num? c)))

(define (alpha? c)
  (or (and (char<=? #\a c) (char<=? c #\z))
      (and (char<=? #\A c) (char<=? c #\Z))))

(define (num? c)
  (and (char<=? #\0 c) (char<=? c #\9)))

(define (main args)
  (let ((ss (port->sexp-list (current-input-port))))
    (dolist (s ss)
      (display (compile s '()))
      (display ";\n"))
    0))
