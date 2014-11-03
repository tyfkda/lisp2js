(define (compile s env)
  (if (pair? s)
      (cond ((special-form? s) => (lambda (fn) (fn s env)))
            (else (compile-funcall s env)))
    (compile-literal s env)))

(define (compile-literal s env)
  (cond ((number? s) (number->string s))
        ((symbol? s) (compile-symbol s env))
        ((null? s)   "false")
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

(define (compile-funcall s env)
  (define (expand-args args)
    (string-join (map (lambda (x) (compile x env))
                      args)
                 ", "))
  (let ((fn (car s))
        (args (cdr s)))
    (string-append (compile fn env)
                   "("
                   (expand-args args)
                   ")")))

(define (compile-quote s env)
  (let ((x (cadr s)))
    (if (pair? x)
        (compile `(cons (quote ,(car x)) (quote ,(cdr x))) env)
      (compile-literal x env))))

(define *special-forms*
  `((quote . ,compile-quote)
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
