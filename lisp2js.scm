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
                 (symbol->js-string s)))

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
      (display (compile s))
      (display ";\n"))))
