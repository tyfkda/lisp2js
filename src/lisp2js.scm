;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scope

(define (extend-scope parent-scope params)
  (vector params parent-scope))

(define (scope-param scope)
  (vector-ref scope 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Tree creator.
(define (traverse-args args scope)
  (map (lambda (x)
         (traverse* x scope))
       args))

(define-macro (record args param . body)
  `(apply (lambda ,param ,@body)
          ,args))

(define-macro (record-case x . clauses)
  (let1 value (gensym)
    `(let1 ,value ,x
       (case (car ,value)
         ,@(map (lambda (clause)
                  (if (eq? (car clause) 'else)
                      clause
                    (let1 key (caar clause)
                      `((,key)
                        (record (cdr ,value) ,(cdar clause) ,@(cdr clause))))))
                clauses)))))

(define (traverse-list s scope)
  (record-case s
    ((quote x)   (cond ((pair? x) (traverse* `(cons ',(car x)
                                                    ',(cdr x))
                                             scope))
                       (else (vector ':CONST x))))
    ((if)      (vector ':IF
                       (traverse-args (cdr s) scope)))
    ((set! x v)  (vector ':SET! (traverse* x scope) (traverse* v scope)))
    ((lambda params . body)  (let ((new-scope (extend-scope scope (cadr s))))
                               (vector ':LAMBDA
                                       new-scope
                                       (traverse-args (cddr s) new-scope))))
    ((define name value . rest)  (if (pair? name)
                                     (traverse* `(define ,(car name)
                                                   (lambda ,(cdr name) ,value ,@rest))
                                                scope)
                                   (vector ':DEFINE
                                           (traverse* name scope)
                                           (traverse* value scope))))
    ((define-macro name-params . body)  (let ((name (car name-params))
                                              (params (cdr name-params)))
                                          (vector ':DEFMACRO
                                                  name
                                                  `(lambda ,params ,@body))))
    ((new klass . args)  (vector ':NEW klass (traverse-args args new-scope)))
    (else (vector ':FUNCALL
                  (traverse* (car s) scope)
                  (traverse-args (cdr s) scope)))))

(define (traverse* s scope)
  (cond ((pair? s)   (let1 expanded (macroexpand s)
                       (if (pair? expanded)
                           (traverse-list expanded scope)
                         (traverse* expanded scope))))
        ((symbol? s) (vector ':REF s))
        (else        (vector ':CONST s))))

(define (traverse s)
  (traverse* s ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler

;; Get symbol which sits on the top of dot-concatenated symbol.
;;   ex. foo.bar.baz => foo
(define (get-receiver sym)
  (let ((s (symbol->string sym)))
    (aif (string-scan s ".")
         (intern (substring s 0 it))
      sym)))

(define (local-var? sym env)
  (member (get-receiver sym) env))

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
  (define (escape-sym-char c)
    (string-append "$"
                   (integer->hex-string (char->integer c) "00")))
  (define (integer->hex-string x padding)
    (let* ((s (string-append padding
                             (number->string x 16)))
           (sl (string-length s))
           (pl (string-length padding)))
      (substring s (- sl pl) sl)))
  (regexp-replace-all #/[^0-9A-Za-z_.]/ (symbol->string sym)
                      (lambda (m) (escape-sym-char (string-ref (m) 0)))))

(define (compile-symbol sym env)
  (if (local-var? sym env)
      (escape-symbol sym)
    (let ((s (symbol->string sym)))
      (if (rxmatch #/^[0-9A-Za-z_.]*$/ s)
          (string-append "LISP."
                         s)
        (string-append "LISP[\""
                       (escape-string s)
                       "\"]")))))

(define (compile-string str)
  (string-append "\""
                 (escape-string str)
                 "\""))

(define (compile-vector vect env)
  (string-append "["
                 (let1 v (vector-map (lambda (x)
                                       (compile-quote x env))
                                     vect)
                   (v.join ", "))
                 "]"))

(define (compile-regexp regex)
  (string-append "/"
                 (regexp->string regex)
                 "/"))

(define (compile-literal s env)
  (cond ((number? s) (number->string s))
        ((symbol? s) (compile-symbol s env))
        ((string? s) (compile-string s))
        ((vector? s) (compile-vector s env))
        ((regexp? s) (compile-regexp s))
        ((null? s)   "LISP.nil")
        (else (error (string-append "compile-literal: [" s "]")))))

(define (unary-op? sym)
  (member sym '(+ -)))

(define (compile-unary-op fn arg env)
  (string-append "("
                 (symbol->string fn)
                 (compile* arg env)
                 ")"))

(define (binop? sym)
  (member sym '(+ - * / %)))

(define (compile-binop fn args env)
  (string-append "("
                 (string-join (map (lambda (x) (compile* x env))
                                   args)
                              (string-append " " (symbol->string fn) " "))
                 ")"))

(define (do-compile-funcall fn args env)
  (string-append (compile* fn env)
                 "("
                 (expand-args args env)
                 ")"))

(define (compile-funcall fn args env)
  (if (and (eq? (vector-ref fn 0) ':REF)
           (not (local-var? (vector-ref fn 1) env))
           (not (null? args)))
      (let1 fnsym (vector-ref fn 1)
        (cond ((and (binop? fnsym)
                    (not (null? (cdr args))))
               (compile-binop fnsym args env))
              ((and (unary-op? fnsym)
                    (null? (cdr args)))
               (compile-unary-op fnsym (car args)))
              (else (do-compile-funcall fn args env))))
    (do-compile-funcall fn args env)))

(define (compile-quote x env)
  (if (pair? x)
      (compile* `(cons ',(car x)
                       ',(cdr x))
                env)
    (if (symbol? x)
        (string-append "LISP.intern(\""
                       (escape-string (symbol->string x))
                       "\")")
      (compile-literal x env))))

(define (compile-if pred-node then-node else-node env)
  (string-append "(("
                 (compile* pred-node env)
                 ") !== LISP.nil ? ("
                 (compile* then-node env)
                 ") : ("
                 (if else-node
                     (compile* else-node env)
                   "LISP.nil")
                 "))"))

(define (compile-set! sym val env)
  (string-append (compile* sym env)
                 " = "
                 (compile* val env)))

(define (compile-lambda raw-params bodies env)
  (define (extend-env env params)
    (append params env))
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
                     (string-join (map (lambda (x) (escape-symbol x))
                                       params)
                                  ", ")
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
                     ");})"))))

(define (compile-define name value env)
  (string-append (compile* name env)
                 " = "
                 (compile* value env)))

(define (macroexpand exp)
  (let ((expanded (macroexpand-1 exp)))
    (if (equal? expanded exp)
        exp
      (macroexpand expanded))))

(define (compile-new class-name args env)
  (string-append "new "
                 (symbol->string class-name)
                 "("
                 (expand-args args env)
                 ")"))

(define (compile* s env)
  (case (vector-ref s 0)
    ((:CONST)  (compile-quote (vector-ref s 1) env))
    ((:REF)    (compile-symbol (vector-ref s 1) env))
    ((:IF)     (let1 exp (vector-ref s 1)
                 (compile-if (car exp) (cadr exp) (caddr exp) env)))
    ((:FUNCALL)  (compile-funcall (vector-ref s 1) (vector-ref s 2) env))
    ((:SET!)  (compile-set! (vector-ref s 1) (vector-ref s 2) env))
    ((:LAMBDA)  (let ((scope (vector-ref s 1))
                      (body (vector-ref s 2)))
                  (compile-lambda (scope-param scope) body env)))
    ((:DEFINE)  (compile-define (vector-ref s 1) (vector-ref s 2) env))
    ((:DEFMACRO)  (do-compile-defmacro (vector-ref s 1)
                                       (vector-ref s 2)))
    ((:NEW)  (compile-new (vector-ref s 1) (vector-ref s 2) env))
    (else  (string-append "???" s "???"))))

(define (compile s)
  (let1 tree (traverse s)
    ;;(write tree)
    (compile* tree
              ())))
