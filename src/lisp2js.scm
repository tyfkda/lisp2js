;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scope

(define (new-scope parent-scope params)
  (vector (dotted->proper params) nil parent-scope))

(define (scope-param scope)
  (vector-ref scope 0))

(define (scope-outer scope)
  (vector-ref scope 2))

(define (scope-add-var scope val)
  (let1 x (gensym)
    (vector-set! scope 1
                 (cons (cons x val) (vector-ref scope 1)))
    (vector-set! scope 0
                 (cons x (vector-ref scope 0)))
    x))

(define (scope-get-var scope)
  (vector-ref scope 1))

(define (scope-var? scope x)
  (cond ((null? scope) nil)
        ((member x (scope-param scope)) t)
        (else (scope-var? (scope-outer scope) x))))

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

(define (traverse-quoted-value x)
  (if (pair? x)
      (vector ':FUNCALL (vector ':REF (if (proper-list? x) 'list 'list*))
              (map traverse-quoted-value (dotted->proper x)))
    (vector ':CONST x)))

(define (traverse-list s scope)
  (record-case s
    ((quote x)   (cond ((pair? x)  (vector ':REF (scope-add-var scope (traverse-quoted-value x))))
                       (else (vector ':CONST x))))
    ((if p thn . els)  (vector ':IF
                               (traverse* p scope)
                               (traverse* thn scope)
                               (if (null? els)
                                   nil
                                 (traverse* (car els) scope))))
    ((set! x v)  (vector ':SET! (traverse* x scope) (traverse* v scope)))
    ((lambda params . body)  (let ((new-scope (new-scope scope params)))
                               (vector ':LAMBDA
                                       new-scope
                                       params
                                       (traverse-args body new-scope))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler

;; Get symbol which sits on the top of dot-concatenated symbol.
;;   ex. foo.bar.baz => foo
(define (get-receiver sym)
  (let ((s (symbol->string sym)))
    (aif (string-scan s ".")
         (intern (substring s 0 it))
      sym)))

(define (local-var? sym scope)
  (scope-var? scope (get-receiver sym)))

(define (expand-args args scope)
  (string-join (map (lambda (x) (compile* x scope))
                    args)
               ", "))

(define (expand-body body scope)
  (if (null? body)
      "LISP.nil"
    (expand-args body scope)))

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

(define (compile-symbol sym scope)
  (if (local-var? sym scope)
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

(define (compile-vector vect scope)
  (string-append "["
                 (let1 v (vector-map (lambda (x)
                                       (compile-quote x scope))
                                     vect)
                   (v.join ", "))
                 "]"))

(define (compile-regexp regex)
  (string-append "/"
                 (regexp->string regex)
                 "/"))

(define (compile-literal s scope)
  (cond ((number? s) (number->string s))
        ((symbol? s) (compile-symbol s scope))
        ((string? s) (compile-string s))
        ((vector? s) (compile-vector s scope))
        ((regexp? s) (compile-regexp s))
        ((null? s)   "LISP.nil")
        (else (error (string-append "compile-literal: [" s "]")))))

(define (unary-op? sym)
  (member sym '(+ -)))

(define (compile-unary-op fn arg scope)
  (string-append "("
                 (symbol->string fn)
                 (compile* arg scope)
                 ")"))

(define (binop? sym)
  (member sym '(+ - * / %)))

(define (compile-binop fn args scope)
  (string-append "("
                 (string-join (map (lambda (x) (compile* x scope))
                                   args)
                              (string-append " " (symbol->string fn) " "))
                 ")"))

(define (do-compile-funcall fn args scope)
  (string-append (compile* fn scope)
                 "("
                 (expand-args args scope)
                 ")"))

(define (compile-funcall fn args scope)
  (if (and (eq? (vector-ref fn 0) ':REF)
           (not (local-var? (vector-ref fn 1) scope))
           (not (null? args)))
      (let1 fnsym (vector-ref fn 1)
        (cond ((and (binop? fnsym)
                    (not (null? (cdr args))))
               (compile-binop fnsym args scope))
              ((and (unary-op? fnsym)
                    (null? (cdr args)))
               (compile-unary-op fnsym (car args)))
              (else (do-compile-funcall fn args scope))))
    (do-compile-funcall fn args scope)))

(define (compile-quote x scope)
  (if (pair? x)
      (compile* `(cons ',(car x)
                       ',(cdr x))
                scope)
    (if (symbol? x)
        (string-append "LISP.intern(\""
                       (escape-string (symbol->string x))
                       "\")")
      (compile-literal x scope))))

(define (compile-if pred-node then-node else-node scope)
  (string-append "(("
                 (compile* pred-node scope)
                 ") !== LISP.nil ? ("
                 (compile* then-node scope)
                 ") : ("
                 (if else-node
                     (compile* else-node scope)
                   "LISP.nil")
                 "))"))

(define (compile-set! sym val scope)
  (string-append (compile* sym scope)
                 " = "
                 (compile* val scope)))

(define (compile-lambda params bodies base-scope extended-scope)
  (let ((proper-params (if (proper-list? params)
                           params
                         (reverse! (reverse params))))  ; Remove dotted part.
        (rest (if (pair? params)
                  (cdr (last-pair params))
                params)))
    (string-append "(function("
                   (string-join (map (lambda (x) (escape-symbol x))
                                     proper-params)
                                ", ")
                   "){"
                   (if (null? rest)
                       ""
                     (string-append "var "
                                    (symbol->string rest)
                                    " = LISP._getRestArgs(arguments, "
                                    (number->string (length proper-params))
                                    "); "))
                   "return ("
                   (expand-body bodies extended-scope)
                   ");})")))

(define (compile-define name value scope)
  (string-append (compile* name scope)
                 " = "
                 (compile* value scope)))

(define (macroexpand exp)
  (let ((expanded (macroexpand-1 exp)))
    (if (equal? expanded exp)
        exp
      (macroexpand expanded))))

(define (compile-new class-name args scope)
  (string-append "new "
                 (symbol->string class-name)
                 "("
                 (expand-args args scope)
                 ")"))

;; If the given scope has quoted value, output them as local variable values,
;; and encapsulate with anonymous function.
(define (compile-new-scope scope compiled-body)
  (aif (scope-get-var scope)
       (string-append "(function() { var "
                      (string-join (map (lambda (x)
                                          (string-append (escape-symbol (car x))
                                                         " = "
                                                         (compile* (cdr x) scope)))
                                        (reverse it))
                                   ", ")
                      "; return "
                      compiled-body
                      "; })()")
    compiled-body))

(define (compile* s scope)
  (case (vector-ref s 0)
    ((:CONST)  (compile-quote (vector-ref s 1) scope))
    ((:REF)    (compile-symbol (vector-ref s 1) scope))
    ((:IF)     (let ((p (vector-ref s 1))
                     (thn (vector-ref s 2))
                     (els (vector-ref s 3)))
                 (compile-if p thn els scope)))
    ((:FUNCALL)  (compile-funcall (vector-ref s 1) (vector-ref s 2) scope))
    ((:SET!)  (compile-set! (vector-ref s 1) (vector-ref s 2) scope))
    ((:LAMBDA)  (let ((extended-scope (vector-ref s 1))
                      (params (vector-ref s 2))
                      (body (vector-ref s 3)))
                  (compile-new-scope extended-scope
                                     (compile-lambda params body scope extended-scope))))
    ((:DEFINE)  (compile-define (vector-ref s 1) (vector-ref s 2) scope))
    ((:DEFMACRO)  (do-compile-defmacro (vector-ref s 1)
                                       (vector-ref s 2)))
    ((:NEW)  (compile-new (vector-ref s 1) (vector-ref s 2) scope))
    (else  (string-append "???" s "???"))))

(define (compile s)
  (let* ((top-scope (new-scope nil ()))
         (tree (traverse* s top-scope)))
    ;;(write tree)
    (compile-new-scope top-scope
                       (compile* tree top-scope))))
