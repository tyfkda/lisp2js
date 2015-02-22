;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scope

(defun new-scope (parent-scope params)
  (vector (dotted->proper params) nil parent-scope))

(defun scope-param (scope)
  (vector-ref scope 0))

(defun scope-outer (scope)
  (vector-ref scope 2))

(defun scope-add-var (scope val)
  (let1 x (gensym)
    (vector-set! scope 1
                 (cons (cons x val) (vector-ref scope 1)))
    (vector-set! scope 0
                 (cons x (vector-ref scope 0)))
    x))

(defun scope-get-var (scope)
  (vector-ref scope 1))

(defun scope-var? (scope x)
  (cond ((null? scope) nil)
        ((member x (scope-param scope)) t)
        (else (scope-var? (scope-outer scope) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Tree creator.
(defun traverse-args (args scope)
  (map (lambda (x)
         (traverse* x scope))
       args))

(defmacro record (args param &body body)
  `(apply (lambda ,param ,@body)
          ,args))

(defmacro record-case (x &body clauses)
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

(defun traverse-quoted-value (x)
  (if (pair? x)
      (vector ':FUNCALL (vector ':REF (if (proper-list? x) 'list 'list*))
              (map traverse-quoted-value (dotted->proper x)))
    (vector ':CONST x)))

(defun traverse-list (s scope)
  (record-case s
    ((quote x)   (cond ((pair? x)  (vector ':REF (scope-add-var scope (traverse-quoted-value x))))
                       (else (vector ':CONST x))))
    ((if p thn &body els)  (vector ':IF
                                   (traverse* p scope)
                                   (traverse* thn scope)
                                   (if (null? els)
                                       nil
                                     (traverse* (car els) scope))))
    ((set! x v)  (vector ':SET! (traverse* x scope) (traverse* v scope)))
    ((lambda params &body body)  (let ((new-scope (new-scope scope params)))
                                   (vector ':LAMBDA
                                           new-scope
                                           params
                                           (traverse-args body new-scope))))
    ((def name value)  (vector ':DEF
                               (traverse* name scope)
                               (traverse* value scope)))
    ((defun name params &body body)  (vector ':DEF
                                             (traverse* name scope)
                                             (traverse* `(lambda ,params ,@body) scope)))
    ((defmacro name params &body body)  (vector ':DEFMACRO
                                                name
                                                `(lambda ,params ,@body)))
    ((new klass &rest args)  (vector ':NEW klass (traverse-args args new-scope)))
    (else (vector ':FUNCALL
                  (traverse* (car s) scope)
                  (traverse-args (cdr s) scope)))))

(defun traverse* (s scope)
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
(defun get-receiver (sym)
  (let ((s (symbol->string sym)))
    (aif (string-scan s ".")
         (intern (substring s 0 it))
      sym)))

(defun local-var? (sym scope)
  (scope-var? scope (get-receiver sym)))

(defun expand-args (args scope)
  (string-join (map (lambda (x) (compile* x scope))
                    args)
               ", "))

(defun expand-body (body scope)
  (if (null? body)
      "LISP.nil"
    (expand-args body scope)))

(let1 table (make-hash-table)
  (hash-table-put! table "\\" "\\\\")
  (hash-table-put! table "\t" "\\t")
  (hash-table-put! table "\n" "\\n")
  (hash-table-put! table "\"" "\\\"")
  (defun escape-char (c)
    (aif (hash-table-get table c)
         it
      c)))

(defun escape-string (s)
  (regexp-replace-all #/[\\\t\n"]/ s  ;" <= Prevent Github source highlight to leak string literal...
                      (lambda (m) (escape-char (m)))))

(defun escape-symbol (sym)
  (defun escape-sym-char (c)
    (string-append "$"
                   (integer->hex-string (char->integer c) "00")))
  (defun integer->hex-string (x padding)
    (let* ((s (string-append padding
                             (number->string x 16)))
           (sl (string-length s))
           (pl (string-length padding)))
      (substring s (- sl pl) sl)))
  (regexp-replace-all #/[^0-9A-Za-z_.]/ (symbol->string sym)
                      (lambda (m) (escape-sym-char (string-ref (m) 0)))))

(defun compile-symbol (sym scope)
  (if (local-var? sym scope)
      (escape-symbol sym)
    (let ((s (symbol->string sym)))
      (if (rxmatch #/^[0-9A-Za-z_.]*$/ s)
          (string-append "LISP."
                         s)
        (string-append "LISP[\""
                       (escape-string s)
                       "\"]")))))

(defun compile-string (str)
  (string-append "\""
                 (escape-string str)
                 "\""))

(defun compile-vector (vect scope)
  (string-append "["
                 (let1 v (vector-map (lambda (x)
                                       (compile-quote x scope))
                                     vect)
                   (v.join ", "))  ;; TODO: Fix this not to use Array#join/JavaScript.
                 "]"))

(defun compile-regexp (regex)
  (string-append "/"
                 (regexp->string regex)
                 "/"))

(defun compile-literal (s scope)
  (cond ((number? s) (number->string s))
        ((symbol? s) (compile-symbol s scope))
        ((string? s) (compile-string s))
        ((vector? s) (compile-vector s scope))
        ((regexp? s) (compile-regexp s))
        ((null? s)   "LISP.nil")
        (else (error (string-append "compile-literal: [" s "]")))))

(defun unary-op? (sym)
  (member sym '(+ -)))

(defun compile-unary-op (fn arg scope)
  (string-append "("
                 (symbol->string fn)
                 (compile* arg scope)
                 ")"))

(defun binop? (sym)
  (member sym '(+ - * / %)))

(defun compile-binop (fn args scope)
  (string-append "("
                 (string-join (map (lambda (x) (compile* x scope))
                                   args)
                              (string-append " " (symbol->string fn) " "))
                 ")"))

(defun do-compile-funcall (fn args scope)
  (string-append (compile* fn scope)
                 "("
                 (expand-args args scope)
                 ")"))

(defun compile-funcall (fn args scope)
  (if (and (eq? (vector-ref fn 0) ':REF)
           (not (local-var? (vector-ref fn 1) scope))
           (not (null? args)))
      (let1 fnsym (vector-ref fn 1)
        (cond ((and (binop? fnsym)
                    (not (null? (cdr args))))
               (compile-binop fnsym args scope))
              ((and (unary-op? fnsym)
                    (null? (cdr args)))
               (compile-unary-op fnsym (car args) scope))
              (else (do-compile-funcall fn args scope))))
    (do-compile-funcall fn args scope)))

(defun compile-quote (x scope)
  (if (pair? x)
      (compile* `(cons ',(car x)
                       ',(cdr x))
                scope)
    (if (symbol? x)
        (string-append "LISP.intern(\""
                       (escape-string (symbol->string x))
                       "\")")
      (compile-literal x scope))))

(defun compile-if (pred-node then-node else-node scope)
  (string-append "(LISP.isTrue("
                 (compile* pred-node scope)
                 ") ? ("
                 (compile* then-node scope)
                 ") : ("
                 (if else-node
                     (compile* else-node scope)
                   "LISP.nil")
                 "))"))

(defun compile-set! (sym val scope)
  (string-append (compile* sym scope)
                 " = "
                 (compile* val scope)))

(defun compile-lambda (params bodies base-scope extended-scope)
  (let1 rest-pos (position-if (lambda (sym) (member sym '(&rest &body))) params)
    (let ((proper-params (if rest-pos
                             (take rest-pos params)
                           params))
          (rest (and rest-pos (elt (+ rest-pos 1) params))))
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
                     ");})"))))

(defun compile-def (name value scope)
  (string-append (compile* name scope)
                 " = "
                 (compile* value scope)))

(defun macroexpand (exp)
  (let ((expanded (macroexpand-1 exp)))
    (if (equal? expanded exp)
        exp
      (macroexpand expanded))))

(defun compile-new (class-name args scope)
  (string-append "new "
                 (symbol->string class-name)
                 "("
                 (expand-args args scope)
                 ")"))

;; If the given scope has quoted value, output them as local variable values,
;; and encapsulate with anonymous function.
(defun compile-new-scope (scope compiled-body)
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

(defun compile* (s scope)
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
    ((:DEF)  (compile-def (vector-ref s 1) (vector-ref s 2) scope))
    ((:DEFMACRO)  (do-compile-defmacro (vector-ref s 1)
                                       (vector-ref s 2)))
    ((:NEW)  (compile-new (vector-ref s 1) (vector-ref s 2) scope))
    (else  (string-append "???" s "???"))))

(defun compile (s)
  (let* ((top-scope (new-scope nil ()))
         (tree (traverse* s top-scope)))
    ;;(write tree)
    (compile-new-scope top-scope
                       (compile* tree top-scope))))
