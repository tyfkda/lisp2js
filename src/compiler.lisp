;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler

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
  (hash-table-put! table "\r" "\\r")
  (hash-table-put! table "\"" "\\\"")
  (defun escape-char (c)
    (or (hash-table-get table c)
        c)))

(defun escape-string (s)
  (regexp-replace-all #/[\\\t\n\r"]/ s  ;; "
                      (lambda (m) (escape-char (m)))))

(flet ((integer->hex-string (x padding)
         (let* ((s (string-append padding
                                  (number->string x 16)))
                (sl (string-length s))
                (pl (string-length padding)))
           (substring s (- sl pl) sl))))
  (defun escape-sym-char (c)
    (string-append "$"
                   (integer->hex-string (char->number c) "00"))))

(let1 JS-RESERVED-WORDS
     ;; `this` can be used in local scope.
     '(null true false
       break case catch continue debugger default delete do else
       finally for function if in instanceof new return switch
       throw try typeof var void while with)
  (defun escape-param-name (sym)
    (if (member sym JS-RESERVED-WORDS)
        (string-append "__" (symbol->string sym))
      (regexp-replace-all #/[^0-9A-Za-z_.]/ (symbol->string sym)
                          (lambda (m) (escape-sym-char (string-ref (m) 0)))))))

(defun compile-gref (sym)
  (let ((s (symbol->string sym)))
    (if (rxmatch #/^[0-9A-Za-z_.]*$/ s)
        (string-append "LISP."
                       s)
      (string-append "LISP[\""
                     (escape-string s)
                     "\"]"))))

(defun compile-keyword (keyword)
  (string-append "LISP[\"make-keyword\"](\""
                 (escape-string (keyword->string keyword))
                 "\")"))

(defun compile-vector (vect scope)
  (string-append "["
                 (string-join (map (lambda (x) (compile-quote x scope))
                                   (vector->list vect))
                              ", ")
                 "]"))

(defun compile-regexp (regex)
  (string-append "/"
                 (regexp->string regex)
                 "/"))

(defun compile-literal (s scope)
  (cond ((number? s)  (number->string s))
        ((keyword? s) (compile-keyword s))
        ((string? s)  (x->string s t))
        ((vector? s)  (compile-vector s scope))
        ((regexp? s)  (compile-regexp s))
        ((null? s)    "LISP.nil")
        ((eq? s t)    "LISP.t")
        (t (error (string-append "compile-literal: [" (x->string s) "]")))))

(flet ((binop? (sym)
         (member sym '(+ - * / %)))
       (unary-op? (sym)
         (member sym '(+ - ! ~)))
       (comp-op? (sym)
         (member sym '(< > <= >=)))
       (do-compile-funcall (fn args scope)
         (string-append (compile* fn scope)
                        "("
                        (expand-args args scope)
                        ")"))
       (compile-binop (fn args scope)
         (string-append "("
                        (string-join (map (lambda (x) (compile* x scope))
                                          args)
                                     (string-append " " (symbol->string fn) " "))
                        ")"))
       (compile-unary-op (fn arg scope)
         (string-append "("
                        (symbol->string fn)
                        (compile* arg scope)
                        ")"))
       (compile-comp-op (fn lhs rhs scope)
         (string-append "("
                        (compile* lhs scope)
                        (symbol->string fn)
                        (compile* rhs scope)
                        ")")))
  (defun compile-funcall (fn args scope)
    (if (and (eq? (vector-ref fn 0) :REF)
             (vector-ref fn 2)  ; not shadowed by local
             (not (null? args)))
        (let1 fnsym (vector-ref fn 1)
          (cond ((and (binop? fnsym)
                      (not (null? (cdr args))))
                 (compile-binop fnsym args scope))
                ((and (unary-op? fnsym)
                      (null? (cdr args)))
                 (compile-unary-op fnsym (car args) scope))
                ((and (comp-op? fnsym)
                      (not (null? (cdr args)))
                      (null? (cddr args)))
                 (compile-comp-op fnsym (car args) (cadr args) scope))
                (t (do-compile-funcall fn args scope))))
      (do-compile-funcall fn args scope))))

(defun compile-new (klass args scope)
  (string-append "(new "
                 (compile* klass scope)
                 "("
                 (expand-args args scope)
                 "))"))

(defun compile-throw (exp scope)
  (string-append "(function() { throw "
                 (compile* exp scope)
                 " })()"))

(defun compile-quote (x scope)
  (cond ((pair? x)
         (compile* `(cons ',(car x)
                          ',(cdr x))
                   scope))
        ((symbol? x)
         (string-append "LISP.intern(\""
                        (escape-string (symbol->string x))
                        "\")"))
        (t (compile-literal x scope))))

(flet ((ast? (type ast)
         (eq? (vector-ref ast 0) type)))
  (labels ((compile-pred (pnode scope)
             (cond
              ;; and
              ((and (ast? :IF pnode)
                    (let1 enode (vector-ref pnode 3)
                      (or (not enode)
                          (and (ast? :CONST enode)
                               (eq? (vector-ref enode 1) nil)))))
               (string-append "("
                              (compile-pred (vector-ref pnode 1) scope)
                              " && "
                              (compile-pred (vector-ref pnode 2) scope)
                              ")"))
              ;; or
              ((and (ast? :FUNCALL pnode)
                    (ast? :LAMBDA (vector-ref pnode 1))  ;; Direct lambda invocation.
                    (eq? (length (scope-param (vector-ref (vector-ref pnode 1) 1))) 1)  ;; Only 1 parameter.
                    (eq? (length (vector-ref pnode 2)) 1)  ;; Only 1 argument.
                    (eq? (length (vector-ref (vector-ref pnode 1) 3)) 1)  ;; Lambda body is single expression.
                    (let1 ifnode (car (vector-ref (vector-ref pnode 1) 3))
                      (ast? :IF ifnode)  ;; If expression.
                      (ast? :REF (vector-ref ifnode 1))
                      (eq? (vector-ref (vector-ref ifnode 1) 1)
                           (car (scope-param (vector-ref (vector-ref pnode 1) 1))))
                      (equal? (vector-ref ifnode 1)
                              (vector-ref ifnode 2))))
               (let1 ifnode (car (vector-ref (vector-ref pnode 1) 3))
                 (let ((pre (car (vector-ref pnode 2)))
                       (els (vector-ref ifnode 3)))
                   (if (or (null? els)
                           (and (eq? (vector-ref els 0) :CONST)
                                (eq? (vector-ref els 1) nil)))
                       (compile-pred pre scope)
                     (string-append "("
                                    (compile-pred pre scope)
                                    " || "
                                    (compile-pred els scope)
                                    ")")))))
              ;; Compare operators (the result is `t` or `nil`, and they are same meaning as underlying JS boolean value).
              ((and (ast? :FUNCALL pnode)
                    (let1 fnode (vector-ref pnode 1)
                      (ast? :REF fnode)
                      (vector-ref fnode 2)  ; Global.
                      (member (vector-ref fnode 1) '(< > <= >= eq?))))  ; Compare operator.
               (string-append "("
                              (compile* pnode scope)
                              ")"))
              ;; Other.
              (t (string-append "LISP.isTrue("
                                (compile* pnode scope)
                                ")")))))
    (defun compile-if (pred-node then-node else-node scope)
      (string-append "("
                     (compile-pred pred-node scope)
                     " ? "
                     (compile* then-node scope)
                     " : "
                     (if else-node
                         (compile* else-node scope)
                       "LISP.nil")
                     ")"))))

(defun compile-set! (sym val scope)
  (string-append (compile* sym scope)
                 " = "
                 (compile* val scope)))

(defun compile-def (name value scope)
  (string-append (compile* name scope)
                 " = "
                 (if (eq? (vector-ref value 0) :LAMBDA)
                     (compile-lambda value (vector-ref name 1))  ;; Assumes name is :REF
                   (compile* value scope))))

;; If the given scope has quoted value, output them as local variable values,
;; and encapsulate with anonymous function.
(defun compile-new-scope (compiled-body scope)
  (aif (scope-get-var scope)
       (string-append "(function() { var "
                      (string-join (map (lambda (x)
                                          (string-append (escape-param-name (car x))
                                                         " = "
                                                         (compile* (cdr x) scope)))
                                        (reverse it))
                                   ", ")
                      "; return "
                      compiled-body
                      "; })()")
    compiled-body))

(flet ((do-compile-lambda (name params bodies base-scope extended-scope)
         (unless (or (null? params) (pair? params))
           (error "function parameters must be a list"))
         (let1 rest-pos (position-if (lambda (sym) (member sym '(&rest &body))) params)
           (let ((proper-params (if rest-pos
                                    (take rest-pos params)
                                  params))
                 (rest (and rest-pos (elt (+ rest-pos 1) params))))
             (string-append "(function "
                            (if (null? name)
                                ""
                              (escape-param-name name))
                            "("
                            (string-join (map (lambda (x) (escape-param-name x))
                                              proper-params)
                                         ", ")
                            "){"
                            (if (null? rest)
                                ""
                              (string-append "var "
                                             (escape-param-name rest)
                                             " = LISP._getRestArgs(arguments, "
                                             (number->string (length proper-params))
                                             "); "))
                            "return ("
                            (expand-body bodies extended-scope)
                            ");})")))))
  (defun compile-lambda (s name scope)
    (let ((extended-scope (vector-ref s 1))
          (params (vector-ref s 2))
          (body (vector-ref s 3)))
      (compile-new-scope (do-compile-lambda name params body scope extended-scope)
                         extended-scope))))

(defun compile* (s scope)
  (record-case (vector->list s)
    ((:CONST x)          (compile-quote x scope))
    ((:REF sym global?)  (if global?
                             (compile-gref sym)
                           (escape-param-name sym)))
    ((:IF p thn els)     (compile-if p thn els scope))
    ((:FUNCALL fn args)  (compile-funcall fn args scope))
    ((:SET! sym val)     (compile-set! sym val scope))
    ((:LAMBDA)           (compile-lambda s nil scope))
    ((:DEF sym val)      (compile-def sym val scope))
    ((:NEW klass args)   (compile-new klass args scope))
    ((:THROW exp)        (compile-throw exp scope))
    (t  (compile-error "Unknown AST node:" s))))

(defun compile-error (&rest args)
  (error args))

(defun compile (s)
  (let* ((top-scope (create-scope nil ()))
         (tree (parse s top-scope)))
    ;;(write tree)
    (compile-new-scope (compile* tree top-scope)
                       top-scope)))
