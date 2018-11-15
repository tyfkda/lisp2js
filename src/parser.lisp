;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scope

(defun create-scope (parent-scope params)
  (vector (remove-if (lambda (x) (member x '(&rest &body))) params)  ;; 0: params
          nil             ; 1: added var (e.g. quoted values)
          parent-scope))  ; 2: parent scope

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
        (t (scope-var? (scope-outer scope) x))))

(defun local-var? (scope sym)
  (and (symbol? sym)
       (scope-var? scope (get-receiver sym))))

(defun special-var? (scope sym)
  (member sym '(this)))

;; Get symbol which sits on the top of dot-concatenated symbol.
;;   ex. foo.bar.baz => foo
(defun get-receiver (sym)
  (let ((s (symbol->string sym)))
    (aif (string-scan s ".")
         (intern (substring s 0 it))
      sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser.
(defmacro record (args param &body body)
  `(apply (lambda ,param ,@body)
          ,args))

(defmacro record-case (x &body clauses)
  (let1 value (gensym)
    `(let1 ,value ,x
       (case (car ,value)
         ,@(map (lambda (clause)
                  (if (eq? (car clause) t)
                      clause
                    (let1 key (caar clause)
                      `((,key)
                        (record (cdr ,value) ,(cdar clause) ,@(cdr clause))))))
                clauses)))))

(defun parse-quoted-value (x)
  (cond ((pair? x)
         (if (proper-list? x)
             (vector :FUNCALL (vector :REF 'list)
                     (map parse-quoted-value x))
           (vector :FUNCALL (vector :REF (if (pair? (cdr x)) 'list* 'cons))
                   (map parse-quoted-value (dotted->proper x)))))
        ((vector? x)
         (vector :FUNCALL (vector :REF 'vector)
                 (map parse-quoted-value (vector->list x))))
        (t (vector :CONST x))))

(flet ((parse-args (args scope)
         (map (lambda (x)
                (parse* x scope))
              args)))
  (labels ((confirm-valid-params (params)
             (when params
               (if (symbol? (car params))
                   (confirm-valid-params (cdr params))
                 (compile-error "function parameter must be symbol, but" (car params))))))

    (defun parse-list (s scope)
      (record-case s
                   ((quote x)   (if (or (pair? x) (vector? x))
                                    (vector :REF (scope-add-var scope (parse-quoted-value x)))
                                  (vector :CONST x)))
                   ((if p thn &body els)  (vector :IF
                                                  (parse* p scope)
                                                  (parse* thn scope)
                                                  (if (null? els)
                                                      nil
                                                    (parse* (car els) scope))))
                   ((set! x v)  (vector :SET! (parse* x scope) (parse* v scope)))
                   ((lambda params &body body)  (progn (confirm-valid-params params)
                                                       (let ((new-scope (create-scope scope params)))
                                                         (vector :LAMBDA
                                                                 new-scope
                                                                 params
                                                                 (parse-args body new-scope)))))
                   ((def name value)  (vector :DEF
                                              (parse* name scope)
                                              (parse* value scope)))
                   (t (if (or (null? s) (proper-list? s))
                          (vector :FUNCALL
                                  (parse* (car s) scope)
                                  (parse-args (cdr s) scope))
                        (compile-error "funcall must be proper list, but" s)))))))

(defun parse* (s scope)
  (cond ((pair? s)   (if (local-var? scope (car s))
                         ;; Symbol is defined in the scope, so it isn't handled as a macro.
                         (parse-list s scope)
                       (let1 expanded (macroexpand s)
                         (if (pair? expanded)
                             (parse-list expanded scope)
                           (parse* expanded scope)))))
        ((symbol? s) (vector :REF s))
        (t           (parse-quoted-value s))))

(defun parse (s)
  (parse* s (create-scope nil ())))
