;; Currently body expression in `defmacro` is evaluated in base Lisp
;; environment, so we don't have to have basic functions in our environment.
(defmacro let (pairs . body)
  `((lambda ,(map car pairs)
      ,@body)
    ,@(map cadr pairs)))

(defmacro let1 (var val . body)
  `((lambda (,var)
      ,@body)
    ,val))

(defmacro let* (pairs . body)
  (if (null? pairs)
      `(begin ,@body)
    `(let1 ,(caar pairs) ,(cadar pairs)
       (let* ,(cdr pairs)
         ,@body))))

(defmacro cond clauses
  (if (null? clauses)
      '()
    (if (eq? (caar clauses) 'else)
        `(begin ,@(cdar clauses))
      `(if ,(caar clauses)
           (begin ,@(cdar clauses))
         (cond ,@(cdr clauses))))))

(defmacro and args
  (if (null? args)
      't  ; (and) = true
    (if (null? (cdr args))
        (car args)
      `(if ,(car args)
           (and ,@(cdr args))
         'nil))))

;;
(define (null? x)
  (eq? x nil))

(define (caar x)  (car (car x)))
(define (cadr x)  (car (cdr x)))
(define (cdar x)  (cdr (car x)))
(define (cddr x)  (cdr (cdr x)))

(define (equal? x y)
  (if (eq? x y)
      t
    (and (pair? x)
         (pair? y)
         (equal? (car x) (car y))
         (equal? (cdr x) (cdr y)))))

(define (member x ls)
  (cond ((null? ls) nil)
        ((eq? x (car ls)) ls)
        (else (member x (cdr ls)))))

(define (assoc x ls)
  (if (null? ls) #f
    (if (eq? x (caar ls)) (car ls)
      (assoc x (cdr ls)))))

(define (map f ls)
  (if (null? ls)
      '()
    (cons (f (car ls))
          (map f (cdr ls)))))
