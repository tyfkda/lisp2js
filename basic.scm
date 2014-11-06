;; Currently body expression in `defmacro` is evaluated in base Lisp
;; environment, so we don't have to have basic functions in our environment.
(defmacro let (pairs . body)
  (if (symbol? pairs)  ; named-let
      (let ((name pairs)
            (pairs (car body))
            (body (cdr body)))
        `((lambda (,name)
            (set! ,name (lambda ,(map car pairs)
                          ,@body))
            (,name ,@(map cadr pairs)))
          nil))
    `((lambda ,(map car pairs)
        ,@body)
      ,@(map cadr pairs))))

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
    (let ((clause (car clauses))
          (rest (cdr clauses)))
      (if (eq? (car clause) 'else)
          `(begin ,@(cdr clause))
        (if (null? (cdr clause))  ; cond ((foo))
            (let ((g (gensym)))
              `(let ((,g ,(car clause)))
                 (if ,g ,g
                   (cond ,@rest))))
          (if (eq? (cadr clause) '=>)  ; cond ((foo) => bar)
              (let ((g (gensym)))
                `(let ((,g ,(car clause)))
                   (if ,g (,(caddr clause) ,g)
                     (cond ,@rest))))
            `(if ,(car clause)  ; otherwise
                 (begin ,@(cdr clause))
               (cond ,@rest))))))))

(defmacro and args
  (if (null? args)
      't  ; (and) = true
    (if (null? (cdr args))
        (car args)
      `(if ,(car args)
           (and ,@(cdr args))
         'nil))))

;;
(define (null? x)  (eq? x nil))
(define (not x)    (eq? x nil))

(define (caar x)  (car (car x)))
(define (cadr x)  (car (cdr x)))
(define (cdar x)  (cdr (car x)))
(define (cddr x)  (cdr (cdr x)))
(define (caddr x)  (car (cddr x)))
(define (cdddr x)  (cdr (cddr x)))

(define (equal? x y)
  (if (eq? x y)
      t
    (and (pair? x)
         (pair? y)
         (equal? (car x) (car y))
         (equal? (cdr x) (cdr y)))))

(define (length ls)
  (let loop ((ls ls)
             (acc 0))
    (if (pair? ls)
        (loop (cdr ls) (+ acc 1))
      acc)))

(define (last-pair ls)
  (if (pair? (cdr ls))
      (last-pair (cdr ls))
    ls))

(define (proper-list? ls)
  (and (pair? ls)
       (null? (cdr (last-pair ls)))))

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

(define (append ls . rest)
  (cond ((null? rest) ls)
        ((null? ls)   (apply append rest))
        (else (cons (car ls)
                    (apply append (cdr ls) rest)))))

(define (reverse ls)
  (let loop ((ls ls)
             (acc '()))
    (if (null? ls)
        acc
      (loop (cdr ls)
            (cons (car ls) acc)))))

(define (list* . args)
  (if (null? args)
      nil
    (if (null? (cdr args))
        (car args)
      (let loop ((p args)
                 (q (cdr args)))
        (if (null? (cdr q))
            (begin (set-cdr! p (car q))
                   args)
          (loop q (cdr q)))))))
