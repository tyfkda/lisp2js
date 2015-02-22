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

(defmacro let1 (name value . body)
  `((lambda (,name)
      ,@body)
    ,value))

(defmacro let* (pairs . body)
  (if (null? pairs)
      `(begin ,@body)
    `(let1 ,(caar pairs) ,(cadar pairs)
       (let* ,(cdr pairs)
         ,@body))))

(defmacro when (pred . body)
  `(if ,pred
       (begin ,@body)))

(defmacro unless (pred . body)
  `(if ,pred
       nil
     (begin ,@body)))

(defmacro cond clauses
  (if (null? clauses)
      ()
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

(defmacro case (x . clauses)
  (let1 value (gensym)
    `(let1 ,value ,x
       (cond
        ,@(map (lambda (clause)
                 (cond ((eq? (car clause) 'else)
                        clause)
                       ((null? (cdar clause))
                        `((eq? ,value ',(caar clause)) ,@(cdr clause)))
                       (else `((member ,value ',(car clause)) ,@(cdr clause)))))
               clauses)))))

(defmacro and args
  (if (null? args)
      't  ; (and) = true
    (if (null? (cdr args))
        (car args)
      `(if ,(car args)
           (and ,@(cdr args))
         nil))))

(defmacro or args
  (and (not (null? args))
       (let1 g (gensym)
         `(let1 ,g ,(car args)
            (if ,g ,g
              (or ,@(cdr args)))))))

(defmacro begin body
  (cond ((null? body) nil)
        ((null? (cdr body)) (car body))
        (else `(let ()
                 ,@body))))

(defmacro aif (expr . rest)
  `(let1 it ,expr
     (if it ,@rest)))

(defmacro awhile (expr . body)
  (let ((loop (gensym)))
    `(let ,loop ()
          (let1 it ,expr
            (when it
              ,@body
              (,loop))))))

;;
(defun null? (x)  (eq? x nil))
(defun not (x)    (eq? x nil))

(defun caar (x)  (car (car x)))
(defun cadr (x)  (car (cdr x)))
(defun cdar (x)  (cdr (car x)))
(defun cddr (x)  (cdr (cdr x)))
(defun cadar (x)  (cadr (car x)))
(defun caddr (x)  (car (cddr x)))
(defun cdddr (x)  (cdr (cddr x)))

(defun equal? (x y)
  (if (eq? x y)
      t
    (let1 xtype (type x)
      (when (eq? xtype (type y))
        (case xtype
          ((pair) (and (equal? (car x) (car y))
                       (equal? (cdr x) (cdr y))))
          ((vector) (let1 n (vector-length x)
                      (and (eq? n (vector-length y))
                           (let loop ((i 0))
                             (or (>= i n)
                                 (and (equal? (vector-ref x i) (vector-ref y i))
                                      (loop (+ i 1))))))))
          )))))

(defun length (ls)
  (let loop ((ls ls)
             (acc 0))
    (if (pair? ls)
        (loop (cdr ls) (+ acc 1))
      acc)))

(defun last-pair (ls)
  (if (pair? (cdr ls))
      (last-pair (cdr ls))
    ls))

(defun member (x ls)
  (cond ((null? ls) nil)
        ((eq? x (car ls)) ls)
        (else (member x (cdr ls)))))

(defun assoc (x ls)
  (if (null? ls) nil
    (if (eq? x (caar ls)) (car ls)
      (assoc x (cdr ls)))))

(defun map (f ls)
  (if (null? ls)
      ()
    (cons (f (car ls))
          (map f (cdr ls)))))

(defun append (ls . rest)
  (cond ((null? rest) ls)
        ((null? ls)   (apply append rest))
        (else (cons (car ls)
                    (apply append (cdr ls) rest)))))

(defun reverse (ls)
  (let loop ((ls ls)
             (acc ()))
    (if (pair? ls)
        (loop (cdr ls)
              (cons (car ls) acc))
      acc)))

(defun list* args
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

(defun last-pair (ls)
  (if (pair? (cdr ls))
      (last-pair (cdr ls))
    ls))

(defun proper-list? (ls)
  (and (pair? ls)
       (null? (cdr (last-pair ls)))))

(defun dotted->proper (ls)
  (if (pair? ls)
      (if (proper-list? ls)
          ls
        (let ((dot (cdr (last-pair ls)))
              (rev (reverse ls)))
          (let1 dup (reverse! rev)
            (set-cdr! rev (list dot))
            dup)))
    (list ls)))

(defun vector-map (proc vect)
  (let* ((len (vector-length vect))
         (new-vect (make-vector len)))
    (let loop ((i 0))
      (if (>= i len)
          new-vect
        (begin (vector-set! new-vect i
                            (proc (vector-ref vect i)))
               (loop (+ i 1)))))))

(defun vector->list (vect)
  (let1 n (vector-length vect)
    (if (<= n 0)
        ()
      (let loop ((i (- n 1))
                 (acc ()))
           (if (< i 0)
               acc
             (loop (- i 1) (cons (vector-ref vect i) acc)))))))
