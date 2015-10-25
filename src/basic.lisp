(register-macro 'defmacro
                (lambda (name params &body body)
                  `(register-macro ',name
                                   (lambda ,params ,@body))))

(defmacro defun (name params &body body)
  `(def ,name (lambda ,params ,@body)))

(defmacro let (pairs &body body)
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

(defmacro let1 (name value &body body)
  `(let ((,name ,value))
     ,@body))

(defmacro let* (pairs &body body)
  (if (null? pairs)
      `(do ,@body)
    `(let1 ,(caar pairs) ,(cadar pairs)
       (let* ,(cdr pairs)
         ,@body))))

(defmacro when (pred &body body)
  `(if ,pred
       (do ,@body)))

(defmacro unless (pred &body body)
  `(if ,pred
       nil
     (do ,@body)))

(defmacro cond (&body clauses)
  (if (null? clauses)
      ()
    (let ((clause (car clauses))
          (rest (cdr clauses)))
      (if (eq? (car clause) t)
          `(do ,@(cdr clause))
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
                 (do ,@(cdr clause))
               (cond ,@rest))))))))

(defmacro case (x &body clauses)
  (let1 value (gensym)
    `(let1 ,value ,x
       (cond
        ,@(map (lambda (clause)
                 (cond ((eq? (car clause) t)
                        clause)
                       ((null? (cdar clause))
                        `((eq? ,value ',(caar clause)) ,@(cdr clause)))
                       (t `((member ,value ',(car clause)) ,@(cdr clause)))))
               clauses)))))

(defmacro and (&rest args)
  (if (null? args)
      t  ; (and) = true
    (if (null? (cdr args))
        (car args)
      `(if ,(car args)
           (and ,@(cdr args))
         nil))))

(defmacro or (&rest args)
  (if (null? (cdr args))
      (car args)
    (let1 g (gensym)
      `(let1 ,g ,(car args)
         (if ,g ,g
           (or ,@(cdr args)))))))

(defmacro do (&body body)
  (cond ((null? body) nil)
        ((null? (cdr body)) (car body))
        (t `(let ()
              ,@body))))

(defmacro aif (expr thn &body els)
  `(let1 it ,expr
     (if it ,thn ,@els)))

(defmacro awhen (expr &body body)
  `(aif ,expr
        (do ,@body)))

(defmacro awhile (expr &body body)
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
        (t (member x (cdr ls)))))

(defun assoc (x ls)
  (if (null? ls) nil
    (if (eq? x (caar ls)) (car ls)
      (assoc x (cdr ls)))))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun map (f ls)
  (if (null? ls)
      ()
    (cons (f (car ls))
          (map f (cdr ls)))))

(defun append (ls &rest rest)
  (cond ((null? rest) ls)
        ((null? ls)   (apply append rest))
        (t (cons (car ls)
                 (apply append (cdr ls) rest)))))

(defun reverse (ls)
  (let loop ((ls ls)
             (acc ()))
    (if (pair? ls)
        (loop (cdr ls)
              (cons (car ls) acc))
      acc)))

(defun list* (&rest args)
  (if (null? args)
      nil
    (if (null? (cdr args))
        (car args)
      (let loop ((p args)
                 (q (cdr args)))
        (if (null? (cdr q))
            (do (set-cdr! p (car q))
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

(defun vector->list (vect)
  (let loop ((i (- (vector-length vect) 1))
             (acc ()))
       (if (< i 0)
           acc
         (loop (- i 1) (cons (vector-ref vect i) acc)))))

(defun position-if (pred seq)
    (let loop ((p seq)
               (i 0))
         (when p
           (if (pred (car p))
               i
             (loop (cdr p) (+ i 1))))))

(defun take (n ls)
  (let loop ((n n)
             (ls ls)
             (acc nil))
       (if (or (<= n 0)
               (null? ls))
           (reverse! acc)
         (loop (- n 1) (cdr ls) (cons (car ls) acc)))))

(defun drop (n ls)
  (if (or (<= n 0)
          (null? ls))
      ls
    (drop (- n 1) (cdr ls))))

(defun elt (n ls)
  (car (drop n ls)))

(defun remove-if (test seq)
  (let loop ((seq seq)
             (acc ()))
       (if (null? seq)
           (reverse! acc)
         (loop (cdr seq)
               (if (test (car seq))
                   acc
                 (cons (car seq) acc))))))

(defmacro dotimes (params &body body)
  (let ((i (car params))
        (limit (gensym))
        (loop (gensym)))
    `(let1 ,limit ,(cadr params)
       (let ,loop ((,i 0))
            (if (< ,i ,limit)
                (do ,@body
                    (,loop (+ ,i 1)))
              ,(caddr params))))))

(defmacro dolist (pair &body body)
  (let ((i (car pair))
        (loop (gensym))
        (ls (gensym)))
    `(let ,loop ((,ls ,(cadr pair)))
          (let1 ,i (car ,ls)
            (when ,i
              ,@body
              (,loop (cdr ,ls)))))))

(defmacro labels (lss &body body)
  `(let ,(map (lambda (ls) (car ls)) lss)
     ,@(map (lambda (ls)
              `(set! ,(car ls) (lambda ,@(cdr ls))))
            lss)
     ,@body))
