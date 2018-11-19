(register-macro 'defmacro
  (lambda (name params &body body)
    `(register-macro ',name
       (lambda ,params ,@body))))

(defun macroexpand (exp)
  (let ((expanded (macroexpand-1 exp)))
    (if (equal? expanded exp)
        exp
      (macroexpand expanded))))

;;;; Reader macros

(def *readtable* (make-hash-table))
(hash-table-put! *readtable* '_dispatch-table (make-hash-table))

(defun set-macro-character (c fn)
  (hash-table-put! *readtable* c fn)
  t)

(defun set-dispatch-macro-character (c c2 fn)
  (flet ((dispatch-macro-character (stream cc)
           (let ((cc2 (read-char stream))
                 (table (hash-table-get (hash-table-get *readtable* '_dispatch-table) cc)))
             (unless (hash-table-exists? table cc2)
               (unread-char cc2 stream)
               (error "No dispatch macro character" cc cc2))
             ((hash-table-get table cc2) stream cc cc2))))
    (let1 dtable (hash-table-get *readtable* '_dispatch-table)
      (unless (hash-table-exists? dtable c)
        (hash-table-put! dtable c (make-hash-table)))
      (hash-table-put! (hash-table-get dtable c) c2 fn)
      (hash-table-put! *readtable* c dispatch-macro-character)
      t)))

;; Line comment
(set-macro-character ";"
  (lambda (stream _)
    (let loop ()
         (let1 c (read-char stream)
           (cond ((null? c) c)  ; eof
                 ((eq? c "\n") (read stream))  ; EOL
                 (t (loop)))))))  ; Continue

;; Quote
(set-macro-character "'"
  (lambda (stream _)
    (list 'quote (read stream))))

;; Backquote
(set-macro-character "`"
  (lambda (stream _)
    (list `quasiquote (read stream))))

;; Comma
(set-macro-character ","
  (lambda (stream _)
    (let ((c2 (read-char stream)))
      (if (eq? c2 "@")
          (list 'unquote-splicing (read stream))
        (progn (unread-char c2 stream)
               (list 'unquote (read stream)))))))

;; String literal
(set-macro-character "\""
  (lambda (stream _)
    (flet ((unescape (c)
             (case c
               (("x")
                (let* ((c1 (read-char stream))
                       (c2 (read-char stream)))
                  (number->char (string->number (string-append c1 c2) 16))))
               (("t") "\t")
               (("n") "\n")
               (t c))))
      (let loop ((acc '()))
           (let1 c (read-char stream)
             (cond ((null? c) (error "NoCloseQuoteException"))
                   ((eq? c "\"") (string-join (reverse! acc) ""))
                   ((eq? c "\\")
                    (loop (cons (unescape (read-char stream)) acc)))
                   (t (loop (cons c acc)))))))))

;; List
(set-macro-character "("
  (lambda (stream _)
    (read-delimited-list ")" stream t)))

;; Block comment.
(set-dispatch-macro-character "#" "|"
  (lambda (stream _c1 _c2)
    (let loop ((c1 nil))
         (let1 c2 (read-char stream)
           (cond ((null? c2) (error "Block comment not closed"))
                 ((and (eq? c1 "|")
                       (eq? c2 "#"))
                  (read stream))
                 (t (loop c2)))))))

;; Vector literal.
(set-dispatch-macro-character "#" "("
  (lambda (stream _c1 _c2)
    (apply vector (read-delimited-list ")" stream nil))))

;; Regexp.
(set-dispatch-macro-character "#" "/"
  (lambda (stream _c1 _c2)
    (let loop ((cs ()))
         (let1 c (read-char stream)
           (cond ((eq? c "/")
                  (regexp (string-join (reverse! cs) "")))
                 ((or (null? c)
                      (eq? c "\n"))
                  (error "Regexp not terminated"))
                 (t (loop (cons c cs))))))))

;; Read time evaluation.
(set-dispatch-macro-character "#" "."
  (lambda (stream _c1 _c2)
    (eval (read stream))))

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
      `(progn ,@body)
    `(let1 ,(caar pairs) ,(cadar pairs)
       (let* ,(cdr pairs)
         ,@body))))

(defmacro when (pred &body body)
  `(if ,pred
       (progn ,@body)))

(defmacro unless (pred &body body)
  `(if ,pred
       nil
     (progn ,@body)))

(defmacro cond (&body clauses)
  (if (null? clauses)
      ()
    (let ((clause (car clauses))
          (rest (cdr clauses)))
      (cond ((eq? (car clause) t)
             `(progn ,@(cdr clause)))
            ((null? (cdr clause))  ; cond ((foo))
             (let ((g (gensym)))
               `(let ((,g ,(car clause)))
                  (if ,g ,g
                    (cond ,@rest)))))
            ((eq? (cadr clause) '=>)  ; cond ((foo) => bar)
             (let ((g (gensym)))
               `(let ((,g ,(car clause)))
                  (if ,g (,(caddr clause) ,g)
                    (cond ,@rest)))))
            (t `(if ,(car clause)  ; otherwise
                    (progn ,@(cdr clause))
                  (cond ,@rest)))))))

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

(defmacro progn (&body body)
  (cond ((null? body) nil)
        ((null? (cdr body)) (car body))
        (t `(let ()
              ,@body))))

(defmacro aif (expr thn &body els)
  `(let1 it ,expr
     (if it ,thn ,@els)))

(defmacro awhen (expr &body body)
  `(aif ,expr
        (progn ,@body)))

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

(defun member (x ls)
  (cond ((null? ls) nil)
        ((eq? x (car ls)) ls)
        (t (member x (cdr ls)))))

(defun assoc (x ls)
  (cond ((null? ls) nil)
        ((eq? x (caar ls)) (car ls))
        (t (assoc x (cdr ls)))))

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
            (progn (set-cdr! p (car q))
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
                (progn ,@body
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

(defmacro flet (lss &body body)
  `(let ,(map (lambda (ls)
                `(,(car ls)
                  (lambda ,@(cdr ls))))
              lss)
     ,@body))

(defmacro labels (lss &body body)
  `(let ,(map (lambda (ls) (car ls)) lss)
     ,@(map (lambda (ls)
              `(set! ,(car ls) (lambda ,@(cdr ls))))
            lss)
     ,@body))

(defun skip-whitespaces (stream)
  (let loop ()
       (let1 c (read-char stream)
         (case c
           ((nil)  nil)
           ((" " "\t" "\n")  (loop))
           (t (unread-char c stream)
              c)))))

(defun read-delimited-list (delim stream enable-dot?)
  (let1 stream (or stream *stdin*)
    (flet ((make-dotted (acc stream)
             (read-char stream)  ;; Drop "."
             (skip-whitespaces stream)
             (let1 elem (read stream)
               (if (not (eq? (skip-whitespaces stream) delim))
                   (error "no close paren for dotted pair")
                 (let1 result (reverse! acc)
                   (set-cdr! acc elem)
                   (read-char stream)  ;; Drop delimitor
                   result)))))
      (let loop ((acc '()))
           (let1 c (skip-whitespaces stream)
             (cond ((null? c)  (error "list not closed by delimitor" delim))
                   ((eq? c delim)
                    (read-char stream)  ;; Drop delimitor
                    (reverse! acc))
                   ((and enable-dot? (eq? c "."))  ;; Dotted list.
                    (make-dotted acc stream))
                   (t (loop (cons (read stream) acc)))))))))

(defmacro deftype? (&rest types)
  `(progn
     ,@(map (lambda (tt)
              `(defun ,(intern (string-append tt "?")) (x)
                 (eq? (type x) ',tt)))
            types)))

(deftype? symbol pair number string keyword vector table regexp)

(defun reverse! (x)
  (let loop ((rev '())
             (ls x))
       (if (pair? ls)
           (let1 d (cdr ls)
             (set-cdr! ls rev)
             (loop ls d))
         rev)))

(defun read-from-string (str)
  (read (make-string-input-stream str)))
