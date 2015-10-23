(defun group (ls n)
  (if (<= n 0) (error "n <= 0")
    (let loop ((ls ls)
               (acc ()))
         (if ls
             (loop (drop n ls)
                   (cons (take n ls) acc))
           (reverse! acc)))))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(do ,@(map (^(pair)
                `(abbrev ,@pair))
              (group names 2))))

;; Test
(abbrevs plus +
         mul *)

(print (plus 1 (mul 2 3)))
