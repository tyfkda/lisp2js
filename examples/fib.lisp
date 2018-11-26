(defun fib (n)
  (if (< n 2)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(let1 n (string->number (or (car *argv*)
                            "20"))
  (puts (fib n)))
