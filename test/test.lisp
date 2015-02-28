(defmacro test (name expect actual)
  `(do (print (string-append ,name
                             " ... "))
       (let ((expect-value ,expect)
             (actual-value ,actual))
         (if (equal? expect-value actual-value)
             (print "ok\n")
           (do (print (string-append "ERROR [" (x->string expect-value) "] expected, but [" (x->string actual-value) "]\n"))
               (exit 1))))))
