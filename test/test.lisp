(defmacro test (name expect actual)
  `(do (print (string-append ,name
                             " ... "))
       (let ((expect-value ,expect)
             (actual-value ,actual))
         (if (equal? expect-value actual-value)
             (print "ok\n")
           (do (print (string-append "ERROR [" expect-value "] expected, but [" actual-value "]\n"))
               (exit 1))))))
