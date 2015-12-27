(defmacro test (name expect actual)
  `(progn (print (string-append ,name
                                " ... "))
          (let ((expect-value ,expect)
                (actual-value ,actual))
            (if (equal? expect-value actual-value)
                (print "ok\n")
              (progn (print (string-append "ERROR [" (x->string expect-value) "] expected, but [" (x->string actual-value) "]\n"))
                     (exit 1))))))
