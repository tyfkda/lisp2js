(defmacro run-test (&body body)
  `(let ((success-count 0)
         (error-count 0))
     (labels ((test (name expect actual)
                    (print (string-append name
                                          " ... "))
                    (if (equal? expect actual)
                        (progn (print "ok\n")
                               (set! success-count (+ success-count 1)))
                      (progn (print (string-append "\x1b[1;31mERROR ["
                                                   expect
                                                   "] expected, but ["
                                                   actual
                                                   "]\x1b[0;39m\n"))
                             (set! error-count (+ error-count 1))))))

       ,@body

       (if (> error-count 0)
           (progn (print (string-append "\x1b[1;31m"
                                        error-count
                                        "/"
                                        (+ success-count error-count)
                                        " tests failed\x1b[0;39m\n"))
                  (exit 1))
         (progn (print (string-append "\x1b[1;32mALL "
                                      success-count
                                      " TESTS SUCCEEDED!\x1b[0;39m\n"
                                      )))))))
