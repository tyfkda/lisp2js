(let1 h (make-hash-table)
  ;; Hash table to JSON string.
  (hash-table-put! h 'a 1)
  (hash-table-put! h 'b #(2 "3" 4))
  (let1 json-string (JS.JSON.stringify h)
    (print (string-append json-string "\n"))

    ;; JSON string to object.
    (let1 json (JS.JSON.parse json-string)
      (print (string-append "a = " json.a
                            ", b = " json.b
                            "\n")))))
