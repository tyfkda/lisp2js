;; Use "http" module to get HTTP resource.
(let ((url (if (null? *argv*)
               "http://www.example.com/"
             (car *argv*))))
  (let ((http (jsrequire "http")))
    (let1 h (http.get url (^(res)
                            (res.setEncoding "utf8")
                            (res.on "data" (^(chunk)
                                             (print chunk)))
                            ;;(res.on "end" (^(res)
                            ;;                ))
                            ))
      (h.on "error" (^(e)
                      (print e.message))))))

#|
// Send HTTP request on Node.js
var http = require('http');
var url = 'http://www.example.com/';
http.get(url, function(res) {
  res.setEncoding('utf8');
  res.on('data', function(chunk) {
    process.stdout.write(chunk)
  });
  res.on('end', function(res) {
    // Do something.
  });
}).on('error', function(e) {
  console.log(e.message);
});
|#
