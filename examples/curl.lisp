;; Use "http" module to get HTTP resource.
(let ((url (if (null? *argv*)
               "http://www.example.com/"
             (car *argv*))))
  (let ((http (jsrequire "http")))
    (let1 h (http.get url (lambda (res)
                            (res.setEncoding "utf8")
                            (res.on "data" (lambda (chunk)
                                             (print chunk)))
                            ;;(res.on "end" (lambda (res)
                            ;;                ))
                            ))
      (h.on "error" (lambda (e)
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
