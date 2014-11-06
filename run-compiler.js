(function() {
  require('./compiler');

  if (process.argv.length < 3) {
    console.error('arguments required');
    process.exit(1);
  }

  var fs = require('fs');
  var fileName = process.argv[2];
  fs.readFile(fileName, 'utf-8', function(error, text) {
    if (error) {
      console.error('File open error [' + fileName + ']: ' + error);
      process.exit(1);
    }

    var reader = new LISP.SReader(text);
    for (;;) {
      var s = reader.read();
      if (!s)
        break;

      var compiled = LISP.compile(s, LISP.nil);
      LISP.jseval(compiled);
    }
  });
})();
