/*
  Usage:
    <script src="jslisp.js">
      (print (+ 1 2))
    </script>
 */

(function() {
  // Run Lisp codes.
  var runCodes = function(codes) {
    var reader = new LISP.SReader(codes);
    for (;;) {
      var s = reader.read();
      if (s === undefined)
        break;
      LISP.eval(s);
    }
  };

  var scriptTags = document.getElementsByTagName('script');
  var myScriptTag = scriptTags[scriptTags.length - 1];
  var codes = myScriptTag.text;
  runCodes(codes);
})();
