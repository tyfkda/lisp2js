/*
  Usage:
    <script src="jslisp.js">
      (print (+ 1 2))
    </script>
 */

(function() {
  // Get called script node.
  var getMyScriptTag = function (e) {
    if (e.id == '_firebugConsole')
      return getMyScriptTag(document.body);
    if (e.nodeName.toLowerCase() == 'script')
      return e;
    return getMyScriptTag(e.lastChild);
  };

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

  var scriptTag = getMyScriptTag(document);
  var codes = scriptTag.innerHTML;
  runCodes(codes);
})();
