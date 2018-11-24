(function () {
  // Run Lisp codes.
  var runCodes = function(codes) {
    var stream = LISP['make-string-input-stream'](codes);
    for (;;) {
      var s = LISP.read(stream);
      if (s === LISP.nil)
        break;
      LISP.eval(s);
    }
  };

  var codes = document.getElementById('code').value;
  runCodes(codes);

  var editor = CodeMirror.fromTextArea(document.getElementById('code'), {
    mode: 'scheme',
    theme: 'ambiance',
    matchBrackets: true,
    keyMap: 'emacs'
  });

  runTextAreaCode = function() {
    var codes = editor.getValue();
    runCodes(codes);
  };
})();
