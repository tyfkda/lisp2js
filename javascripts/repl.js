$(function() {

  var prompt = '<div class="prompt">jsl&gt;</div>';

  var holder = $('#holder');
  var txt = $('#repl-txt');
  txt.html(';; JsLisp<br>' + prompt);

  var cm = CodeMirror(holder[0], {
    value: '',
    mode: "clojure",
    theme: "ambiance",
    matchBrackets: true,
    keyMap: 'emacs'
  });

  function onenter(code) {
    var s;
    try {
      var reader = new LISP.Reader(code);
      s = reader.read();
    } catch (e) {
      if (e instanceof LISP.NoCloseParenException) {
        cm.execCommand('indentAuto');
        return;  // continue input.
      }
    }

    if (s === undefined)
      return undefined;
    var result;
    try {
      result = LISP.eval(s);
    } catch (e) {
      result = e;
    }

    cm.setValue('');

    var org = txt.html();
    var _new = ('<div class="float: left"><pre class="code" style="float: left">' + code + '</pre></div>' +
                '<div style="clear: both"></div>' +
                LISP.makeString(result, 10) + '<br>');

    var nss = '';
    txt.html(org + _new + prompt);
    var last_pos = holder.position();
    var next_pos = {
      'top':  txt.height() - 22 - 5,
      'left': 50 + (nss.length * 10)
    };
    holder.css(next_pos);
  }

  var last_len = 0;
  cm.on('change', function onchange() {
    var v = cm.getValue();
    var len = v.length;
    if (last_len < len && v[len-1] === '\n') {
      cm.off('change', onchange);
      onenter(v);
      cm.on('change', onchange);
    }
    last_len = len;
  });

  cm.focus();
});
