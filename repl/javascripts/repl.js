$(function() {
  'use strict'

  var prompt = '<div class="prompt">jsl&gt;</div>';

  var holder = $('#holder');
  var txt = $('#repl-txt');
  txt.html(';; JsLisp version ' + escape(LISP['*version*']) + '<br>' + prompt);

  var cm = CodeMirror(holder[0], {
    value: '',
    mode: 'scheme',
    theme: 'ambiance',
    matchBrackets: true,
    keyMap: 'emacs'
  });

  function escape(string) {
    return string.replace(/[&'`"<>]/g, function(match) {
      return {
        '&': '&amp;',
        "'": '&#x27;',
        '`': '&#x60;',
        '"': '&quot;',
        '<': '&lt;',
        '>': '&gt;',
      }[match]
    });
  }

  function onenter(code) {
    var s;
    try {
      var stream = LISP['make-string-input-stream'](code);
      s = LISP.read(stream);
    } catch (e) {
      if (e instanceof LISP.NoCloseParenException) {
        cm.execCommand('indentAuto');
        return;  // continue input.
      }
    }

    if (s === LISP.nil)
      return undefined;
    var result;
    try {
      result = LISP.eval(s);
    } catch (e) {
      result = e;
    }

    cm.setValue('');

    var org = txt.html();
    var _new = ('<div class="float: left"><pre class="code" style="float: left">' + escape(code) + '</pre></div>' +
                '<div style="clear: both"></div>' +
                escape(LISP['x->string'](result, 10)) + '<br>');

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
