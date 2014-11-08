if (typeof LISP === 'undefined')
  LISP = {};

LISP.Reader = function(str) {
  this.str = str;
};

LISP.NoCloseParenException = function() {};

LISP.Reader.prototype = {
  read: function() {
    var m;
    if (m = this.str.match(/^\s*([0-9]+)/))  // Number.
      return this.proceed(), parseInt(m[1]);
    if (m = this.str.match(/^\s*([0-9A-Za-z_\-+*\/%!?~^&<>=]+)/))  // Symbol.
      return this.proceed(), LISP.intern(m[1]);
    if (m = this.str.match(/^\s*\(/))  // Left paren '('.
      return this.proceed(), this.readList(RegExp.rightContext);
    if (m = this.str.match(/^\s*;[^\n]*\n?/))  // Line comment.
      return this.proceed(), this.read();
    if (m = this.str.match(/^\s*'/))  // quote.
      return this.proceed(), this.readQuote();
    if (m = this.str.match(/^\s*"((\\.|[^"\\])*)"/))  // string.
      return this.proceed(), this.unescape(m[1]);
    if (m = this.str.match(/^\s*`/))  // quasiquote.
      return this.proceed(), this.readQuasiQuote();
    if (m = this.str.match(/^\s*,(@?)/))  // unquote or unquote-splicing.
      return this.proceed(), this.readUnquote(m[1]);
    if (m = this.str.match(/^\s*#\/([^\/]*)\//))  // regexp TODO: Implement properly.
      return this.proceed(), new RegExp(m[1]);
    if (m = this.str.match(/^\s*#(t|f)\b/))  // #t, #f
      return this.proceed(), (m[1] == 't' ? LISP.t : LISP.nil);
    return undefined;
  },

  proceed: function(value) {
    this.str = RegExp.rightContext;
  },

  readList: function() {
    var result = LISP.nil;
    var m;
    for (;;) {
      var x = this.read();
      if (x !== undefined) {
        result = LISP.cons(x, result);
        continue;
      }

      if (m = this.str.match(/^\s*\)/)) {  // Close paren.
        this.proceed();
        return LISP['reverse!'](result);
      }
      if (m = this.str.match(/^\s*\.\s/)) {  // Dot.
        this.proceed();
        var last = this.read();
        if (last !== undefined) {
          if (m = this.str.match(/^\s*\)/)) {  // Close paren.
            this.proceed();
            var reversed = LISP['reverse!'](result);
            result.cdr = last;
            return reversed;
          }
        }
      }
      // Error
      throw new LISP.NoCloseParenException();
    }
  },

  readQuote: function() {
    return LISP.list(LISP.intern('quote'), this.read());
  },

  readQuasiQuote: function() {
    return LISP.list(LISP.intern('quasiquote'), this.read());
  },

  readUnquote: function(splicing) {
    var keyword = splicing === '@' ? 'unquote-splicing' : 'unquote';
    return LISP.list(LISP.intern(keyword), this.read());
  },

  unescape: function(str) {
    return str.replace(/\\./g, function(match) {
      switch (match[1]) {
      case 't':  return '\t';
      case 'n':  return '\n';
      default:  return match[1];
      }
    });
  },
};

LISP["read-from-string"] = function(str) {
  var reader = new LISP.Reader(str);
  return reader.read();
};

module.exports = LISP;
