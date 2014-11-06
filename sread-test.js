function print(value) {
  console.log(value);
}

function equals(x, y) {
  if (LISP['eq?'](x, y))
    return true;
  return (x instanceof LISP.Cons && y instanceof LISP.Cons &&
          equals(LISP.car(x), LISP.car(y)) &&
          equals(LISP.cdr(x), LISP.cdr(y)));
}

function test(title, expected, result) {
  process.stdout.write('Testing ' + title + '... ');
  if (equals(expected, result)) {
    print('ok');
    return;
  }

  console.error("\033[1;31m[ERROR]\033[0;39m");
  console.error("  expected " + expected + ' : actual ' + result);
  process.exit(1);
}

function main() {
  /*var LISP =*/ require('./lisp');
  /*var LISP =*/ require('./sread');
  var cons = LISP.cons;
  var reads = LISP['read-from-string'];

  test('integer', 123, reads('123'));
  test('nil', LISP.nil, reads('()'));
  test('single element list', LISP.cons(1, LISP.nil), reads('(1)'));
  test('multiple elements list', LISP.cons(1, LISP.cons(2, LISP.cons(3, LISP.nil))), reads('(1 2 3)'));
  test('line comment', 123, reads(';comment\n123'));
  test('symbol', LISP.intern('symbol'), reads('symbol'));
  test('quote', LISP.list(LISP.intern('quote'), LISP.intern('abc')), reads("'abc"));

  print("\033[1;32mTEST ALL SUCCEEDED!\033[0;39m")
}

main();
