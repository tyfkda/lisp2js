all:	jslisp

jslisp:	jslisp.js
jslisp.js:	lisp.js sread.js basic.scm backquote.scm lisp2js.scm
	cat lisp.js sread.js > $@
	cat basic.scm backquote.scm lisp2js.scm | gosh run-goshlisp.scm >> $@

test:	sread-test simple-test
	echo 'ok'

sread-test:
	node sread-test.js

simple-test:
	./test.sh
