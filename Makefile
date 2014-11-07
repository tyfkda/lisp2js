all:	jslisp

jslisp:	jslisp.js
jslisp.js:	lisp.js sread.js basic.scm lisp2js.scm
	cat lisp.js sread.js > $@
	cat basic.scm lisp2js.scm | gosh lisp2js.scm >> $@

test:	sread-test simple-test
	echo 'ok'

sread-test:
	node sread-test.js

simple-test:
	./test.sh
