all:	jslisp

RUNTIMES=runtime/lisp.js runtime/sread.js
SRCS=src/basic.scm src/backquote.scm src/lisp2js.scm

jslisp:	jslisp.js
jslisp.js:	$(RUNTIMES) $(SRCS)
	cat $(RUNTIMES) > $@
	cat $(SRCS) | gosh run-goshlisp.scm >> $@

test:	sread-test simple-test
	echo 'ok'

sread-test:
	cd runtime && node sread-test.js

simple-test:
	./test.sh
