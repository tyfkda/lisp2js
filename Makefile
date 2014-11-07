all:	jslisp

SRCS=src/basic.scm src/backquote.scm src/lisp2js.scm

clean:
	rm -rf lisp2js.js

jslisp:	lisp2js.js
lisp2js.js:	$(SRCS)
	cat $(SRCS) | gosh run-goshlisp.scm > $@

test:	sread-test simple-test
	echo 'ok'

sread-test:
	cd runtime && node sread-test.js

simple-test:
	./test.sh
