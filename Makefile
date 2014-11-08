all:	jslisp

SRCS=src/basic.scm src/backquote.scm src/lisp2js.scm

clean:
	rm -rf lisp2js.js lisp2js2.js

jslisp:	lisp2js.js
lisp2js.js:	$(SRCS)
	echo '// DO NOT EDIT, this file is generated from src/*.scm' > $@
	cat $(SRCS) | gosh run-goshlisp.scm >> $@

lisp2js2.js:	$(SRCS)
	echo '// DO NOT EDIT, this file is generated from src/*.scm' > $@
	./run-jslisp -c $(SRCS) >> $@

test:	sread-test simple-test
	echo 'ok'

sread-test:
	cd runtime && node sread-test.js

simple-test:
	./test.sh
