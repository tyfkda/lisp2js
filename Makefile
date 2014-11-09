all:	update-compiler

SRCS=src/basic.scm src/backquote.scm src/lisp2js.scm
TMPFN=,lisp2js.js

clean:
	rm -rf lisp2js-old.js $(TMPFN)

update-compiler:	lisp2js.js
lisp2js.js:	$(SRCS)
	echo '// DO NOT EDIT, this file is generated from src/*.scm' > $(TMPFN)
	./run-jslisp -c $(SRCS) >> $(TMPFN)
	mv $(TMPFN) $@

lisp2js-old.js:	$(SRCS)
	echo '// DO NOT EDIT, this file is generated from src/*.scm' > $@
	cat $(SRCS) | gosh run-goshlisp.scm >> $@

test:	sread-test simple-test
	echo 'ok'

sread-test:
	cd test && node sread-test.js

simple-test:
	./test.sh
