all:	test

.PHONY:	read-test basic-test parser-test compiler-test js-test example-test

test:	read-test basic-test parser-test compiler-test js-test example-test

read-test:
	node read-test.js

basic-test:
	../jslisp basic-test.lisp

parser-test:
	../jslisp parser-test.lisp

compiler-test:
	../jslisp compiler-test.lisp

js-test:
	node ./js-test.js

example-test:
	./example-test.sh
