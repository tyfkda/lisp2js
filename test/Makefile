all:	test

test:	read-test basic-test compiler-test js-test example-test

read-test:
	node read-test.js

basic-test:
	../jslisp basic-test.lisp

compiler-test:
	../jslisp compiler-test.lisp

js-test:
	node ./test.js

example-test:
	./example-test.sh
