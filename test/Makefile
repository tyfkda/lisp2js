all:	test

test:	read-test inside-test js-test example-test

read-test:
	node read-test.js

inside-test:
	../jslisp test.lisp

js-test:
	node ./test.js

example-test:
	./example-test.sh
