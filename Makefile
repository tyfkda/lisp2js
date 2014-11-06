all:	test

test:	sread-test simple-test
	echo 'ok'

sread-test:
	node sread-test.js

simple-test:
	./test.sh
