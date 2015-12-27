all:	update-compiler

SRCS=src/basic.lisp src/backquote.lisp src/lisp2js.lisp
TMPFN=runtime.js

clean:
	rm -rf lisp2js-old.js $(TMPFN)

release:	lisp2js.min.js

update-compiler:	lisp2js.js
lisp2js.js:	$(SRCS) src/runtime/runtime.js
	make ,$(TMPFN)
	./jslisp tools/embed-compiled.lisp ,$(TMPFN) < src/runtime/runtime.js > $(TMPFN)
	gulp babel
	rm $(TMPFN) ,$(TMPFN)

lisp2js.min.js:	lisp2js.js
	gulp uglify

,$(TMPFN):	$(SRCS)
	rm -f $@
	for fn in $(SRCS) ; do \
	  ./jslisp -c $$fn >> $@ ; \
	done

.PHONY:	test

test:
	make -C test

src/runtime/runtime.min.js:	src/runtime/runtime.js
	uglifyjs -c -o $@ --source-map $<.map $<

update-gh-pages:
	git checkout gh-pages && \
	git checkout master -- lisp2js.js && mv lisp2js.js js/ && \
	git commit -am 'Update runtime' && \
	git checkout master
