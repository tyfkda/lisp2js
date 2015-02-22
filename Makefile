all:	update-compiler

SRCS=src/basic.lisp src/backquote.lisp src/lisp2js.lisp
TMPFN=,lisp2js.js

clean:
	rm -rf lisp2js-old.js $(TMPFN)

update-compiler:	lisp2js.js
lisp2js.js:	$(SRCS) src/runtime/runtime.js
	make $(TMPFN)
	ruby -e 'marker = "/*==== EMBED COMPILED CODE HERE ====*/"; compiled_code = File.read("$(TMPFN)"); runtime = File.read("src/runtime/runtime.js"); print runtime.sub(marker) { marker + "\n" + compiled_code };' > $@
	rm $(TMPFN)

$(TMPFN):	$(SRCS)
	rm -f $@
	for fn in $(SRCS) ; do \
	  ./jslisp -c $$fn >> $@ ; \
	done

lisp2js-test.js:	$(SRCS) src/runtime/runtime.js
	make $(TMPFN)
	ruby -e 'marker = "/*==== EMBED COMPILED CODE HERE ====*/"; compiled_code = File.read("$(TMPFN)"); runtime = File.read("src/runtime/runtime.js"); print runtime.sub(marker) { marker + "\n" + compiled_code };' > $@
	rm $(TMPFN)

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
