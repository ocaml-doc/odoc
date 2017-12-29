.PHONY : build
build :
	jbuilder build --dev

.PHONY : test
test : build
	jbuilder runtest --no-buffer -j 1 --dev

COVERAGE := _coverage
BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY : coverage
coverage :
	BISECT_ENABLE=yes jbuilder build --dev test/parser/test.exe
	BISECT_ENABLE=yes jbuilder build --dev @html-tester
	(cd _build/default/test/parser && ./test.exe) || true
	(cd _build/default/test/html && ./test.exe) || true
	@bisect-ppx-report \
	    -I _build/default/ -html $(COVERAGE)/ \
	    -text - -summary-only \
	    $(BISECT_FILES_PATTERN)
	@echo See $(COVERAGE)/index.html

.PHONY : clean
clean :
	jbuilder clean
	rm -rf $(COVERAGE)
