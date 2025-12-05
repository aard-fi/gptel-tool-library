#ALL: module-tests functional-tests
ALL: module-tests

.PHONY: module-tests test

TESTS =
TESTS += gtl-module-elisp.el
#TESTS += gtl-search-and-replace.el

functional-tests:
	@emacs -batch -Q --eval "(progn\
	(load-file \"test-init.el\")\
	(load-file \"bug-tests.el\")\
	(ert-run-tests-batch-and-exit))"

module-tests: $(TESTS)
	@for test in $^; do\
	  echo ">> Running tests from $$test";\
	  emacs -batch -Q --eval "(progn \
	    (load-file \"test-init.el\")\
	    (load-file \"$$test\")\
	    (ert-run-tests-batch-and-exit))";\
	  if [ $$? -ne 0 ]; then exit 1; fi ;\
	done
