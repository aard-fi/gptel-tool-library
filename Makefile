EMACS=emacs

ALL: test

.PHONY: clean lisp tests

clean:
	@$(MAKE) -C lisp clean

%.elc: %.el
	emacs -batch -Q -L . -f batch-byte-compile $<

byte-compile:
	@$(EMACS) -Q -L . --batch -f batch-byte-compile *.el

lisp:
	@$(MAKE) -C lisp

test:
	@$(MAKE) -C t
