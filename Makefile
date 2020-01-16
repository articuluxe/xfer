EMACS=$(VISUAL) -nw
ROOT=$(HOME)/.emacs.d
DEPS=-L . -L $(ROOT)/plugins -L $(ROOT)/elisp
ELC := $(patsubst %.el,%.elc,$(wildcard *.el))
rwildcard=$(wildcard $1$2)$(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))
TESTS := $(call rwildcard,test/,test_*.el)
ifdef COMSPEC
	BLANK = echo.
else
	BLANK = echo -e
endif

check: $(TESTS)

$(TESTS):
	@$(BLANK)
	@$(BLANK)
	@echo Running -*- $@ -*-
	@$(BLANK)
	$(EMACS) -Q $(DEPS) -batch -l $@ -f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) -Q -batch $(DEPS) -f batch-byte-compile $<

compile: $(ELC)

clean:
	rm -f $(ELC)

.PHONY: compile clean test $(TESTS) check
