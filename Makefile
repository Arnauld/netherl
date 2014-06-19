#
# Maybe you have a file/directory named test in the directory. 
# If this directory exists, and has no dependencies that are 
# more recent, then this target is not rebuild.
# To force rebuild on these kind of not-file-related targets, 
# you should make them phony as follows:
.PHONY: all test clean

REBAR=./rebar

all: clean compile test analyze

compile:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@echo on
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

analyze:
	@dialyzer ebin/*.beam

init-analyze:
	@dialyzer --build_plt --apps erts kernel stdlib mnesia