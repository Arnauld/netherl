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