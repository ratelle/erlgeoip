REBAR=./rebar

all: deps compile xref

deps:
	@$(REBAR) update-deps
	@$(REBAR) get-deps

xref:
	@$(REBAR) skip_deps=true xref

compile:
	$(REBAR) compile

test:  compile
	@$(REBAR) skip_deps=true ct

clean:
	@rm -rf deps
	@$(REBAR) clean

.PHONY: all deps compile clean

