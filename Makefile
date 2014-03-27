.PHONY: deps, test

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test:
	@./rebar eunit skip_deps=true

run: all
	@./start.sh
