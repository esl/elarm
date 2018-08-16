.PHONY: all get_deps upd_deps compile clean purge test dialyzer build_plt

all: get_deps compile

doc:
	rebar3 edoc

compile:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 eunit apps=elarm

dialyzer: compile
	rebar3 dialyzer 
