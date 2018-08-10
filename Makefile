.PHONY: all get_deps upd_deps compile clean purge test dialyzer build_plt

all: get_deps compile

get_deps:
	rebar3 get-deps

doc:
	rebar3 edoc

upd_deps:
	rebar3 upd-deps

compile:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 eunit apps=elarm

dialyzer: compile
	dialyzer --plt .plt --no_native ebin

build_plt:
	dialyzer --build_plt --output_plt .plt --apps erts stdlib kernel eunit ; [ -f ".plt" ]

purge:
	git ls-files -o --directory | xargs -r rm -r
