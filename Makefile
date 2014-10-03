.PHONY: all get_deps upd_deps compile clean purge test dialyzer build_plt

all: get_deps compile

get_deps:
	rebar get-deps

doc:
	rebar doc

upd_deps:
	rebar upd-deps

compile:
	rebar compile

clean:
	rebar clean

test:
	rebar eunit apps=elarm

dialyzer: compile
	dialyzer --plt .plt --no_native ebin

build_plt:
	dialyzer --build_plt --output_plt .plt --apps erts stdlib kernel eunit ; [ -f ".plt" ]

purge:
	git ls-files -o --directory | xargs -r rm -r
