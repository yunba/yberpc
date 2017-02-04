.PHONY: all deps clean release

all: compile

compile: deps
	./rebar -j8 compile

deps:
	./rebar -j8 get-deps

clean:
	./rebar -j8 clean

relclean:
	rm -rf rel/yberpc

generate: compile
	cd rel && .././rebar -j8 generate

run: generate
	./rel/yberpc/bin/yberpc start

console: generate
	./rel/yberpc/bin/yberpc console

foreground: generate
	./rel/yberpc/bin/yberpc foreground

erl: compile
	erl -pa ebin/ -pa deps/*/ebin/ -s yberpc

test: generate
	ERL_AFLAGS="-config ${PWD}/rel/yberpc/etc/app.config" ./rebar compile ct suite=yberpc skip_deps=true

