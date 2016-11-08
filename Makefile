.PHONY: all deps clean release

all: compile

compile: deps
	./rebar -j8 compile

deps:
	./rebar -j8 get-deps

clean:
	./rebar -j8 clean

relclean:
	rm -rf rel/msbus_rpc_proxy

generate: compile
	cd rel && .././rebar -j8 generate

run: generate
	./rel/msbus_rpc_proxy/bin/msbus_rpc_proxy start

console: generate
	./rel/msbus_rpc_proxy/bin/msbus_rpc_proxy console

foreground: generate
	./rel/msbus_rpc_proxy/bin/msbus_rpc_proxy foreground

erl: compile
	erl -pa ebin/ -pa deps/*/ebin/ -s msbus_rpc_proxy

eunit:
	./rebar compile eunit	
