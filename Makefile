.PHONY: all deps clean release

all: compile

compile: deps
	./rebar -j8 compile

deps:
	./rebar -j8 get-deps

clean:
	./rebar -j8 clean

relclean:
	rm -rf rel/msgbus_rpc_proxy

generate: compile
	cd rel && .././rebar -j8 generate

run: generate
	./rel/msgbus_rpc_proxy/bin/msgbus_rpc_proxy start

console: generate
	./rel/msgbus_rpc_proxy/bin/msgbus_rpc_proxy console

foreground: generate
	./rel/msgbus_rpc_proxy/bin/msgbus_rpc_proxy foreground

erl: compile
	erl -pa ebin/ -pa deps/*/ebin/ -s msgbus_rpc_proxy

eunit:
	./rebar compile eunit	
