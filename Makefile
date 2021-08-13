all: compile

compile:
	rebar3 $@

test:
	rebar3 eunit

publish:
	rebar3 hex publish $(if $(replace),--replace)

test2:
	erlc +debug_info -o ebin src/erlpipe.erl
	erlc +debug_info -Derlpipe_debug -pa ebin -o ebin/ t.erl
	@cd ebin && erl -pa . -eval 'decompiler:run("t.beam"), halt(0).' -noinput
	@echo "===================================="
	@echo "Source:" && cat t.erl
	@echo
	@echo "===================================="
	@echo "Result:" && cat ebin/t.erl
	@erl -pa ebin -eval 'io:format("\n\nOutput:\n"), t:t(1), halt(0).' -noinput

.PHONY: test
