all: compile

compile:
	rebar3 $@

test:
	ERL_LIBS= rebar3 eunit

publish:
	rebar3 hex publish $(if $(replace),--replace)

debug:
	erlc +debug_info -Derlpipe_debug +'{parse_transform,erlpipe}' -pa _build/default/lib/etran/ebin -o _build/default/lib/etran/ebin $(file).erl
	@cd _build/default/lib/etran/ebin && erl -pa . -eval 'decompiler:run("$(file).beam"), halt(0).' -noinput
	@echo "===================================="
	@echo "Source:" && cat $(file).erl
	@echo
	@echo "===================================="
	@echo "Result:" && cat _build/default/lib/etran/ebin/$(file).erl
	@erl -pa _build/default/lib/etran/ebin -eval 'io:format("\n\nOutput:\n"), $(file):test(), halt(0).' -noinput
	@rm -f _build/default/lib/etran/ebin/$(file).{erl,beam}

.PHONY: test
