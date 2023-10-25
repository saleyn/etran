PROJECT := $(notdir $(PWD))

all: compile

compile:
	rebar3 $@

clean:
	rm -fr doc ebin _build build-aux/*.{edoc,awk,css,sh}

distclean: clean
	rm -f build-aux/*.mk

docs doc:
	rebar3 ex_doc

test:
	ERL_LIBS= rebar3 eunit

publish:
	rebar3 hex $(if $(replace),publish --replace,cut)

info::
	@echo "make [compile]                             - Compile $(PROJECT)"
	@echo "make clean                                 - Clean all build artifacts of $(PROJECT)"
	@echo "make distclean                             - Clean all build artifacts of $(PROJECT) and build-aux/"
	@echo "make publish [replace=1]                   - Publish/replace the library to hex.pm"
	@echo "make debug module=Mod file=File            - Debug application of the Mod parse transform on the File"
	@echo "make debug-ui module=Mod file=File         - Start a UI debugger for the Mod parse transform on the File"

debug:
	@[ -z "$(module)" -o -z "$(file)" ] && echo "Run 'make $@ module=[erlpipe] file=FileName[.erl] [print=1]'" && exit 1 || true
	erlc +debug_info -pa _build/default/lib/etran/ebin -o _build/default/lib/etran/ebin src/$(module).erl
	erlc +debug_info $(DEBUG) +'{parse_transform,$(module)}' -pa _build/default/lib/etran/ebin \
		   $(if $(print),-D$(module)_orig -D$(module)_ast -D$(module)_src) -o _build/default/lib/etran/ebin $(basename $(file)).erl
	@cd _build/default/lib/etran/ebin && erl -pa . -eval 'decompiler:run("$(basename $(file)).beam"), halt(0).' -noinput
	@echo "===================================="
	@echo "Source:" && cat $(basename $(file)).erl
	@echo
	@echo "===================================="
	@echo "Result:" && cat _build/default/lib/etran/ebin/$(basename $(file)).erl
	@erl -pa _build/default/lib/etran/ebin -eval 'case proplists:get_value($(if $(fun),$(fun),test), $(basename $(file)):module_info(exports), -1) of -1 -> ok; _ -> io:format("\n\nOutput:\n ~p\n", [$(basename $(file)):$(if $(fun),$(fun),test)()]) end, halt(0).' -noinput
	@rm -f _build/default/lib/etran/ebin/$(basename $(file)).{erl,beam}

debug-ui: DBG=debugger:start(), i:iaa([break]), i:ii($(module)), i:ib($(module),parse_transform,2)
debug-ui: COMPILE=compile:file("$(basename $(file)).erl", [{d,$(module)_orig},{d,$(module)_ast},{d,$(module)_src},debug_info,{parse_transform,$(module)}])
debug-ui:
	[ -z "$(module)" -o -z "$(file)" ] && echo "Run 'make $@ module=[erlpipe] file=FileName[.erl]'" && exit 1 || true
	erl -pa _build/default/lib/etran/ebin -eval '$(DBG), $(COMPILE).'

.PHONY: test doc
.SUFFIXES:
