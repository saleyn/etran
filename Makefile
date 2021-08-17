PROJECT := $(notdir $(PWD))

ifeq (, $(shell [ -f build-aux/docs-addon.mk ] && echo ok))
all: get-addon compile
else
all: compile
endif

-include build-aux/docs-addon.mk

get-addon:
	@echo "Fetching build-aux/docs-addon.mk" && \
		mkdir -p build-aux && \
		curl -s -o build-aux/docs-addon.mk https://raw.githubusercontent.com/saleyn/util/master/build-aux/docs-addon.mk

compile:
	rebar3 $@

clean:
	rm -fr doc ebin _build build-aux/*.{edoc,mk,awk,css,sh}

test:
	ERL_LIBS= rebar3 eunit

publish:
	rebar3 hex publish $(if $(replace),--replace)

set-version:
	@[ -z $(version) ] && echo "Missing version=X.Y.Z!" && exit 1 || true
	@sed -i "s/{$(PROJECT), \"[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\"}/{$(PROJECT), \"$(version)\"}/" rebar.config
	@sed -i "s/{vsn, \"[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\"}/{vsn, \"$(version)\"}/" src/$(PROJECT).app.src

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
