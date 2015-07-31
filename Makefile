REBAR = $(shell pwd)/rebar3

.PHONY: rel package quick-test

quick-test: cp-hooks
	-$(REBAR) compile 
	$(REBAR) eunit

all: cp-hooks compile

cp-hooks:
	cp hooks/* .git/hooks

apps/chunter/priv/zonedoor: utils/zonedoor.c
	gcc -lzdoor utils/zonedoor.c -o apps/chunter/priv/zonedoor

apps/chunter/priv/runpty: utils/runpty.c
	gcc utils/runpty.c -o apps/chunter/priv/runpty

version:
	echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > chunter.version

version_header: version
	cp chunter.version rel/files/chunter.version
	echo "-define(VERSION, <<\"$(shell cat chunter.version)\">>)." > apps/chunter/src/chunter_version.hrl

package: rel
	make -C rel/pkg package

compile: apps/chunter/priv/runpty apps/chunter/priv/zonedoor version_header
	$(REBAR) compile

clean:
	$(REBAR) clean
	make -C rel/pkg clean

test: all
	$(REBAR) skip_deps=true xref
	$(REBAR) skip_deps=true eunit

rel: all
	-rm -r ./rel/chunter/share
	$(REBAR) as prod release

###
### Docs
###
docs:
	$(REBAR) skip_deps=true doc

##
## Developer targets
##

stage : rel
	$(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf rel/chunter/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/chunter/lib;)

##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.chunter_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin | grep -v -f dialyzer.mittigate

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
