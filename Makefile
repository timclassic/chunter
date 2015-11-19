.PHONY: deps rel package quick-test tree

all: apps/chunter/priv/zonedoor compile version_header

include fifo.mk

# We need to overwrite teh pre-commit form fifo.mk
# since chunter can't compile all deps on every platform
# the kstat library will not compile on OS X
pre-commit:
	-$(REBAR) compile
	$(ELVIS) rock
	$(REBAR) xref
	$(REBAR) eunit

apps/chunter/priv/zonedoor: utils/zonedoor.c
# Only copile the zonedoor under sunus
	[ $(shell uname) != "SunOS" ] && true || gcc -lzdoor utils/zonedoor.c -o apps/chunter/priv/zonedoor

version:
	echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > chunter.version

version_header: version
	cp chunter.version rel/files/chunter.version
	echo "-define(VERSION, <<\"$(shell cat chunter.version)\">>)." > apps/chunter/src/chunter_version.hrl

package: rel
	make -C rel/pkg package

clean:
	$(REBAR) clean
	make -C rel/pkg clean

rel: all
	-rm -r ./rel/chunter/share
	$(REBAR) as prod release
