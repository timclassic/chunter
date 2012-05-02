OBJ=ebin/chunter.app ebin/chunter_app.beam ebin/chunter_server.beam ebin/chunter_sup.beam ebin/chunter.beam
DEPS=deps/jsx deps/alogger
ERL=erl
PA=ebin deps/*/ebin
REBAR=./rebar

all: $(OBJ) $(DEPS)

rel: all FORCE
	-rm -r rel/chunter
	cd rel; ../rebar generate

tar: rel
	cd rel; tar jcvf chunter.tar.bz2 chunter

clean: FORCE
	-rm -r *.beam ebin
	-rm erl_crash.dump
	-rm -r rel/chunter
	-rm rel/chunter.tar.bz2

deps/jsx:
	$(REBAR) get-deps

deps/alogger:
	$(REBAR) get-deps

deps/lhttpc:
	$(REBAR) get-deps

ebin/%.app: src/%.app.src
	$(REBAR) compile

ebin/%.beam: src/%.erl
	$(REBAR) compile

shell: all
	$(ERL) -pa $(PA) -config standalone.config
	-rm *.beam

FORCE:
