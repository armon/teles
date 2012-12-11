APP = teles

# The environment we're building for. This mainly affects what
# overlay variables are used for rel creation.
ENVIRONMENT ?= development

all: deps compile

compile:
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps

devrel: rel
	rm -rf rel/$(APP)/lib/$(APP)-*/ebin
	ln -sf $(abspath ./apps/$(APP)/ebin) rel/$(APP)/lib/$(APP)-*
	rm -rf rel/$(APP)/lib/$(APP)-*/priv
	ln -sf $(abspath ./apps/$(APP)/priv) rel/$(APP)/lib/$(APP)-*

rel: compile
	cd rel; ../rebar generate -f overlay_vars=vars/$(ENVIRONMENT).config

test: compile
	./rebar eunit apps=$(APP)

.PHONY: all compile clean deps
