PROJECT=relang
REBAR=`which rebar || printf ./rebar`

.PHONY: all
all: get-deps compile

test: eunit

eunit:
	$(REBAR) eu skip_deps=true

compile: rebar co
