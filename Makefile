PROJECT=relang
REBAR=`which rebar || printf ./rebar`
VERSION=0.1

.PHONY: all docker
all: get-deps compile repl

get-deps:
	$(REBAR) get-deps

compile: $(REBAR) co

test: eunit

eunit:
	$(REBAR) eu skip_deps=true

docker:
	docker build -t kureikain/relang:$(VERSION) .
push:
	docker push kureikain/relang:$(VERSION)

wercker:
	wercker build --docker-host tcp://192.168.59.103:2376

repl:
	erl -pa ebin -pa deps/protobuffs/ebin deps/jsx/ebin
