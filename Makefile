PROJECT=relang
REBAR=`which rebar || printf ./rebar`
VERSION=0.1

.PHONY: all docker
all: get-deps compile

test: eunit

eunit:
	$(REBAR) eu skip_deps=true

compile: rebar co

docker:
	docker build -t kureikain/relang:$(VERSION) .
push:
	docker push kureikain/relang:$(VERSION)
