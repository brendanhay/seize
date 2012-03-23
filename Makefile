REBAR=`which rebar`

.PHONY: build

all: build

clean:
	rm -rf ebin log doc erl_crash.dump
	$(REBAR) clean

build: deps
	$(REBAR) compile
	$(REBAR) skip_deps=true xref

deps:
	$(REBAR) get-deps

test: build
	rm -rf .eunit
	$(REBAR) skip_deps=true eunit

boot:
	exec erl -pa ebin -sname muxer
