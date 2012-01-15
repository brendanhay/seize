REBAR=`which rebar`

.PHONY: deps build

all: deps build

clean:
	rm -rf ./**/ebin log doc erl_crash.dump ./**/erl_crash.dump
	$(REBAR) clean

deps:
	$(REBAR) get-deps

build:
	$(REBAR) compile
