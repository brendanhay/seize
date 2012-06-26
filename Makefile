#
# Targets
#

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
	exec erl -pa ../seize/ebin -sname seize


#
# C
#

valgrind: build
	valgrind --tool=memcheck --leak-check=full priv/seize /bin/ls /bin

splint: build
	splint +posixstrictlib -I /usr/local/Cellar/erlang/R15B01/lib/erlang/lib/erl_interface-3.7.7/include c_src/*.c


