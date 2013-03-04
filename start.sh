#!/bin/sh
mkdir -p log/sasl
erl -name sue@localhost -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s sue -s erlbuild
