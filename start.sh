#!/bin/sh
mkdir -p log/sasl
erl -name searcher@localhost -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s searcher -s erlbuild
