#!/bin/sh
mkdir -p log/sasl
erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s searcher 
