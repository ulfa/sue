#!/bin/sh
mkdir -p log/sasl
erl -sname sue -setcookie nocookie -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s sue 