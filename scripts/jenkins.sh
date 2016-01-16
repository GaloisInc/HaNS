#!/bin/sh

set -ev

if [ ! -d .cabal-sandbox ]; then
	cabal sandbox init
	cabal install --only-dep
fi

cabal configure --enable-tests
cabal build

./dist/build/hans-tests/hans-tests --xml=results.xml
