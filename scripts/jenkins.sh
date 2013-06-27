#!/bin/sh

set -ev

cabal-dev install -fenable-tests

./cabal-dev/bin/test-suite --jxml=results.xml
