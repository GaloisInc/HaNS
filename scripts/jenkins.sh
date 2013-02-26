#!/bin/sh

set -e

cabal-dev install-deps --enable-tests
cabal-dev configure --enable-tests
cabal-dev build
cabal-dev test --test-option='--jxml=results.xml'
