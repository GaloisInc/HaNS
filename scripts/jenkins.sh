#!/bin/sh

set -e

cabal-dev install -fbuild-tests
cabal-dev/bin/hans-tests --jxml=results.xml
