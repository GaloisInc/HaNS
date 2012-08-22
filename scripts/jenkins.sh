#!/bin/sh

set -e

cabal-dev install -fbuild-tests
cabal-dev/bin/hans-tests --junit=results.xml
