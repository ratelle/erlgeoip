#!/bin/bash

set -e

cd $REBAR_DEPS_DIR/geoip

if [ ! -f configure ]; then
    ./bootstrap
fi

if [ ! -f config.status ]; then
    ./configure --prefix=/usr
fi

make
