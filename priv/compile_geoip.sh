#!/bin/bash

set -e
export LC_CTYPE=C

cd $REBAR_DEPS_DIR/geoip

if [ ! -f configure ]; then
    ./bootstrap
fi

if [ ! -f config.status ]; then
    ./configure --prefix=/usr
fi

make
