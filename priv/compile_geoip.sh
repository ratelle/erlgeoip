#!/bin/bash

set -e
export LC_CTYPE=C

cd $REBAR_DEPS_DIR/geoip

if [ ! -f configure ]; then
    ./bootstrap
fi

if [ ! -f config.status ]; then
%%    The prefix doesn't work in mavericks. Removing for now
%%    ./configure --prefix=/usr
    ./configure
fi

make
