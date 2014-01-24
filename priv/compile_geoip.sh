#!/bin/bash

set -e
export LC_CTYPE=C

cd $REBAR_DEPS_DIR/geoip

if [ ! -f configure ]; then
    ./bootstrap
fi


# Geoip will expect the databases in /usr/local/share/GeoIP/
# Theorically with   ./configure --prefix=/path/to/database
# the database path is changed to /path/to/database/share/GeoIP/

if [ ! -f config.status ]; then
    ./configure
fi

make
