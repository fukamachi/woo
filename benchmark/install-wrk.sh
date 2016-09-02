#!/bin/sh

WRK_VERSION=4.0.2

curl -L "https://github.com/wg/wrk/archive/$WRK_VERSION.tar.gz" | tar xzf -;
cd "wrk-$WRK_VERSION/"
make
cp wrk ~/.roswell/bin
