#!/bin/sh

FPC=/usr/local/lib/fpc/3.1.1/ppcrossa64

${FPC} -Tdarwin -Cn -Fuz -Fu3rd/bzip2 -Fu3rd/exlz hjz_ios.lpr
ar -q libcompress.a `grep "\.o$" link.res`
ranlib libcompress.a 


