#!/bin/sh

FPC=/usr/local/lib/fpc/3.1.1/ppcrossa64

rm libalg.a
rm libalg.o
rm libalg.rsj
rm link.res
rm linksyms.fpc
rm ppas.sh

${FPC} -Tdarwin -Cn libalg.lpr
ar -q libalg.a `grep "\.o$" link.res`
ranlib libalg.a 

