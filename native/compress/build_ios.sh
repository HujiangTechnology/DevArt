#!/bin/sh

FPC_ROOT=/usr/local/lib/fpc/3.1.1
FPC_A64=${FPC_ROOT}/ppcrossa64
FPC_ARM=${FPC_ROOT}/ppcrossarm
FPC_EMU=${FPC_ROOT}/ppcx64

T_A64="darwin"
T_ARM="darwin"
T_EMU="iphonesim"

_clean() {
    rm *.o
    rm z/*.o
    rm 3rd/bzip2/*.o
    rm 3rd/exlz/*.o
    rm *.ppu
    rm z/*.ppu
    rm 3rd/bzip2/*.ppu
    rm 3rd/exlz/*.ppu
    rm *.rsj
    rm z/*.rsj
    rm 3rd/bzip2/*.rsj
    rm 3rd/exlz/*.rsj
    rm link.res
    rm linksyms.fpc
    rm ppas.sh

}


_compile() {
    ARCH=$1
    CMD="FPC_${ARCH}"
    TARGET="T_${ARCH}"
    ${!CMD} -T${!TARGET} -Cn -Fuz -Fu3rd/bzip2 -Fu3rd/exlz hjz_ios.lpr
    ar -q libcompress_${ARCH}.a `grep "\.o$" link.res`
    ranlib libcompress_${ARCH}.a
}

rm libcompress.a

#compile
_clean
_compile "A64"
_clean
_compile "ARM"
_clean
_compile "EMU"

#combine
lipo -create libcompress_A64.a libcompress_ARM.a libcompress_EMU.a -output libcompress.a

# clean
_clean
rm libcompress_A64.a
rm libcompress_ARM.a
rm libcompress_EMU.a