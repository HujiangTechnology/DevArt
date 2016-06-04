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
    rm lockbox/*.o
	rm *.ppu
    rm lockbox/*.ppu
	rm *.rsj
    rm lockbox/*.rsj
	rm link.res
	rm linksyms.fpc
	rm ppas.sh
}

_compile() {
	ARCH=$1
	CMD="FPC_${ARCH}"
    TARGET="T_${ARCH}"
    ${!CMD} -T${!TARGET} -Cn -Fulockbox alg_ios.lpr
	ar -q libalg_${ARCH}.a `grep "\.o$" link.res`
	ranlib libalg_${ARCH}.a 
}

rm libalg.a

#compile
_clean
_compile "A64"
_clean
_compile "ARM"
_clean
_compile "EMU"

# combine
lipo -create libalg_A64.a libalg_ARM.a libalg_EMU.a -output libalg.a

# clean
_clean
rm libalg_A64.a
rm libalg_ARM.a
rm libalg_EMU.a
