#!/bin/sh

ROOT_PATH=/usr/local/codetyphon
TYPHON_PATH=${ROOT_PATH}/typhon
TYPHON_BIN_LIB=${ROOT_PATH}/binLibraries
FPC=/usr/local/codetyphon/fpc/fpc64/bin/x86_64-linux/fpc

P_ARM=arm
P_MIPS=mipsel
P_X86=i386

LIB_ARM=arm
LIB_MIPS=mips
LIB_X86=i386

_env() {
	CPU=$1
	if [ ! -d "../out" ]; then
		mkdir "../out"
	fi
	if [ ! -d "../out/${CPU}-android" ]; then
		mkdir "../out/${CPU}-android"
	fi
}

_install() {
	DIR=$1
	OUT=$2
	CPU=$3
	if [ ! -d "../out/android" ]; then
		mkdir "../out/android"
	fi
	if [ ! -d "../out/android/${DIR}" ]; then
		mkdir "../out/android/${DIR}"
	fi
	mv $OUT ../out/android/${DIR}/libterm.so
	rm -fr ../out/${CPU}-android
}

_compile() {
	CPU=$1
	LIB=$2
	INSTALL=$3
	_env ${CPU}
	${FPC} -B -Tandroid -P${CPU} -MObjFPC -Scghi -O1 -l -vewnhibq \
		-Fl${TYPHON_BIN_LIB}/android-4.4-api19-${LIB} \
		-Fu../jni \
		-Fu. \
		-Fi../out/${CPU}-android \
		-FU../out/${CPU}-android \
		-olibterm_${CPU}.so \
	term.lpr
	_install $INSTALL "libterm_${CPU}.so" $CPU
}

_compile $P_ARM $LIB_ARM "armeabi"
_compile $P_MIPS $LIB_MIPS "mips"
_compile $P_X86 $LIB_X86 "x86"


