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
	APP=$4
	if [ ! -d "../out/android" ]; then
		mkdir "../out/android"
	fi
	if [ ! -d "../out/android/${DIR}" ]; then
		mkdir "../out/android/${DIR}"
	fi
	mv $OUT ../out/android/${DIR}/${APP}
	rm -fr ../out/${CPU}-android
}

_assets() {
	DIR=$1
	OUT=$2
	CPU=$3
	APP=$4
	if [ ! -d "../out/assets" ]; then
		mkdir "../out/assets"
	fi
	if [ ! -d "../out/assets/${DIR}" ]; then
		mkdir "../out/assets/${DIR}"
	fi
	mv $OUT ../out/assets/${DIR}/${APP}
	rm -fr ../out/${CPU}-android
}

_compile() {
	CPU=$1
	LIB=$2
	INSTALL=$3
	PROJ=$4
	APP=$5
	_env ${CPU}
	${FPC} -B -Tandroid -P${CPU} -MObjFPC -Scghi -O1 -l -vewnhibq \
		-Fl${TYPHON_BIN_LIB}/android-4.4-api19-${LIB} \
		-Fu../jni \
		-Fu. \
		-Fi../out/${CPU}-android \
		-FU../out/${CPU}-android \
		-o${APP}_${CPU} \
	${PROJ}.lpr
	if [ "${APP##*.}" = "so" ]; then
		_install $INSTALL "${APP}_${CPU}" $CPU $APP
	else
		_assets $INSTALL "${APP}_${CPU}" $CPU $APP
	fi
}

_compile $P_ARM $LIB_ARM "armeabi" "daemon20" "libdaemon20.so"
_compile $P_MIPS $LIB_MIPS "mips" "daemon20" "libdaemon20.so"
_compile $P_X86 $LIB_X86 "x86" "daemon20" "libdaemon20.so"
_compile $P_ARM $LIB_ARM "armeabi" "daemon21" "libdaemon21.so"
_compile $P_MIPS $LIB_MIPS "mips" "daemon21" "libdaemon21.so"
_compile $P_X86 $LIB_X86 "x86" "daemon21" "libdaemon21.so"
_compile $P_ARM $LIB_ARM "armeabi" "daemon" "daemon"
_compile $P_MIPS $LIB_MIPS "mips" "daemon" "daemon"
_compile $P_X86 $LIB_X86 "x86" "daemon" "daemon"


