#!/bin/sh

ROOT_PATH=/usr/local/codetyphon
TYPHON_PATH=${ROOT_PATH}/typhon
LIB_PATH=x86_64-iphonesim
LAZARUS_ROOT=/Developer/lazarus
PLATFORM_ROOT=/Applications/Xcode.app/Contents/Developer/Platforms
PLATFORM=iPhoneSimulator
SDK_PATH=${PLATFORM_ROOT}/${PLATFORM}.platform/Developer/SDKs/${PLATFORM}.sdk
BIN_PATH=${SDK_PATH}/usr/bin/
SDK_LIBS=${SDK_PATH}/usr/lib/
FPC=/usr/local/lib/fpc/3.0.1/ppcx64
OUTPUT=libalg_emu.dylib

${FPC} -B -Tiphonesim -Px86_64 -Scghi -O1 -va \
	-XR${SDK_PATH} \
	-Filib${SDK_LIBS} \
	-Filib${SDK_LIBS}/system/ \
	-Fu${SDK_LIBS} \
	-Fu${SDK_LIBS}/system/ \
	-Fi${SDK_LIBS}/ \
	-Fi${SDK_LIBS}/system/ \
	-Fi../jni \
    -Filockbox \
    -Fisec \
	-Fl. \
	-Fu../jni \
	-Fulockbox \
    -Fusec \
	-Fu${TYPHON_PATH}/components/BaseUtils \
        -Fu${LAZARUS_ROOT}/packager/units/${LIB_PATH} \
	-Fu. \
	-FUlib/${LIB_PATH} \
	-FD${BIN_PATH} \
	-o${OUTPUT} \
alg.lpr
