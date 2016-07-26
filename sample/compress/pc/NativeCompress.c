#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <unistd.h>

#define ERRORCODE_NOERROR 0
#define ERRORCODE_FORMAT_NOT_SUPPORT -1
#define ERRORCODE_UNCOMPRESS -2
#define ERRORCODE_COMPRESS -3

#define __stdcall __attribute__((__stdcall__))
typedef int (*uncompress)(const char* filePath, const char* dest);
typedef int (*compress)(const char* filePath, const char* src);
typedef char* (*getFileSize)(const char* path);
typedef int (*getCompressErrorCode)(const char* filePath);
typedef char* (*getCompressErrorMessage)(const char* filePath);
typedef int (*getCompressFileCount)(const char* filePath);
typedef int (*getCompressedCount)(const char* filePath);
typedef int (*getUncompressErrorCode)(const char* filePath);
typedef char* (*getUncompressErrorMessage)(const char* filePath);
typedef int (*getUncompressFileCount)(const char* filePath);
typedef int (*getUncompressedCount)(const char* filePath);

uncompress mUncompress = NULL;
compress mCompress = NULL;
getFileSize mGetFileSize = NULL;
getCompressErrorCode mGetCompressErrorCode = NULL;
getCompressErrorMessage mGetCompressErrorMessage = NULL;
getCompressFileCount mGetCompressFileCount = NULL;
getCompressedCount mGetCompressedCount = NULL;
getUncompressErrorCode mGetUncompressErrorCode = NULL;
getUncompressErrorMessage mGetUncompressErrorMessage = NULL;
getUncompressFileCount mGetUncompressFileCount = NULL;
getUncompressedCount mGetUncompressedCount = NULL;

void* hLib = NULL;
char* libPath =
    #ifdef _WINDOWS
    "\\hjz.dll"
    #else
    #ifdef _DARWIN
    "/libhjz.dylib"
    #else
    "/libhjz.so"
    #endif
    #endif
;

static void initialize() {
    char buf[255];
    char * basePath = getcwd(buf, sizeof(buf));
    strcat(basePath, libPath);
    hLib = dlopen(basePath, RTLD_LAZY);
    if (hLib != NULL) {
        mUncompress = dlsym(hLib, "uncompress");
        mCompress = dlsym(hLib, "compress");
        mGetFileSize = dlsym(hLib, "getFileSize");
        mGetCompressErrorCode = dlsym(hLib, "getCompressErrorCode");
        mGetCompressErrorMessage = dlsym(hLib, "getCompressErrorMessage");
        mGetCompressFileCount = dlsym(hLib, "getCompressFileCount");
        mGetCompressedCount = dlsym(hLib, "getCompressedCount");
        mGetUncompressErrorCode = dlsym(hLib, "getUncompressErrorCode");
        mGetUncompressErrorMessage = dlsym(hLib, "getUncompressErrorMessage");
        mGetUncompressFileCount = dlsym(hLib, "getUncompressFileCount");
        mGetUncompressedCount = dlsym(hLib, "getUncompressedCount");
    }
}

static void finalize() {
    mUncompress = NULL;
    mCompress = NULL;
    mGetFileSize = NULL;
    mGetCompressErrorCode = NULL;
    mGetCompressErrorMessage = NULL;
    mGetCompressFileCount = NULL;
    mGetCompressedCount = NULL;
    mGetUncompressErrorCode = NULL;
    mGetUncompressErrorMessage = NULL;
    mGetUncompressFileCount = NULL;
    mGetUncompressedCount = NULL;
    if (hLib != NULL) {
        dlclose(hLib);
    }
}

