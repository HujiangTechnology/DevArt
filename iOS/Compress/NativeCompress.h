//
//  NativeCompress.h
//  Compress
//
//  Created by rarnu on 6/3/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

#ifndef NativeCompress_h
#define NativeCompress_h

#define ERRORCODE_NOERROR 0
#define ERRORCODE_FORMAT_NOT_SUPPORT -1
#define ERRORCODE_UNCOMPRESS -2
#define ERRORCODE_COMPRESS -3

extern int uncompress(const char* filePath, const char* dest);
extern int compress(const char* filePath, const char* src);
extern char* getFileSize(const char* path);

extern int getCompressErrorCode(const char* filePath);
extern char* getCompressErrorMessage(const char* filePath);
extern int getCompressFileCount(const char* filePath);
extern int getCompressedCount(const char* filePath);
extern int getUncompressErrorCode(const char* filePath);
extern char* getUncompressErrorMessage(const char* filePath);
extern int getUncompressFileCount(const char* filePath);
extern int getUncompressedCount(const char* filePath);

#endif /* NativeCompress_h */
