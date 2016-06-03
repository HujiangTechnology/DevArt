//
//  NativeCompress.h
//  Compress
//
//  Created by rarnu on 6/3/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

#ifndef NativeCompress_h
#define NativeCompress_h

extern int uncompress(const char* filePath, const char* dest);
extern int compress(const char* filePath, const char* src);
extern int getLastError();
extern char* getLastErrorMessage();
extern char* getHelp();

#endif /* NativeCompress_h */
