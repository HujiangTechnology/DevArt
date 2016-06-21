//
//  NativeCompressStatus.m
//  Compress
//
//  Created by rarnu on 6/21/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

#import "NativeCompressStatus.h"

@implementation CompressStatus

-(id)init:(NSString *) filePath fileCount:(NSInteger)fileCount compressCount:(NSInteger)compressCount errorCode:(NSInteger)errorCode errorMessage:(NSString *)errorMessage {
    if (self) {
        self.filePath = filePath;
        self.fileCount = fileCount;
        self.compressCount = compressCount;
        self.errorCode = errorCode;
        self.errorMessage = errorMessage;
    }
    return self;
}

@end


@implementation UnCompressStatus

-(id)init:(NSString *) filePath fileCount:(NSInteger)fileCount uncompressCount:(NSInteger)uncompressCount errorCode:(NSInteger)errorCode errorMessage:(NSString *)errorMessage {
    if (self) {
        self.filePath = filePath;
        self.fileCount = fileCount;
        self.uncompressCount = uncompressCount;
        self.errorCode = errorCode;
        self.errorMessage = errorMessage;
    }
    return self;
}

@end

@implementation NativeCompress

+(CompressStatus *) getCompressStatus: (NSString *)filePath {
    const char* fileName = [filePath UTF8String];
    NSString * errMsg = [NSString stringWithUTF8String:getCompressErrorMessage(fileName)];
    return [[CompressStatus alloc] init:filePath fileCount:getCompressFileCount(fileName) compressCount:getCompressedCount(fileName) errorCode:getCompressErrorCode(fileName) errorMessage:errMsg];
}

+(UnCompressStatus *) getUncompressStatus:(NSString *)filePath {
    const char* fileName = [filePath UTF8String];
    NSString * errMsg = [NSString stringWithUTF8String:getUncompressErrorMessage(fileName)];
    return [[UnCompressStatus alloc] init:filePath fileCount:getUncompressFileCount(fileName) uncompressCount:getUncompressedCount(fileName) errorCode:getUncompressErrorCode(fileName) errorMessage:errMsg];
}

@end
