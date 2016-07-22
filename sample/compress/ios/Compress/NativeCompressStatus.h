//
//  NativeCompressStatus.h
//  Compress
//
//  Created by rarnu on 6/21/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NativeCompress.h"

@interface CompressStatus : NSObject

@property NSString * filePath;
@property NSInteger fileCount;
@property NSInteger compressCount;
@property NSInteger errorCode;
@property NSString * errorMessage;

-(id)init:(NSString *) filePath fileCount:(NSInteger)fileCount compressCount:(NSInteger)compressCount errorCode:(NSInteger)errorCode errorMessage:(NSString *)errorMessage;

@end

@interface UnCompressStatus : NSObject

@property NSString * filePath;
@property NSInteger fileCount;
@property NSInteger uncompressCount;
@property NSInteger errorCode;
@property NSString * errorMessage;

-(id)init:(NSString *) filePath fileCount:(NSInteger)fileCount uncompressCount:(NSInteger)uncompressCount errorCode:(NSInteger)errorCode errorMessage:(NSString *)errorMessage;

@end

@interface NativeCompress : NSObject

+(CompressStatus *) getCompressStatus: (NSString *)filePath;
+(UnCompressStatus *) getUncompressStatus:(NSString *)filePath;


@end
