//
//  NativeAlg.swift
//  alg
//
//  Created by rarnu on 5/27/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class NativeAlg: NSObject {
    
    /*
     rsassaGenerateKeys,
     rsassaSignString,
     rsassaSignFile,
     rsassaVerifyString,
     rsassaVerifyFile,
     */

    /// (__stdcall) char* md5EncryptString(char* str);
    typealias md5EncryptString = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* md5EncryptFile(char* filePath);
    typealias md5EncryptFile = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* sha1EncryptString(char* str);
    typealias sha1EncryptString = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* sha1EncryptFile(char* filePath);
    typealias sha1EncryptFile = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* lmdEncryptString(char* str);
    typealias lmdEncryptString = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* lmdEncryptFile(char* filePath);
    typealias lmdEncryptFile = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* elfEncryptString(char* str);
    typealias elfEncryptString = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* desEncryptString(char* str, char* key);
    typealias desEncryptString = @convention(c) (UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* desDecryptString(char* str, char* key);
    typealias desDecryptString = @convention(c) (UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* base64EncryptString(char* str);
    typealias base64EncryptString = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* base64DecryptString(char* str);
    typealias base64DecryptString = @convention(c) (UnsafePointer<Int8>) -> UnsafePointer<Int8>
    
    /// (__stdcall) int rsaGenerateKeys(int keySize, char* pubPass, char* privPass, char* pubSavePath, char* privSavePath);
    typealias rsaGenerateKeys = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) char* rsaEncryptString(int keySize, char* pubPass, char* pubPath, char* str);
    typealias rsaEncryptString = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) int rsaEncryptFile(int keySize, char* pubPass, char* pubPath, char* filePath, char* outFilePath);
    typealias rsaEncryptFile = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) char* rsaDecryptString(int keySize, char* privPass, char* privPath, char* str);
    typealias rsaDecryptString = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) int rsaDecryptFile(int keySize, char* privPass, char* privPath, char* filePath, char* outFilePath);
    typealias rsaDecryptFile = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) char* rsaGetPubkeyModules(int keySize, char* pubPass, char* pubPath);
    typealias rsaGetPubkeyModules = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (_-stdcall) char* rsaGetPrivkeyModules(int keySize, char* privPass, char* privPath);
    typealias rsaGetPrivkeyModules = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    
    /// (__stdcall) int dsaGenerateKeys(int keySize, char* pubPass, char* privPass, char* pubSavePath, char* privSavePath);
    typealias dsaGenerateKeys = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) char* dsaSignString(int keySize, char* privPass, char* privPath, char* str);
    typealias dsaSignString = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* dsaSignFile(int keySize, char* privPass, char* privPath, char* filePath);
    typealias dsaSignFile = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) int dsaVerifyString(int keySize, char* pubPass, char* pubPath, char* rs, char* str);
    typealias dsaVerifyString = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) int dsaVerifyFile(int keySize, char* pubPass, char* pubPath, char* rs, char* filePath);
    typealias dsaVerifyFile = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) char* dsaGetPubkeyQPGY(int keySize, char* pubPass, char* pubPath);
    typealias dsaGetPubkeyQPGY = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* dsaGetPrivkeyQPGX(int keySize, char* privPass, char* privPath);
    typealias dsaGetPrivkeyQPGX = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    
    /// (__stdcall) char* rdlEncryptString(int keySize, int cipherMode, char* key, char* str);
    typealias rdlEncryptString = @convention(c) (Int32, Int32, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) int rdlEncryptFile(int keySize, int cipherMode, char* key, char* filePath, char* outFilePath);
    typealias rdlEncryptFile = @convention(c) (Int32, Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) char* rdlDecryptString(int keySize, int cipherMode, char* key, char* str);
    typealias rdlDecryptString = @convention(c) (Int32, Int32, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) int rdlDecryptFile(int keySize, int cipherMode, char* key, char* filePath, char* outFilePath);
    typealias rdlDecryptFile = @convention(c) (Int32, Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    
    /// (__stdcall) int rsassaGenerateKeys(int keySize, char* pubPass, char* privPass, char* pubSavePath, char* privSavePath);
    typealias rsassaGenerateKeys = @convention(c) (Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) char* rsassaSignString(int keySize, int hashMethod, char* privPass, char* privPath, char* str);
    typealias rsassaSignString = @convention(c) (Int32, Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) char* rsassaSignFile(int keySize, int hashMethod, char* privPass, char* privPath, char* filePath);
    typealias rsassaSignFile = @convention(c) (Int32, Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> UnsafePointer<Int8>
    /// (__stdcall) int rsassaVerifyString(int keySize, int hashMethod, char* pubPass, char* pubPath, char* sig, char* str);
    typealias rsassaVerifyString = @convention(c) (Int32, Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32
    /// (__stdcall) int rsassaVerifyFile(int keySize, int hashMethod, char* pubPass, char* pubPath, char* sig, char* filePath);
    typealias rsassaVerifyFile = @convention(c) (Int32, Int32, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>, UnsafePointer<Int8>) -> Int32

    static var hLib: UnsafeMutablePointer<Void>? = nil
    static var mMd5EncryptString: md5EncryptString? = nil
    static var mMd5EncryptFile: md5EncryptFile? = nil
    static var mSha1EncryptString: sha1EncryptString? = nil
    static var mSha1EncryptFile: sha1EncryptFile? = nil
    static var mLmdEncryptString: lmdEncryptString? = nil
    static var mLmdEncryptFile: lmdEncryptFile? = nil
    static var mElfEncryptString: elfEncryptString? = nil
    static var mDesEncryptString: desEncryptString? = nil
    static var mDesDecryptString: desDecryptString? = nil
    static var mBase64EncryptString: base64EncryptString? = nil
    static var mBase64DecryptString: base64DecryptString? = nil
    
    static var mRsaGenerateKeys: rsaGenerateKeys? = nil
    static var mRsaEncryptString: rsaEncryptString? = nil
    static var mRsaEncryptFile: rsaEncryptFile? = nil
    static var mRsaDecryptString: rsaDecryptString? = nil
    static var mRsaDecryptFile: rsaDecryptFile? = nil
    static var mRsaGetPubkeyModules: rsaGetPubkeyModules? = nil
    static var mRsaGetPrivkeyModules: rsaGetPrivkeyModules? = nil
    
    static var mDsaGenerateKeys: dsaGenerateKeys? = nil
    static var mDsaSignString: dsaSignString? = nil
    static var mDsaSignFile: dsaSignFile? = nil
    static var mDsaVerifyString: dsaVerifyString? = nil
    static var mDsaVerifyFile: dsaVerifyFile? = nil
    static var mDsaGetPubkeyQPGY: dsaGetPubkeyQPGY? = nil
    static var mDsaGetPrivkeyQPGX: dsaGetPrivkeyQPGX? = nil
    
    static var mRdlEncryptString: rdlEncryptString? = nil
    static var mRdlEncryptFile: rdlEncryptFile? = nil
    static var mRdlDecryptString: rdlDecryptString? = nil
    static var mRdlDecryptFile: rdlDecryptFile? = nil
    
    static var mRsassaGenerateKeys: rsassaGenerateKeys? = nil
    static var mRsassaSignString: rsassaSignString? = nil
    static var mRsassaSignFile: rsassaSignFile? = nil
    static var mRsassaVerifyString: rsassaVerifyString? = nil
    static var mRsassaVerifyFile: rsassaVerifyFile? = nil
    
    class func initLib() {
        let libPath = NSBundle.mainBundle().pathForResource("libalg", ofType: "dylib")
        hLib = dlopen(libPath!, RTLD_LAZY)
        if (hLib != nil) {
            mMd5EncryptString = unsafeBitCast(dlsym(hLib!, "md5EncryptString"), md5EncryptString.self)
            mMd5EncryptFile = unsafeBitCast(dlsym(hLib!, "md5EncryptFile"), md5EncryptFile.self)
            mSha1EncryptString = unsafeBitCast(dlsym(hLib!, "sha1EncryptString"), sha1EncryptString.self)
            mSha1EncryptFile = unsafeBitCast(dlsym(hLib!, "sha1EncryptFile"), sha1EncryptFile.self)
            mLmdEncryptString = unsafeBitCast(dlsym(hLib!, "lmdEncryptString"), lmdEncryptString.self)
            mLmdEncryptFile = unsafeBitCast(dlsym(hLib!, "lmdEncryptFile"), lmdEncryptFile.self)
            mElfEncryptString = unsafeBitCast(dlsym(hLib!, "elfEncryptString"), elfEncryptString.self)
            mDesEncryptString = unsafeBitCast(dlsym(hLib!, "desEncryptString"), desEncryptString.self)
            mDesDecryptString = unsafeBitCast(dlsym(hLib!, "desDecryptString"), desDecryptString.self)
            mBase64EncryptString = unsafeBitCast(dlsym(hLib!, "base64EncryptString"), base64EncryptString.self)
            mBase64DecryptString = unsafeBitCast(dlsym(hLib!, "base64DecryptString"), base64DecryptString.self)
            
            mRsaGenerateKeys = unsafeBitCast(dlsym(hLib!, "rsaGenerateKeys"), rsaGenerateKeys.self)
            mRsaEncryptString = unsafeBitCast(dlsym(hLib!, "rsaEncryptString"), rsaEncryptString.self)
            mRsaEncryptFile = unsafeBitCast(dlsym(hLib!, "rsaEncryptFile"), rsaEncryptFile.self)
            mRsaDecryptString = unsafeBitCast(dlsym(hLib!, "rsaDecryptString"), rsaDecryptString.self)
            mRsaDecryptFile = unsafeBitCast(dlsym(hLib!, "rsaDecryptFile"), rsaDecryptFile.self)
            mRsaGetPubkeyModules = unsafeBitCast(dlsym(hLib!, "rsaGetPubkeyModules"), rsaGetPubkeyModules.self)
            mRsaGetPrivkeyModules = unsafeBitCast(dlsym(hLib!, "rsaGetPrivkeyModules"), rsaGetPrivkeyModules.self)
            
            mDsaGenerateKeys = unsafeBitCast(dlsym(hLib!, "dsaGenerateKeys"), dsaGenerateKeys.self)
            mDsaSignString = unsafeBitCast(dlsym(hLib!, "dsaSignString"), dsaSignString.self)
            mDsaSignFile = unsafeBitCast(dlsym(hLib!, "dsaSignFile"), dsaSignFile.self)
            mDsaVerifyString = unsafeBitCast(dlsym(hLib!, "dsaVerifyString"), dsaVerifyString.self)
            mDsaVerifyFile = unsafeBitCast(dlsym(hLib!, "dsaVerifyFile"), dsaVerifyFile.self)
            mDsaGetPubkeyQPGY = unsafeBitCast(dlsym(hLib!, "dsaGetPubkeyQPGY"), dsaGetPubkeyQPGY.self)
            mDsaGetPrivkeyQPGX = unsafeBitCast(dlsym(hLib!, "dsaGetPrivkeyQPGX"), dsaGetPrivkeyQPGX.self)
            
            mRdlEncryptString = unsafeBitCast(dlsym(hLib!, "rdlEncryptString"), rdlEncryptString.self)
            mRdlEncryptFile = unsafeBitCast(dlsym(hLib!, "rdlEncryptFile"), rdlEncryptFile.self)
            mRdlDecryptString = unsafeBitCast(dlsym(hLib!, "rdlDecryptString"), rdlDecryptString.self)
            mRdlDecryptFile = unsafeBitCast(dlsym(hLib!, "rdlDecryptFile"), rdlDecryptFile.self)
            
            mRsassaGenerateKeys = unsafeBitCast(dlsym(hLib!, "rsassaGenerateKeys"), rsassaGenerateKeys.self)
            mRsassaSignString = unsafeBitCast(dlsym(hLib!, "rsassaSignString"), rsassaSignString.self)
            mRsassaSignFile = unsafeBitCast(dlsym(hLib!, "rsassaSignFile"), rsassaSignFile.self)
            mRsassaVerifyString = unsafeBitCast(dlsym(hLib!, "rsassaVerifyString"), rsassaVerifyString.self)
            mRsassaVerifyFile = unsafeBitCast(dlsym(hLib!, "rsassaVerifyFile"), rsassaVerifyFile.self)
        }
    }
    
    class func finaLib() {
        mMd5EncryptString = nil
        if (hLib != nil) {
            dlclose(hLib!)
        }
    }
}
