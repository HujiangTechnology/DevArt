//
//  NativeAlg.h
//  alg
//
//  Created by rarnu on 6/1/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

#ifndef NativeAlg_h
#define NativeAlg_h

// Hash ==============================================================================
extern char* md5EncryptString(const char* str);
extern char* md5EncryptFile(const char* filePath);
extern char* sha1EncryptString(const char* str);
extern char* sha1EncryptFile(const char* filePath);
extern char* lmdEncryptString(const char* str);
extern char* lmdEncryptFile(const char* filePath);
extern char* elfEncryptString(const char* str);

// Base64 ==============================================================================
extern char* base64EncryptString(const char* str);
extern char* base64DecryptString(const char* str);

// DES ==============================================================================
extern char* des3EncryptString(const char* str, const char* key);
extern char* des3DecryptString(const char* str, const char* key);

// RSA ==============================================================================
// keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
// return int: succ = 0, other = error code
extern int rsaGenerateKeysEx(int keySize, const char* pubPass, const char* privPass, const char* pubSavePath, const char* privSavePath);
extern char* rsaEncryptStringEx(int keySize, const char* pubPass, const char* pubPath, const char* str);
extern int rsaEncryptFileEx(int keySize, const char* pubPass, const char* pubPath, const char* filePath, const char* outFilePath);
extern char* rsaDecryptStringEx(int keySize, const char* privPass, const char* privPath, const char* str);
extern int rsaDecryptFileEx(int keySize, const char* privPass, const char* privPath, const char* filePath, const char* outFilePath);
extern char* rsaGetPubkeyModulesEx(int keySize, const char* pubPass, const char* pubPath);
extern char* rsaGetPrivkeyModulesEx(int keySize, const char* privPass, const char* privPath);

// DSA ==============================================================================
// keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
// return int: succ = 0, other = error code
extern int dsaGenerateKeys(int keySize, const char* pubPass, const char* privPass, const char* pubSavePath, const char* privSavePath);
extern char* dsaSignString(int keySize, const char* privPass, const char* privPath, const char* str);
extern char* dsaSignFile(int keySize, const char* privPass, const char* privPath, const char* filePath);
extern int dsaVerifyString(int keySize, const char* pubPass, const char* pubPath, const char* rs, const char* str);
extern int dsaVerifyFile(int keySize, const char* pubPass, const char* pubPath, const char* rs, const char* filePath);
extern char* dsaGetPubkeyQPGY(int keySize, const char* pubPass, const char* pubPath);
extern char* dsaGetPrivkeyQPGX(int keySize, const char* privPass, const char* privPath);

// RDL ==============================================================================
// keySize: (ks128 = 0, ks192 = 1, ks256 = 2);
// cipherMode: (cmECB = 0, cmCBC = 1);
// return int: succ = 0, other = error code
extern char* rdlEncryptStringEx2(int keySize, int cipherMode, const char* key, const char* str);
extern int rdlEncryptFileEx2(int keySize, int cipherMode, const char* key, const char* filePath, const char* outFilePath);
extern char* rdlDecryptStringEx2(int keySize, int cipherMode, const char* key, const char* str);
extern int rdlDecryptFileEx2(int keySize, int cipherMode, const char* key, const char* filePath, const char* outFilePath);

// RSASSA ==============================================================================
// keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
// hashMethod : (md5 = 0, sha1 = 1)
// return int: succ = 0, other = error code
extern int rsassaGenerateKeys(int keySize, const char* pubPass, const char* privPass, const char* pubSavePath, const char* privSavePath);
extern char* rsassaSignString(int keySize, int hashMethod, const char* privPass, const char* privPath, const char* str);
extern char* rsassaSignFile(int keySize, int hashMethod, const char* privPass, const char* privPath, const char* filePath);
extern int rsassaVerifyString(int keySize, int hashMethod, const char* pubPass, const char* pubPath, const char* sig, const char* str);
extern int rsassaVerifyFile(int keySize, int hashMethod, const char* pubPass, const char* pubPath, const char* sig, const char* filePath);

#endif /* NativeAlg_h */
