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
extern int rsaGenerateKeys(int keySize, const char* pubPass, const char* privPass, const char* pubSavePath, const char* privSavePath);
extern char* rsaEncryptString(int keySize, const char* pubPass, const char* pubPath, const char* str);
extern int rsaEncryptFile(int keySize, const char* pubPass, const char* pubPath, const char* filePath, const char* outFilePath);
extern char* rsaDecryptString(int keySize, const char* privPass, const char* privPath, const char* str);
extern int rsaDecryptFile(int keySize, const char* privPass, const char* privPath, const char* filePath, const char* outFilePath);
extern char* rsaGetPubkeyModules(int keySize, const char* pubPass, const char* pubPath);
extern char* rsaGetPrivkeyModules(int keySize, const char* privPass, const char* privPath);

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
extern char* rdlEncryptString(int keySize, int cipherMode, const char* key, const char* str);
extern int rdlEncryptFile(int keySize, int cipherMode, const char* key, const char* filePath, const char* outFilePath);
extern char* rdlDecryptString(int keySize, int cipherMode, const char* key, const char* str);
extern int rdlDecryptFile(int keySize, int cipherMode, const char* key, const char* filePath, const char* outFilePath);

// RSASSA ==============================================================================
// keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
// hashMethod : (md5 = 0, sha1 = 1)
// return int: succ = 0, other = error code
extern int rsassaGenerateKeys(int keySize, const char* pubPass, const char* privPass, const char* pubSavePath, const char* privSavePath);
extern char* rsassaSignString(int keySize, int hashMethod, const char* privPass, const char* privPath, const char* str);
extern char* rsassaSignFile(int keySize, int hashMethod, const char* privPass, const char* privPath, const char* filePath);
extern int rsassaVerifyString(int keySize, int hashMethod, const char* pubPass, const char* pubPath, const char* sig, const char* str);
extern int rsassaVerifyFile(int keySize, int hashMethod, const char* pubPass, const char* pubPath, const char* sig, const char* filePath);

// AES =================================================================================
extern char* aesEncryptECB128(const char* key, const char* src);
extern char* aesEncryptECB192(const char* key, const char* src);
extern char* aesEncryptECB256(const char* key, const char* src);
// AES Encrypt ECB Exp
extern char* aesEncryptECB128Exp(const char* key, const char* src);
extern char* aesEncryptECB192Exp(const char* key, const char* src);
extern char* aesEncryptECB256Exp(const char* key, const char* src);
// AES Encrypt CBC
extern char* aesEncryptCBC128(const char* init, const char* key, const char* src);
extern char* aesEncryptCBC192(const char* init, const char* key, const char* src);
extern char* aesEncryptCBC256(const char* init, const char* key, const char* src);
// AES Encrypt CBC Exp
extern char* aesEncryptCBC128Exp(const char* init, const char* key, const char* src);
extern char* aesEncryptCBC192Exp(const char* init, const char* key, const char* src);
extern char* aesEncryptCBC256Exp(const char* init, const char* key, const char* src);
// AES Decrypt ECB
extern char* aesDecryptECB128(const char* key, const char* src);
extern char* aesDecryptECB192(const char* key, const char* src);
extern char* aesDecryptECB256(const char* key, const char* src);
// AES Decrypt ECB Exp
extern char* aesDecryptECB128Exp(const char* key, const char* src);
extern char* aesDecryptECB192Exp(const char* key, const char* src);
extern char* aesDecryptECB256Exp(const char* key, const char* src);
// AES Decrypt CBC
extern char* aesDecryptCBC128(const char* init, const char* key, const char* src);
extern char* aesDecryptCBC192(const char* init, const char* key, const char* src);
extern char* aesDecryptCBC256(const char* init, const char* key, const char* src);
// AES Decrypt CBC Exp
extern char* aesDecryptCBC128Exp(const char* init, const char* key, const char* src);
extern char* aesDecryptCBC192Exp(const char* init, const char* key, const char* src);
extern char* aesDecryptCBC256Exp(const char* init, const char* key, const char* src);

#endif /* NativeAlg_h */
