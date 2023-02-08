#include "OemMisc2.h"
#include <Uefi/UefiBaseType.h>
#include <Protocol/EFIChipInfoTypes.h>
#include <Protocol/EFIDDRGetConfig.h>
#include <Protocol/EFIRng.h>
#include <Protocol/EFINandPartiGuid.h>
#include <Library/LinuxLoaderLib.h>
#include <Library/UefiLib.h>
#include <Library/BootLinux.h>
#include <Protocol/EFISmem.h>

#define BLK_BITS         (9)
#define BLK_SIZE         (1 << BLK_BITS)

Misc2Info misc2Info;

/**********************************************************************************************************************************************************/
#define SHA256_ROTL(a,b) (((a>>(32-b))&(0x7fffffff>>(31-b)))|(a<<b))
#define SHA256_SR(a,b) ((a>>b)&(0x7fffffff>>(b-1)))
#define SHA256_Ch(x,y,z) ((x&y)^((~x)&z))
#define SHA256_Maj(x,y,z) ((x&y)^(x&z)^(y&z))
#define SHA256_E0(x) (SHA256_ROTL(x,30)^SHA256_ROTL(x,19)^SHA256_ROTL(x,10))
#define SHA256_E1(x) (SHA256_ROTL(x,26)^SHA256_ROTL(x,21)^SHA256_ROTL(x,7))
#define SHA256_O0(x) (SHA256_ROTL(x,25)^SHA256_ROTL(x,14)^SHA256_SR(x,3))
#define SHA256_O1(x) (SHA256_ROTL(x,15)^SHA256_ROTL(x,13)^SHA256_SR(x,10))

char* StrSHA256(const char *str,  int length, char *sha256) {
    char *pp, *ppend;
    int l, i, W[64], T1, T2, A, B, C, D, E, F, G, H, H0, H1, H2, H3, H4, H5, H6, H7;
    H0 = 0x6a09e667, H1 = 0xbb67ae85, H2 = 0x3c6ef372, H3 = 0xa54ff53a;
    H4 = 0x510e527f, H5 = 0x9b05688c, H6 = 0x1f83d9ab, H7 = 0x5be0cd19;

    int  K[64] = {
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    };

    l = length + ((length % 64 >= 56) ? (128 - length % 64) : (64 - length % 64));
    if (!(pp = (char *)AllocatePool((unsigned int)l))) return 0;
    ZeroMem(pp, l);
    for (i = 0; i < length; pp[i + 3 - 2 * (i % 4)] = str[i], i++) ;
    for (pp[i + 3 - 2 * (i % 4)] = (char)128, i++; i < l; pp[i + 3 - 2 * (i % 4)] = 0, i++) ;
    *((int *)(pp + l - 4)) = length << 3;
    *((int *)(pp + l - 8)) = length >> 29;
    for (ppend = pp + l; pp < ppend; pp += 64) {
        for (i = 0; i < 16; W[i] = ((int *)pp)[i], i++) ;
        for (i = 16; i < 64; W[i] = (SHA256_O1(W[i - 2]) + W[i - 7] + SHA256_O0(W[i - 15]) + W[i - 16]), i++) ;
        A = H0, B = H1, C = H2, D = H3, E = H4, F = H5, G = H6, H = H7;
        for (i = 0; i < 64; i++) {
            T1 = H + SHA256_E1(E) + SHA256_Ch(E, F, G) + K[i] + W[i];
            T2 = SHA256_E0(A) + SHA256_Maj(A, B, C);
            H = G, G = F, F = E, E = D + T1, D = C, C = B, B = A, A = T1 + T2;
        }
        H0 += A, H1 += B, H2 += C, H3 += D, H4 += E, H5 += F, H6 += G, H7 += H;
    }
    FreePool(pp - l);
    AsciiSPrint(sha256, 256, "%08X%08X%08X%08X%08X%08X%08X%08X", H0, H1, H2, H3, H4, H5, H6, H7);
    return sha256;
}

int StrSHA256_o(const char *str,  int length, char *sha256) {
    char *pp, *ppend;
    int l, i, W[64], T1, T2, A, B, C, D, E, F, G, H, H0, H1, H2, H3, H4, H5, H6, H7;
    H0 = 0x6a09e667, H1 = 0xbb67ae85, H2 = 0x3c6ef372, H3 = 0xa54ff53a;
    H4 = 0x510e527f, H5 = 0x9b05688c, H6 = 0x1f83d9ab, H7 = 0x5be0cd19;

    int  K[64] = {
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    };

    l = length + ((length % 64 >= 56) ? (128 - length % 64) : (64 - length % 64));
    if (!(pp = (char *)AllocatePool((unsigned int)l))) return 0;
    for (i = 0; i < length; pp[i + 3 - 2 * (i % 4)] = str[i], i++) ;
    for (pp[i + 3 - 2 * (i % 4)] = (char)128, i++; i < l; pp[i + 3 - 2 * (i % 4)] = 0, i++) ;
    *((int *)(pp + l - 4)) = length << 3;
    *((int *)(pp + l - 8)) = length >> 29;
    for (ppend = pp + l; pp < ppend; pp += 64) {
        for (i = 0; i < 16; W[i] = ((int *)pp)[i], i++) ;
        for (i = 16; i < 64; W[i] = (SHA256_O1(W[i - 2]) + W[i - 7] + SHA256_O0(W[i - 15]) + W[i - 16]), i++) ;
        A = H0, B = H1, C = H2, D = H3, E = H4, F = H5, G = H6, H = H7;
        for (i = 0; i < 64; i++) {
            T1 = H + SHA256_E1(E) + SHA256_Ch(E, F, G) + K[i] + W[i];
            T2 = SHA256_E0(A) + SHA256_Maj(A, B, C);
            H = G, G = F, F = E, E = D + T1, D = C, C = B, B = A, A = T1 + T2;
        }
        H0 += A, H1 += B, H2 += C, H3 += D, H4 += E, H5 += F, H6 += G, H7 += H;
    }
    FreePool(pp - l);
    AsciiSPrint(sha256, 256, "%08X%08X%08X%08X%08X%08X%08X%08X", H0, H1, H2, H3, H4, H5, H6, H7);
    return H0 ^ H1 ^ H2 ^ H3 ^ H4 ^ H5 ^ H6 ^ H7;
}

static const unsigned char sbox[256] = {    //static:内部变量  const：只读，不可变常量
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5,
    0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0,
    0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc,
    0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a,
    0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0,
    0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b,
    0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85,
    0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5,
    0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17,
    0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88,
    0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c,
    0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9,
    0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6,
    0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e,
    0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94,
    0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68,
    0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16,
};
//逆向S 盒矩阵
static const unsigned char contrary_sbox[256] = {
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38,
    0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87,
    0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d,
    0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e, //0x4e
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2,
    0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16,
    0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda,
    0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a,
    0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02,
    0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea,
    0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85,
    0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89,
    0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20,
    0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31,
    0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d,
    0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0,
    0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26,
    0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d,
};

static const unsigned char Rcon[10] = {
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36 };

static unsigned char x2time(unsigned char x) {
    if (x & 0x80) {
        return (((x << 1) ^ 0x1B) & 0xFF);
    }
    return x << 1;
}

static unsigned char x3time(unsigned char x) {
    return (x2time(x) ^ x);
}

static unsigned char x4time(unsigned char x) {
    return (x2time(x2time(x)));
}

static unsigned char x8time(unsigned char x) {
    return (x2time(x2time(x2time(x))));
}

static unsigned char x9time(unsigned char x) {    //9:1001
    return (x8time(x) ^ x);
}

static unsigned char xBtime(unsigned char x) {    //B:1011
    return (x8time(x) ^ x2time(x) ^ x);
}

static unsigned char xDtime(unsigned char x) {    //D:1101
    return (x8time(x) ^ x4time(x) ^ x);
}

static unsigned char xEtime(unsigned char x) {    //E:1110
    return (x8time(x) ^ x4time(x) ^ x2time(x));
}

static void MixColumns(unsigned char *col) { //列混合
    unsigned char tmp[4];
    //unsigned char xt[4];
    int i;
    for (i = 0; i < 4; i++, col += 4){  //col代表一列的基地址，col+4:下一列的基地址
        /*
        xt[0]=x2time(col[0]);
        xt[1]=x2time(col[1]);
        xt[2]=x2time(col[2]);
        xt[3]=x2time(col[3]);
        //xt[n]代表*2   xt[n]^col[n]代表*3   col[n]代表*1
        tmp[0]=(xt[0])^(xt[1]^col[1])^col[2]^col[3];    //2 3 1 1
        tmp[1]=col[0]^(xt[1])^(xt[2]^col[2])^col[3];    //1 2 3 1
        tmp[2]=col[0]^col[1]^(xt[2])^(xt[3]^col[3]);    //1 1 2 3
        tmp[3]=(xt[0]^col[0])^col[1]^col[2]^(xt[3]);    //3 1 1 2
        */
        tmp[0] = x2time(col[0]) ^ x3time(col[1]) ^ col[2] ^ col[3];    //2 3 1 1
        tmp[1] = col[0] ^ x2time(col[1]) ^ x3time(col[2]) ^ col[3];    //1 2 3 1
        tmp[2] = col[0] ^ col[1] ^ x2time(col[2]) ^ x3time(col[3]);    //1 1 2 3
        tmp[3] = x3time(col[0]) ^ col[1] ^ col[2] ^ x2time(col[3]);    //3 1 1 2
                                                                       //修改后的值 直接在原矩阵上修改
        col[0] = tmp[0];
        col[1] = tmp[1];
        col[2] = tmp[2];
        col[3] = tmp[3];
    }
}

static void Contrary_MixColumns(unsigned char *col) {
    unsigned char tmp[4];
    //unsigned char xt2[4];//colx2
    //unsigned char xt4[4];//colx4
    //unsigned char xt8[4];//colx8
    int x;
    for (x = 0; x < 4; x++, col += 4) {
        tmp[0] = xEtime(col[0]) ^ xBtime(col[1]) ^ xDtime(col[2]) ^ x9time(col[3]);
        tmp[1] = x9time(col[0]) ^ xEtime(col[1]) ^ xBtime(col[2]) ^ xDtime(col[3]);
        tmp[2] = xDtime(col[0]) ^ x9time(col[1]) ^ xEtime(col[2]) ^ xBtime(col[3]);
        tmp[3] = xBtime(col[0]) ^ xDtime(col[1]) ^ x9time(col[2]) ^ xEtime(col[3]);
        col[0] = tmp[0];
        col[1] = tmp[1];
        col[2] = tmp[2];
        col[3] = tmp[3];
    }
}

static void ShiftRows(unsigned char *col) { //正向行移位
    unsigned char t;

    t = col[1]; col[1] = col[5]; col[5] = col[9]; col[9] = col[13]; col[13] = t;
    t = col[2]; col[2] = col[10]; col[10] = t;
    t = col[6]; col[6] = col[14]; col[14] = t;
    t = col[15]; col[15] = col[11]; col[11] = col[7]; col[7] = col[3]; col[3] = t;
}

static void Contrary_ShiftRows(unsigned char *col) {
    unsigned char t;
    /*1nd row*/
    t = col[13]; col[13] = col[9]; col[9] = col[5]; col[5] = col[1]; col[1] = t;
    /*2rd row*/
    t = col[2]; col[2] = col[10]; col[10] = t;
    t = col[6]; col[6] = col[14]; col[14] = t;
    /*3th row*/
    t = col[3]; col[3] = col[7]; col[7] = col[11]; col[11] = col[15]; col[15] = t;
    /*4th row*/    //第4行不移位
}

static void SubBytes(unsigned char *col) { //字节代换
    int x;
    for (x = 0; x < 16; x++) {
        col[x] = sbox[col[x]];
    }
}

static void Contrary_SubBytes(unsigned char *col) {
    int x;
    for (x = 0; x < 16; x++) {
        col[x] = contrary_sbox[col[x]];
    }
}

static void AddRoundKey(unsigned char *col, unsigned char *expansionkey, int round) { //密匙加
    int x;
    for (x = 0; x < 16; x++)   {
        col[x] ^= expansionkey[(round << 4) + x];
    }
}

void AesEncrypt(unsigned char *blk, unsigned char *expansionkey, int Nr) { //加密一个区块
    int round;

    AddRoundKey(blk, expansionkey, 0);
    //第1-9轮：4类操作：字节代换、行移位、列混合、轮密钥加
    for (round = 1; round <= (Nr - 1); round++)    {
        SubBytes(blk);        //输入16字节数组，直接在原数组上修改
        ShiftRows(blk);        //输入16字节数组，直接在原数组上修改
        MixColumns(blk);    //输入16字节数组，直接在原数组上修改
        AddRoundKey(blk, expansionkey, round);
    }
    //第10轮：不进行列混合
    SubBytes(blk);
    ShiftRows(blk);
    AddRoundKey(blk, expansionkey, Nr);
}

void Contrary_AesEncrypt(unsigned char *blk, unsigned char *expansionkey, int Nr) {
    int x;
    /* unsigned char *contrary_key=key;
    for(x=0;x<11;x++,key+=16)
    Contrary_MixColumns(key);*/
    AddRoundKey(blk, expansionkey, Nr);
    Contrary_ShiftRows(blk);
    Contrary_SubBytes(blk);
    for (x = (Nr - 1); x >= 1; x--) {
        AddRoundKey(blk, expansionkey, x);
        Contrary_MixColumns(blk);
        Contrary_ShiftRows(blk);
        Contrary_SubBytes(blk);
    }
    AddRoundKey(blk, expansionkey, 0);
}

void ScheduleKey(unsigned char *inkey, unsigned char *outkey, int Nk, int Nr) { //安排一个保密密钥使用

    unsigned char temp[4], t;
    int x, i;

    for (i = 0; i < (4 * Nk); i++) {
        outkey[i] = inkey[i];
    }

    i = Nk;
    while (i < (4 * (Nr + 1))) { //i=4~43 WORD 32bit的首字节地址，每一个4字节
        for (x = 0; x < 4; x++) temp[x] = outkey[(4 * (i - 1)) + x];    //i：32bit的首字节地址

        if (i % Nk == 0) {

            t = temp[0]; temp[0] = temp[1]; temp[1] = temp[2]; temp[2] = temp[3]; temp[3] = t;
            for (x = 0; x < 4; x++) {
                temp[x] = sbox[temp[x]];
            }
            temp[0] ^= Rcon[(i / Nk) - 1];
        }
        //else if(Nk>6 && (i%Nk)==4)    //Nk>6的算法不同，暂时用不到
        //{
        //    /*SubWord*/
        //    for(x=0;x<4;x++)
        //    {
        //        temp[x]=sbox[temp[x]];
        //    }
        //}

        /*w[i] = w[i-4]^w[i-1]*/
        for (x = 0; x < 4; x++) {
            outkey[(4 * i) + x] = outkey[(4 * (i - Nk)) + x] ^ temp[x];
        }
        ++i;
    }
}

void EncMem(void *buf, int size) {
    unsigned char key[17] = { 0 };
    unsigned char expansionkey[15 * 16];
    unsigned char *p = (unsigned char *)buf;
    int i = 0, count = 0;
    AsciiSPrint((char *)key, sizeof(key), "%a", "830730");
    for (i = 0; i < 16 && i < size; ++i) {
        if (p[i] > 128) ++count;
    }
    if (count > 1) return;
    key[16] = 0;
    ScheduleKey(key, expansionkey, 4, 10);
    while (size >= 16) {
        AesEncrypt(p, expansionkey, 10);
        p += 16;
        size -= 16;
    }
}

void DecMem(void *buf, int size) {
    unsigned char key[17] = { 0 };
    unsigned char expansionkey[15 * 16];
    unsigned char *p = (unsigned char *)buf;
    int i = 0, count = 0;

    ZeroMem(key, sizeof(key));
    ZeroMem(expansionkey, sizeof(expansionkey));
    AsciiSPrint((char *)key, sizeof(key), "%a", "830730");
    for (i = 0; i < 16 && i < size; ++i) {
        if (p[i] > 128) ++count;
    }
    if (count < 1) return;
    ScheduleKey(key, expansionkey, 4, 10);

    while (size >= 16) {
        Contrary_AesEncrypt(p, expansionkey, 10);
        p += 16;
        size -= 16;
    }
}

char* strstr_tn(char *s1, char *s2, int length) {
    int l2;

    l2 = AsciiStrLen(s2);
    if (!l2) return (char *)s1;
    while (length >= l2) {
        length--;
        if (!CompareMem(s1, s2, l2)) return (char *)s1;
        s1++;
    }
    return NULL;
}

static int GetPropFromMisc2(char *buf, const char *prop, char *val) {
    char *p;
    int i;
    int length;

    if (buf == NULL || prop == NULL) {
        return -1;
    }
    p = AsciiStrStr(buf, prop);
    if (p != NULL) {
        i = p - buf;
        length = 0;
        while ((buf[i + length] != '\n')
               && (buf[i + length] != ' ')
               && (buf[i + length] != '\0')) {
            val[length] = buf[i + length];
            if (++length > 64) break;
        }
        DEBUG((EFI_D_ERROR, "get prop %a from misc2: %a\n", prop, val));
    }
    return 0;
}

static void Misc2InfoDataInit(void) {
    ZeroMem(&misc2Info, sizeof(misc2Info));
}

#define Header_len 24

int Misc2InfoParse(int key) {
    int read_part_ret, length;
    char *read_part_buf;
    char *p, *q;
    char hashbuffer[256];
    char hashbuffer_misc2[256];
    //int nRetCode = 0;
    //char lock_down[32]={0};
    char chipid[64] = { 0 };
    //unsigned char rid_sz=64;
    UINT32 data_hash;
    //UINT32 data_otp=0x5A;
    UINT32 read_size = 4096;

    extern int efuse_sbc_enabled(void);

    read_part_buf = (char *)AllocatePool(read_size);
    if (read_part_buf == NULL) {
        DEBUG((EFI_D_ERROR, "tn_Boot_Misc2_Parser find config_version&config_end error\n"));
        AsciiStrCpy((CHAR8*)(misc2Info.info + E_CARRIER), " FAIL:malloc error");
        FreePool(read_part_buf);
        return TINNO_INIT_MALLOC_FAIL;
    }

    read_part_ret = LoadImageFromPartition(read_part_buf, &read_size, L"misc2");
    if (read_part_ret < 0) {
        AsciiStrCpy((CHAR8*)(misc2Info.info + E_CARRIER), " FAIL:no partion");
        DEBUG((EFI_D_ERROR, "do have partition misc2\n"));
        FreePool(read_part_buf);
        return TINNO_INIT_PARTITION_ERROR;
    }

    p = AsciiStrStr(read_part_buf, "tn_config_version=002");
    q = strstr_tn(read_part_buf, "tn_config_end", 4096);
    if (p == NULL || q == NULL) {
        DEBUG((EFI_D_ERROR, "tn_Boot_Misc2_Parser find config_version&config_end error\n"));
        AsciiStrCpy((CHAR8*)(misc2Info.info + E_CARRIER), " FAIL:partition format error");
        FreePool(read_part_buf);
        return TINNO_INIT_PARTITION_ERROR;
    }

    length = (int)(q - p);
    if (length < 4096) {
        ZeroMem(hashbuffer, sizeof(hashbuffer));
        ZeroMem(hashbuffer_misc2, sizeof(hashbuffer_misc2));

        StrSHA256(&read_part_buf[Header_len], length - Header_len, &hashbuffer_misc2[0]);
        AsciiSPrint(hashbuffer, 256, "tn_config_end_hash=");
        if (1 == key) {
            AsciiStrCpy(&hashbuffer_misc2[64], "salt730830");
            data_hash = StrSHA256_o(&hashbuffer_misc2[0],  64 + AsciiStrLen("salt730830"), &hashbuffer[19]);
        } else if (2 == key) {
            AsciiStrCpy(&hashbuffer_misc2[64], "salt410v1");
            data_hash = StrSHA256_o(&hashbuffer_misc2[0],  64 + AsciiStrLen("salt410v1"), &hashbuffer[19]);
        } else if (3 == key) {
            AsciiStrCpy(&hashbuffer_misc2[64], "saltkey3");
            data_hash = StrSHA256_o(&hashbuffer_misc2[0],  64 + AsciiStrLen("saltkey3"), &hashbuffer[19]);
        } else {
            AsciiStrCpy(&hashbuffer_misc2[64], "salt860168");
            data_hash = StrSHA256_o(&hashbuffer_misc2[0],  64 + AsciiStrLen("salt860168"), &hashbuffer[19]);
        }
        //printf("code:%x\n", data_hash);
        DEBUG((EFI_D_ERROR, "code:%x\n", data_hash));
        ZeroMem(hashbuffer_misc2, sizeof(hashbuffer_misc2));
        AsciiStrCpy(hashbuffer_misc2, q);

        if (0 != AsciiStrCmp(hashbuffer_misc2, hashbuffer)) {
            DEBUG((EFI_D_ERROR, "hash miss match!!!!!!!!!!!!\n"));
            AsciiStrCpy((CHAR8*)(misc2Info.info + E_CARRIER), " FAIL:hash miss match");
            FreePool(read_part_buf);
            return TINNO_INIT_MD5_MISMATCH;
        } else
            DEBUG((EFI_D_ERROR, "misc2 partition check hash successful!!!\n"));

        //decrypt(&read_part_buf[Header_len], length-Header_len);
        DecMem(&read_part_buf[Header_len], length - Header_len);
        DEBUG((EFI_D_ERROR, "---------------------------------------------\n"));
        DEBUG((EFI_D_ERROR, "%a\n", read_part_buf));
        DEBUG((EFI_D_ERROR, "---------------------------------------------\n"));

        GetPropFromMisc2(read_part_buf, STR_MISC2_CARRIER, (char*)(misc2Info.info + E_CARRIER));
        GetPropFromMisc2(read_part_buf, STR_MISC2_SIMNUMBER, (char*)(misc2Info.info + E_SIM_NUMBER));
        GetPropFromMisc2(read_part_buf, STR_MISC2_MISC2VER, (char*)(misc2Info.info + E_MISC_VERSION));
        GetPropFromMisc2(read_part_buf, STR_MISC2_HWSKU, (char*)(misc2Info.info + E_HARDWARE_SKU));
        GetPropFromMisc2(read_part_buf, STR_MISC2_SIMLOCK, (char*)(misc2Info.info + E_SIM_LOCK));
        GetPropFromMisc2(read_part_buf, STR_MISC2_BLUR, (char*)(misc2Info.info + E_BLUR_STRING));
        GetPropFromMisc2(read_part_buf, STR_MISC2_SVNKIT, (char*)(misc2Info.info + E_SVNKIT));
        GetPropFromMisc2(read_part_buf, STR_MISC2_ESIMSTAT, (char*)(misc2Info.info + E_ESIMSTAT));
        if (read_part_buf[4080] == '1') {
            DEBUG((EFI_D_ERROR, "update esim state to 1\n"));
            AsciiSPrint((CHAR8*)(misc2Info.info + E_ESIMSTAT), BUFF_MAX, "%a", "esim_state=1");
        } else if (read_part_buf[4080] == '2') {
            DEBUG((EFI_D_ERROR, "update esim state to 2\n"));
            AsciiSPrint((CHAR8*)(misc2Info.info + E_ESIMSTAT), BUFF_MAX, "%a", "esim_state=2");
        }

        ZeroMem(hashbuffer_misc2, sizeof(hashbuffer_misc2)); 
        CopyMem(&hashbuffer_misc2[0], &hashbuffer[19], 64);

        ZeroMem(chipid, 64);
        ZeroMem(hashbuffer, sizeof(hashbuffer));
        //get_hrid(hashbuffer, &rid_sz);
        AsciiSPrint(chipid, 64, "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x",
                    hashbuffer[0], hashbuffer[1], hashbuffer[2], hashbuffer[3], hashbuffer[4],
                    hashbuffer[5], hashbuffer[6], hashbuffer[7], hashbuffer[8], hashbuffer[9],
                    hashbuffer[10], hashbuffer[11], hashbuffer[12], hashbuffer[13], hashbuffer[14], hashbuffer[15]);
        AsciiStrCpy(&hashbuffer_misc2[64], chipid);

        AsciiStrCpy(&hashbuffer_misc2[96], "jhq867");
        ZeroMem(hashbuffer, sizeof(hashbuffer));
        AsciiSPrint(hashbuffer, 256, "tn_config_sig=");
        StrSHA256(&hashbuffer_misc2[0],  64 + 32 + AsciiStrLen("jhq867"), &hashbuffer[14]);

        ZeroMem(hashbuffer_misc2, sizeof(hashbuffer_misc2));
        q = strstr_tn(read_part_buf, "tn_config_sig=", 4096);
        if (q == NULL) {
            DEBUG((EFI_D_ERROR, "tn_Boot_Misc2_Parsersig sig error\n"));
            FreePool(read_part_buf);
            //if(efuse_sbc_enabled()) {
            //	AsciiStrCpy(tinno_board_info.tinno_carrier_misc2, " FAIL:sig error");
            //	return TINNO_INIT_MD5_MISMATCH;
            //}
            //else
            //	return TINNO_INIT_OK;
            return TINNO_INIT_OK;
        }
        AsciiStrCpy(hashbuffer_misc2, q);
        DEBUG((EFI_D_ERROR, "tn misc2 sig  , %s\n", hashbuffer));
        DEBUG((EFI_D_ERROR, "tn misc2 sig  from misc2 , %s\n", hashbuffer_misc2));

        if (0 != AsciiStrCmp(hashbuffer_misc2, hashbuffer)) {
            DEBUG((EFI_D_ERROR, "sig miss match!!!!!!!!!!!!\n"));
            AsciiStrCpy((CHAR8*)(misc2Info.info + E_CARRIER), " FAIL:sig mistach");
        } else
            DEBUG((EFI_D_ERROR, "misc2 partition check sig successful!!!\n"));
    } else {
        AsciiStrCpy((CHAR8*)(misc2Info.info + E_CARRIER), " FAIL:length error");
        FreePool(read_part_buf);
        return  TINNO_INIT_LEN_OVERFLOW;
    }

    FreePool(read_part_buf);
    return TINNO_INIT_OK;
}

int  Misc2InfoInit(int key) {
    Misc2InfoDataInit();
    return Misc2InfoParse(key);
}

void Misc2AddCmdLine(char *cmdBuf, int bufLen)
{
    char buf[64];

    if (cmdBuf == NULL) {
        return;
    }

    if (Misc2InfoInit(2) != TINNO_INIT_OK) {
        return;
    }

    for (int i = 0; i <= E_ESIMSTAT; i++) {
        if (AsciiStrLen(misc2Info.info[i])) {
            ZeroMem(buf, sizeof(buf));
            AsciiSPrint (buf, sizeof(buf), " androidboot.%a", misc2Info.info[i]);
            AsciiStrnCatS (cmdBuf, bufLen, buf, AsciiStrLen(buf));
        }
    }
}

void Misc2SetFdt(void * fdt)
{
    int ret;
    int offset;
    char *p;
    EFI_SMEM_PROTOCOL * smem_protocol;
    EFI_STATUS status;
    SkuBoardIDSmemType sku;
    SkuBoardIDSmemType *addr = NULL;

    ZeroMem(&sku, sizeof(sku));
    offset = fdt_path_offset(fdt, "/firmware/android");
    for (int i = 0; i <= E_ESIMSTAT; i++) {
        if (AsciiStrLen(misc2Info.info[i])) {
            p = AsciiStrStr(misc2Info.info[i], "=");
            if (!p) {
                continue;
            }
            *p++ = 0;

            if (!AsciiStriCmp(p, "V800")) {
                sku.skuid_idx = 1;
            } else if (!AsciiStriCmp(p, "V900")) {
                sku.skuid_idx = 2;
            }
            ret = fdt_setprop_string(fdt, offset, misc2Info.info[i], p); 
            if (ret < 0) {
                DEBUG((EFI_D_ERROR, "Misc2: setprop failed\n"));
            }
        }
    }

    // set sku id to smem for modem
    status = gBS->LocateProtocol(&gEfiSMEMProtocolGuid, NULL, (void**)&smem_protocol);
    if(status != EFI_SUCCESS) {
        DEBUG ((EFI_D_ERROR, "Unable to locate smem protocol; status=0x%x\n", status));
        return;
    }
    status = smem_protocol->SmemAlloc(SMEM_ID_VENDOR1, sizeof(sku), (void **)&addr);
    if(status != EFI_SUCCESS) {
        DEBUG ((EFI_D_ERROR, "Unable to alloc smem; status=0x%x\n", status));
        return;
    }
    DEBUG ((EFI_D_ERROR, "Set skuid_idx %d to smem SMEM_ID_VENDOR1\n", sku.skuid_idx));
    CopyMem(addr, &sku, sizeof(sku));
}
