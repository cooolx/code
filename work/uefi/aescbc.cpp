#include <time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>


#define SHA256_ROTL(a,b) (((a>>(32-b))&(0x7fffffff>>(31-b)))|(a<<b))
#define SHA256_SR(a,b) ((a>>b)&(0x7fffffff>>(b-1)))
#define SHA256_Ch(x,y,z) ((x&y)^((~x)&z))
#define SHA256_Maj(x,y,z) ((x&y)^(x&z)^(y&z))
#define SHA256_E0(x) (SHA256_ROTL(x,30)^SHA256_ROTL(x,19)^SHA256_ROTL(x,10))
#define SHA256_E1(x) (SHA256_ROTL(x,26)^SHA256_ROTL(x,21)^SHA256_ROTL(x,7))
#define SHA256_O0(x) (SHA256_ROTL(x,25)^SHA256_ROTL(x,14)^SHA256_SR(x,3))
#define SHA256_O1(x) (SHA256_ROTL(x,15)^SHA256_ROTL(x,13)^SHA256_SR(x,10))

char* StrSHA256(const char* str,  int length, char* sha256){

    char *pp, *ppend;
    int l, i, W[64], T1, T2, A, B, C, D, E, F, G, H, H0, H1, H2, H3, H4, H5, H6, H7;
    H0 = 0x6a09e667, H1 = 0xbb67ae85, H2 = 0x3c6ef372, H3 = 0xa54ff53a;
    H4 = 0x510e527f, H5 = 0x9b05688c, H6 = 0x1f83d9ab, H7 = 0x5be0cd19;
	
    unsigned int  K[64] = {
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
    if (!(pp = (char*)malloc((unsigned int)l)))
		return 0;
    for (i = 0; i < length; pp[i + 3 - 2 * (i % 4)] = str[i], i++);
    for (pp[i + 3 - 2 * (i % 4)] = 128, i++; i < l; pp[i + 3 - 2 * (i % 4)] = 0, i++);
    *((int*)(pp + l - 4)) = length << 3;
    *((int*)(pp + l - 8)) = length >> 29;
    for (ppend = pp + l; pp < ppend; pp += 64){
        for (i = 0; i < 16; W[i] = ((int*)pp)[i], i++);
        for (i = 16; i < 64; W[i] = (SHA256_O1(W[i - 2]) + W[i - 7] + SHA256_O0(W[i - 15]) + W[i - 16]), i++);
        A = H0, B = H1, C = H2, D = H3, E = H4, F = H5, G = H6, H = H7;
        for (i = 0; i < 64; i++){
            T1 = H + SHA256_E1(E) + SHA256_Ch(E, F, G) + K[i] + W[i];
            T2 = SHA256_E0(A) + SHA256_Maj(A, B, C);
            H = G, G = F, F = E, E = D + T1, D = C, C = B, B = A, A = T1 + T2;
        }
        H0 += A, H1 += B, H2 += C, H3 += D, H4 += E, H5 += F, H6 += G, H7 += H;
    }
    free(pp - l);
    sprintf(sha256, "%08X%08X%08X%08X%08X%08X%08X%08X", H0, H1, H2, H3, H4, H5, H6, H7);
    return sha256;
}

int StrSHA256_otp(const char* str,  int length, char* sha256){

    char *pp, *ppend;
    int l, i, W[64], T1, T2, A, B, C, D, E, F, G, H, H0, H1, H2, H3, H4, H5, H6, H7;
    H0 = 0x6a09e667, H1 = 0xbb67ae85, H2 = 0x3c6ef372, H3 = 0xa54ff53a;
    H4 = 0x510e527f, H5 = 0x9b05688c, H6 = 0x1f83d9ab, H7 = 0x5be0cd19;
	
    unsigned int  K[64] = {
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
    if (!(pp = (char*)malloc((unsigned int)l)))
		return 0;
    for (i = 0; i < length; pp[i + 3 - 2 * (i % 4)] = str[i], i++);
    for (pp[i + 3 - 2 * (i % 4)] = 128, i++; i < l; pp[i + 3 - 2 * (i % 4)] = 0, i++);
    *((int*)(pp + l - 4)) = length << 3;
    *((int*)(pp + l - 8)) = length >> 29;
    for (ppend = pp + l; pp < ppend; pp += 64){
        for (i = 0; i < 16; W[i] = ((int*)pp)[i], i++);
        for (i = 16; i < 64; W[i] = (SHA256_O1(W[i - 2]) + W[i - 7] + SHA256_O0(W[i - 15]) + W[i - 16]), i++);
        A = H0, B = H1, C = H2, D = H3, E = H4, F = H5, G = H6, H = H7;
        for (i = 0; i < 64; i++){
            T1 = H + SHA256_E1(E) + SHA256_Ch(E, F, G) + K[i] + W[i];
            T2 = SHA256_E0(A) + SHA256_Maj(A, B, C);
            H = G, G = F, F = E, E = D + T1, D = C, C = B, B = A, A = T1 + T2;
        }
        H0 += A, H1 += B, H2 += C, H3 += D, H4 += E, H5 += F, H6 += G, H7 += H;
    }
    free(pp - l);
    sprintf(sha256, "%08X%08X%08X%08X%08X%08X%08X%08X", H0, H1, H2, H3, H4, H5, H6, H7);
    return H0^H1^H2^H3^H4^H5^H6^H7;
}

char* FileSHA256(const char* file, char* sha256){

    FILE* fh;
    char* addlp, T[64];
    int addlsize, j, W[64], T1, T2, A, B, C, D, E, F, G, H, H0, H1, H2, H3, H4, H5, H6, H7;
    long long length, i, cpys;
    void *pp, *ppend;
    H0 = 0x6a09e667, H1 = 0xbb67ae85, H2 = 0x3c6ef372, H3 = 0xa54ff53a;
    H4 = 0x510e527f, H5 = 0x9b05688c, H6 = 0x1f83d9ab, H7 = 0x5be0cd19;
    unsigned int K[64] = {
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    };
    fh = fopen(file, "rb");
    fseek(fh, 0, SEEK_END);
    length = ftello64(fh);	  //_ftelli64
    addlsize = (56 - length % 64 > 0) ? (64) : (128);
    if (!(addlp = (char*)malloc(addlsize))) return 0;
    cpys = ((length - (56 - length % 64)) > 0) ? (length - length % 64) : (0);
    j = (int)(length - cpys);
    if (!(pp = (char*)malloc(j))) return 0;
    fseek(fh, -j, SEEK_END);
    fread(pp, 1, j, fh);
    for (i = 0; i < j; addlp[i + 3 - 2 * (i % 4)] = ((char*)pp)[i], i++);
    free(pp);
    for (addlp[i + 3 - 2 * (i % 4)] = 128, i++; i < addlsize; addlp[i + 3 - 2 * (i % 4)] = 0, i++);
    *((int*)(addlp + addlsize - 4)) = length << 3;
    *((int*)(addlp + addlsize - 8)) = length >> 29;
    for (rewind(fh); 64 == fread(W, 1, 64, fh);){
        for (i = 0; i < 64; T[i + 3 - 2 * (i % 4)] = ((char*)W)[i], i++);
        for (i = 0; i < 16; W[i] = ((int*)T)[i], i++);
        for (i = 16; i < 64; W[i] = (SHA256_O1(W[i - 2]) + W[i - 7] + SHA256_O0(W[i - 15]) + W[i - 16]), i++);
        A = H0, B = H1, C = H2, D = H3, E = H4, F = H5, G = H6, H = H7;
        for (i = 0; i < 64; i++){
            T1 = H + SHA256_E1(E) + SHA256_Ch(E, F, G) + K[i] + W[i];
            T2 = SHA256_E0(A) + SHA256_Maj(A, B, C);
            H = G, G = F, F = E, E = D + T1, D = C, C = B, B = A, A = T1 + T2;
        }
        H0 += A, H1 += B, H2 += C, H3 += D, H4 += E, H5 += F, H6 += G, H7 += H;
    }
    for (pp = addlp, ppend = addlp + addlsize; pp < ppend; pp = (int*)pp + 16){
        for (i = 0; i < 16; W[i] = ((int*)pp)[i], i++);
        for (i = 16; i < 64; W[i] = (SHA256_O1(W[i - 2]) + W[i - 7] + SHA256_O0(W[i - 15]) + W[i - 16]), i++);
        A = H0, B = H1, C = H2, D = H3, E = H4, F = H5, G = H6, H = H7;
        for (i = 0; i < 64; i++){
            T1 = H + SHA256_E1(E) + SHA256_Ch(E, F, G) + K[i] + W[i];
            T2 = SHA256_E0(A) + SHA256_Maj(A, B, C);
            H = G, G = F, F = E, E = D + T1, D = C, C = B, B = A, A = T1 + T2;
        }
        H0 += A, H1 += B, H2 += C, H3 += D, H4 += E, H5 += F, H6 += G, H7 += H;
    }
    free(addlp); 
    fclose(fh);
    sprintf(sha256, "%08X%08X%08X%08X%08X%08X%08X%08X", H0, H1, H2, H3, H4, H5, H6, H7);
    return sha256;
}

#define AESKEY	"jhq867860168"
#define AESIV	"71412E2299B0EEA5"    //cbc模式第一个异或块
 
typedef unsigned long DWORD;
typedef unsigned char UCHAR,*PUCHAR;
typedef void *PVOID,*LPVOID;
typedef unsigned char byte;
typedef DWORD *PDWORD,*LPDWORD;
 
#ifndef VOID
#define VOID void
#endif
 

#define Bits128	16
#define Bits192	24
#define Bits256	32
 
 
void Aes_init(int keySize, unsigned char* keyBytes);
void Cipher(unsigned char* input, unsigned char* output);  // encipher 16-bit input
void InvCipher(unsigned char* input, unsigned char* output);  // decipher 16-bit input




int Nb;         // block size in 32-bit words.  Always 4 for AES.  (128 bits).
int Nk;         // key size in 32-bit words.  4, 6, 8.  (128, 192, 256 bits).
int Nr;         // number of rounds. 10, 12, 14.

unsigned char key[32]={0};
unsigned char w[16*15]={0};
unsigned char State[4][4]={0};
static unsigned char AesSbox[16*16]=
{// populate the Sbox matrix
 /* 0     1     2     3     4     5     6     7     8     9     a     b     c     d     e     f */
 /*0*/  0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
 /*1*/  0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
 /*2*/  0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
 /*3*/  0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
 /*4*/  0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
 /*5*/  0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
 /*6*/  0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
 /*7*/  0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
 /*8*/  0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
 /*9*/  0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
 /*a*/  0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
 /*b*/  0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
 /*c*/  0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
 /*d*/  0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
 /*e*/  0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
 /*f*/  0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
};
 
static unsigned char AesiSbox[16*16]=
{
    // populate the iSbox matrix
    /* 0     1     2     3     4     5     6     7     8     9     a     b     c     d     e     f */
    /*0*/  0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    /*1*/  0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    /*2*/  0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
    /*3*/  0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    /*4*/  0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    /*5*/  0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    /*6*/  0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    /*7*/  0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    /*8*/  0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    /*9*/  0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    /*a*/  0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    /*b*/  0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    /*c*/  0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    /*d*/  0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    /*e*/  0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    /*f*/  0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
};
static unsigned char AesRcon[11*4]=
{
    0x00, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00,
    0x04, 0x00, 0x00, 0x00,
    0x08, 0x00, 0x00, 0x00,
    0x10, 0x00, 0x00, 0x00,
    0x20, 0x00, 0x00, 0x00,
    0x40, 0x00, 0x00, 0x00,
    0x80, 0x00, 0x00, 0x00,
    0x1b, 0x00, 0x00, 0x00,
    0x36, 0x00, 0x00, 0x00
};

void SetNbNkNr(int keySize)
{
	Nb=4;
	if(keySize==Bits128)
	{
		Nk=4;    //4*4字节，128位密钥，10轮加密
		Nr=10;
	}
	else if(keySize==Bits192)
	{
		Nk=6;    //6*4字节，192位密钥，12轮加密
		Nr=12;
	}
	else if(keySize==Bits256)
	{
		Nk=8;    //8*4字节，256位密钥，14轮加密
		Nr=14;
	}
}

//密钥移位函数
 unsigned char*  RotWord(unsigned char word[])
{
	static byte temp[4];
	temp[0] = word[1];
	temp[1] = word[2];
	temp[2] = word[3];
	temp[3] = word[0];
	return temp;
}

//密钥字代换函数
 unsigned char*  SubWord(unsigned char word[])
{
	int j;
	static byte temp[4];
	
	for( j=0;j<4;j++)
	{
		temp[j] = AesSbox[16*(word[j] >> 4)+(word[j] & 0x0f)];  //实际上也可以写成AesSbox[[j]];因为两者相等
	}
	return temp;
 
}

void KeyExpansion()
{
  int row,i,j,k;
   byte temp[4];	
   byte *ret;
	memset(w,0,16*15);
	for( row=0;row<Nk;row++)       //拷贝seed 密钥
	{
		w[4*row+0] =  key[4*row];
		w[4*row+1] =  key[4*row+1];
		w[4*row+2] =  key[4*row+2];
		w[4*row+3] =  key[4*row+3];
	}
	
	for( row=Nk;row<4*(Nr+1);row++)
	{
		temp[0]=w[4*row-4];     //当前列的前一列  
		temp[1]=w[4*row-3];
		temp[2]=w[4*row-2];
		temp[3]=w[4*row-1];
		if(row%Nk==0)           //逢nk时，对当前列的前一列作特殊处理
		{
			ret=SubWord(RotWord(temp));   //先移位，再代换，最后和轮常量异或
			temp[0] =ret[0];
			temp[1] =ret[1];
			temp[2] =ret[2];
			temp[3] =ret[3];
			temp[0] = (byte)( (int)temp[0] ^ (int) AesRcon[4*(row/Nk)+0] );   
			temp[1] = (byte)( (int)temp[1] ^ (int) AesRcon[4*(row/Nk)+1] );
			temp[2] = (byte)( (int)temp[2] ^ (int) AesRcon[4*(row/Nk)+2] );
			temp[3] = (byte)( (int)temp[3] ^ (int) AesRcon[4*(row/Nk)+3] );
			/*ret[0]=(byte)( (int)ret[0] ^ (int) AesRcon[4*(row/Nk)+0] ); 
      ret[1] = (byte)( (int)ret[1] ^ (int) AesRcon[4*(row/Nk)+1] );
			ret[2] = (byte)( (int)ret[2] ^ (int) AesRcon[4*(row/Nk)+2] );
			ret[3] = (byte)( (int)ret[3] ^ (int) AesRcon[4*(row/Nk)+3] );		*/		
		}
		else if ( Nk > 6 && (row % Nk == 4) )  
		{
			ret = SubWord(temp);
			temp[0] =ret[0];
			temp[1] =ret[1];
			temp[2] =ret[2];
			temp[3] =ret[3];
		}
 
		// w[row] = w[row-Nk] xor temp
		w[4*row+0] = (byte) ( (int) w[4*(row-Nk)+0] ^ (int)temp[0] );
		w[4*row+1] = (byte) ( (int) w[4*(row-Nk)+1] ^ (int)temp[1] );
		w[4*row+2] = (byte) ( (int) w[4*(row-Nk)+2] ^ (int)temp[2] );
		w[4*row+3] = (byte) ( (int) w[4*(row-Nk)+3] ^ (int)temp[3] );
		/*w[4*row+0] = (byte) ( (int) w[4*(row-Nk)+0] ^ (int)ret[0] );
		w[4*row+1] = (byte) ( (int) w[4*(row-Nk)+1] ^ (int)ret[1] );
		w[4*row+2] = (byte) ( (int) w[4*(row-Nk)+2] ^ (int)ret[2] );
		w[4*row+3] = (byte) ( (int) w[4*(row-Nk)+3] ^ (int)ret[3] );*/
	}  // for loop
		
}

void Aes_init(int keysize,unsigned char* keyBytes)
{
	SetNbNkNr(keysize);                         //设置密钥块数，轮数 
	memcpy(key,keyBytes,keysize);				//字符串拷贝函数，把keyBytes的keysize个字符复制到key中
	KeyExpansion();								//密钥扩展，必须提前做的初始化	
}

//轮密钥加
void  AddRoundKey(int round)
{
	int i,j;  //i行 j列           //因为密钥w是一列一列排列的，即 k0 k4 k8 k12
	for(j=0;j<4;j++)			  //							  k1 k5 k9 k13
	{							  //							  k2 k6 k10k14
		for(i=0;i<4;i++)		  //							  k3 k7 k11k15
		{						  // 所以i行j列的下标是4*((round*4)+j)+i即16*round+4*j+i
			State[i][j]=(unsigned char)((int)State[i][j]^(int)w[4*((round*4)+j)+i]);  
		}
	}
}

//字节代换函数
void  SubBytes()                              //Page 103
{
	int i,j;
	for(j=0;j<4;j++)
	{
		for(i=0;i<4;i++)
		{
			State[i][j]=AesSbox[State[i][j]];
			//因为 16*(State[i][j]>>4)+State[i][j]&0x0f=State[i][j]
 
 
		}
	}
}
 
void  InvSubBytes(void)
{
	int i,j;
	for(j=0;j<4;j++)
	{
		for(i=0;i<4;i++)
		{
			State[i][j]=AesiSbox[State[i][j]]; //因为 16*(State[i][j]>>4)+State[i][j]&0x0f=State[i][j]
		}
	}
 
}

void  ShiftRows(void)
{
	unsigned char temp[4*4];                                        //Page105
	int i,j;
	for(j=0;j<4;j++)
	{
		for(i=0;i<4;i++)
		{
			temp[4*i+j]=State[i][j];
		}
	}
	for(i=1;i<4;i++)
	{
		for(j=0;j<4;j++)
		{
			if(i==1)State[i][j]=temp[4*i+(j+1)%4];					//第一行左移1位
			else if(i==2)State[i][j]=temp[4*i+(j+2)%4];				//第二行左移2位
			else if(i==3)State[i][j]=temp[4*i+(j+3)%4];				//第三行左移3位
		}
	}
 
}

void  InvShiftRows(void)
{
	unsigned char temp[4*4];
	int i,j;
	for(j=0;j<4;j++)
	{
		for(i=0;i<4;i++)
		{
			temp[4*i+j]=State[i][j];
		}
	}
	for(i=1;i<4;i++)
	{
		for(j=0;j<4;j++)
		{
			//if(i==1)State[i][j]=temp[4*i+(j-1)%4];    在此犯了一个错误 -1%4=-1 而不是3，所以采用了下面再加一个4的做法
			if(i==1)State[i][j]=temp[4*i+(j+3)%4];			//第一行右移1位 j-1+4=j+3
			else if(i==2)State[i][j]=temp[4*i+(j+2)%4];		//第二行右移2位 j-2+4=j+2
			else if(i==3)State[i][j]=temp[4*i+(j+1)%4];		//第三行右移3位 j-3+4=j+2
		}
	}
 
}

unsigned char  gfmultby01(unsigned char b)
{
	return b;
}
unsigned char  gfmultby02(unsigned char b)
{
	if (b < 0x80)
		return (unsigned char)(int)(b <<1);
	else
		return (unsigned char)( (int)(b << 1) ^ (int)(0x1b) );
}
 
unsigned char  gfmultby03(unsigned char b)
{
	return (unsigned char) ( (int)gfmultby02(b) ^ (int)b );
}
 
unsigned char  gfmultby09(unsigned char b)
{
	return (unsigned char)( (int)gfmultby02(gfmultby02(gfmultby02(b))) ^ (int)b );
}
 
unsigned char  gfmultby0b(unsigned char b)
{
	return (unsigned char)( (int)gfmultby02(gfmultby02(gfmultby02(b))) ^
		(int)gfmultby02(b) ^ (int)b );
}
 
unsigned char  gfmultby0d(unsigned char b)
{
	return (unsigned char)( (int)gfmultby02(gfmultby02(gfmultby02(b))) ^
		(int)gfmultby02(gfmultby02(b)) ^ (int)(b) );
}
 
unsigned char  gfmultby0e(unsigned char b)
{
	return (unsigned char)( (int)gfmultby02(gfmultby02(gfmultby02(b))) ^
		(int)gfmultby02(gfmultby02(b)) ^(int)gfmultby02(b) );
}

void  MixColumns(void)
{
	unsigned char temp[4*4];
	int i,j;
	for(j=0;j<4;j++)                                    //2 3 1 1  列混淆矩阵  Page107
	{													//1 2 3 1
		for(i=0;i<4;i++)								//1 1 2 3
		{												//3 1 1 2
			temp[4*i+j]=State[i][j];
		}
	}
	for(j=0;j<4;j++)
	{
		State[0][j] = (unsigned char) ( (int)gfmultby02(temp[0+j]) ^ (int)gfmultby03(temp[4*1+j]) ^
			(int)gfmultby01(temp[4*2+j]) ^ (int)gfmultby01(temp[4*3+j]) );
		State[1][j] = (unsigned char) ( (int)gfmultby01(temp[0+j]) ^ (int)gfmultby02(temp[4*1+j]) ^
			(int)gfmultby03(temp[4*2+j]) ^ (int)gfmultby01(temp[4*3+j]) );
		State[2][j] = (unsigned char) ( (int)gfmultby01(temp[0+j]) ^ (int)gfmultby01(temp[4*1+j]) ^
			(int)gfmultby02(temp[4*2+j]) ^ (int)gfmultby03(temp[4*3+j]) );
		State[3][j] = (unsigned char) ( (int)gfmultby03(temp[0+j]) ^ (int)gfmultby01(temp[4*1+j]) ^
			(int)gfmultby01(temp[4*2+j]) ^ (int)gfmultby02(temp[4*3+j]) );
	}
 
}
void  InvMixColumns(void)
{
	unsigned char temp[4*4];
	int i,j;
	for (i = 0; i < 4; i++)  // copy State into temp[]
	{
		for (j = 0; j < 4; j++)                         //0e 0b 0d 09   逆变换矩阵 Page108
		{												//09 0e 0b 0d
			temp[4*i+j] =  State[i][j];					//0d 09 0e 0b
		}												//0b 0d 09 0e
	}
 
	for (j = 0; j < 4; j++)
	{
		State[0][j] = (unsigned char) ( (int)gfmultby0e(temp[j]) ^ (int)gfmultby0b(temp[4+j]) ^
			(int)gfmultby0d(temp[4*2+j]) ^ (int)gfmultby09(temp[4*3+j]) );
		State[1][j] = (unsigned char) ( (int)gfmultby09(temp[j]) ^ (int)gfmultby0e(temp[4+j]) ^
			(int)gfmultby0b(temp[4*2+j]) ^ (int)gfmultby0d(temp[4*3+j]) );
		State[2][j] = (unsigned char) ( (int)gfmultby0d(temp[j]) ^ (int)gfmultby09(temp[4+j]) ^
			(int)gfmultby0e(temp[4*2+j]) ^ (int)gfmultby0b(temp[4*3+j]) );
		State[3][j] = (unsigned char) ( (int)gfmultby0b(temp[j]) ^ (int)gfmultby0d(temp[4+j]) ^
			(int)gfmultby09(temp[4*2+j]) ^ (int)gfmultby0e(temp[4*3+j]) );
	}
}

void  Cipher(unsigned char* input, unsigned char* output)
{
	int i;
	int round ;
	memset(&State[0][0],0,16);
	for(i=0;i<4*Nb;i++)                        //这里是先写列后写行的，即输入是一列一列的进来的
	{
		State[i%4][i/4]=input[i];					//换成先写行后写列也是可以的，只要在输出时也是这样就可以了
	}
	AddRoundKey(0);									//轮密钥加
 
	for (round = 1; round <= (Nr - 1); round++)  // main round loop
	{
		SubBytes();									//字节代换
		ShiftRows();								//行移位
		MixColumns();								//列混淆
		AddRoundKey(round);							//轮密钥加
	}  // main round loop
 
	SubBytes();										//字节代换
	ShiftRows();									//行移位
	AddRoundKey(Nr);								//轮密钥加
 
	// output = state
	for ( i = 0; i < (4 * Nb); i++)
	{
		output[i] =  State[i % 4][ i / 4];
	}
 
}

void  InvCipher(unsigned char* input,unsigned char* output)
{
	int round;
	int i;
	memset(&State[0][0],0,16);
	for ( i = 0; i < (4 * Nb); i++)
	{
		State[i % 4][ i / 4] = input[i];
	}
 
	AddRoundKey(Nr);
 
	for (round = Nr-1; round >= 1; round--)  // main round loop
	{
		InvShiftRows();
		InvSubBytes();
		AddRoundKey(round);
		InvMixColumns();
	}  // end main round loop for InvCipher
 
	InvShiftRows();
	InvSubBytes();
	AddRoundKey(0);
 
	// output = state
	for ( i = 0; i < (4 * Nb); i++)
	{
		output[i] =  State[i % 4][ i / 4];
	}
}
 
VOID InitializePrivateKey(int KeySize,UCHAR *KeyBytes)
{
	Aes_init(KeySize,KeyBytes);
}

DWORD OnAesEncrypt(LPVOID InBuffer,DWORD InLength,LPVOID OutBuffer)
{
    DWORD OutLength=0;
    long j;
    long i;
 
    UCHAR *lpCurInBuff=(UCHAR *)InBuffer;
    UCHAR *lpCurOutBuff=(UCHAR *)OutBuffer;
    long blocknum=InLength/16;
    long leftnum=InLength%16;
 
    UCHAR iv[20] = AESIV;
 
    for( i=0;i<blocknum;i++)
    {
        for(j = 0; j < 16; j++ )
        {
            lpCurOutBuff[j] = (unsigned char)( lpCurInBuff[j] ^ iv[j] );
        }
 
        Cipher(lpCurOutBuff,lpCurOutBuff);
 
        memcpy( iv, lpCurOutBuff, 16 );
 
        lpCurInBuff+=16;
        lpCurOutBuff+=16;
        OutLength+=16;
    }
    if(leftnum)                   //多余出leftnum 字节  则加密时 多出16-leftnum 个字节
    {
        UCHAR inbuff[16];
        memset(inbuff,16-leftnum,16);
        memcpy(inbuff,lpCurInBuff,leftnum);
 
        for(j = 0; j < 16; j++ )
        {
            lpCurOutBuff[j] = (unsigned char)( inbuff[j] ^ iv[j] );
 
        }
 
        Cipher(lpCurOutBuff,lpCurOutBuff);
 
        memcpy( iv, lpCurOutBuff, 16 );
 
        lpCurOutBuff+=16;
        OutLength+=16;
    }
    else
    {
        //新增16个字节，用以确定增加的字节数
        UCHAR extrabuff[16];
        memset(extrabuff,16,16);
        //*((LPDWORD)extrabuff)=16+(16-leftnum)%16;      //多出16+(16-leftnum)%16个字节
 
        for(j = 0; j < 16; j++ )
        {
            lpCurOutBuff[j] = (unsigned char)( extrabuff[j] ^ iv[j] );
 
        }
 
        Cipher(lpCurOutBuff,lpCurOutBuff);
 
        memcpy( iv, lpCurOutBuff, 16 );
 
        OutLength+=16;
    }
    return OutLength;
}

 DWORD OnAesEncrypt_onebuffer(LPVOID InBuffer,DWORD InLength)//InBuffer should InLength+16
{
    DWORD OutLength=0;
    long j;
    long i;
 
    UCHAR *lpCurInBuff=(UCHAR *)InBuffer;
    //UCHAR *lpCurOutBuff=(UCHAR *)OutBuffer;
    long blocknum=InLength/16;
    long leftnum=InLength%16;
 
    UCHAR iv[20] = AESIV;
 
    for( i=0;i<blocknum;i++)
    {
        for(j = 0; j < 16; j++ )
        {
            lpCurInBuff[j] = (unsigned char)( lpCurInBuff[j] ^ iv[j] );
        }
 
        Cipher(lpCurInBuff,lpCurInBuff);
 
        memcpy( iv, lpCurInBuff, 16 );
 
        lpCurInBuff+=16;
        //lpCurOutBuff+=16;
        OutLength+=16;
    }
    if(leftnum)                   //多余出leftnum 字节  则加密时 多出16-leftnum 个字节
    {
        UCHAR inbuff[16];
        memset(inbuff,16-leftnum,16);
        memcpy(inbuff,lpCurInBuff,leftnum);
 
        for(j = 0; j < 16; j++ )
        {
            lpCurInBuff[j] = (unsigned char)( inbuff[j] ^ iv[j] );
 
        }
 
        Cipher(lpCurInBuff,lpCurInBuff);
 
        memcpy( iv, lpCurInBuff, 16 );
 
        lpCurInBuff+=16;
        OutLength+=16;
    }
    else
    {
        //新增16个字节，用以确定增加的字节数
        UCHAR extrabuff[16];
        memset(extrabuff,16,16);
        //*((LPDWORD)extrabuff)=16+(16-leftnum)%16;      //多出16+(16-leftnum)%16个字节
 
        for(j = 0; j < 16; j++ )
        {
            lpCurInBuff[j] = (unsigned char)( extrabuff[j] ^ iv[j] );
 
        }
 
        Cipher(lpCurInBuff,lpCurInBuff);
 
        memcpy( iv, lpCurInBuff, 16 );
 
        OutLength+=16;
    }
    return OutLength;
}
 
DWORD OnAesUncrypt(LPVOID InBuffer,DWORD InLength,LPVOID OutBuffer)
{
    DWORD OutLength=0;
    long blocknum=InLength/16;
    long leftnum=InLength%16;
    long j;
    long i;
    unsigned char temp[16];
    UCHAR iv[20] = AESIV;
    UCHAR *lpCurInBuff=(UCHAR *)InBuffer;
    UCHAR *lpCurOutBuff=(UCHAR *)OutBuffer;
    
    if(leftnum)
    {
        return 0;
    }
 
    for(i=0;i<blocknum;i++)
    {
        InvCipher(lpCurInBuff,lpCurOutBuff);
 
        for(j = 0; j < 16; j++ )
        {
            lpCurOutBuff[j] = (unsigned char)( lpCurOutBuff[j] ^ iv[j] );
        }
        memcpy( iv, lpCurInBuff, 16 );
 
        if(i == (blocknum-1))
        {
            memset(temp,0,16);
            if(lpCurOutBuff[15] != 0x10)
            {
                if(lpCurOutBuff[15] < 0x10)
                {
                    OutLength = InLength - lpCurOutBuff[15];
                    memcpy( temp, lpCurOutBuff, 16-lpCurOutBuff[15] );
                    memcpy( lpCurOutBuff, temp, 16 );
                }
                else
                    break;
            }
            else
            {
                OutLength = InLength - 16;
                memcpy( lpCurOutBuff, temp, 16 );
            }
        }
        lpCurInBuff+=16;
        lpCurOutBuff+=16;
    }
 
    return OutLength;
 
}

DWORD OnAesUncrypt_onebuffer(LPVOID InBuffer,DWORD InLength)
{
    DWORD OutLength=0;
    long blocknum=InLength/16;
    long leftnum=InLength%16;
    long j;
    long i;
    unsigned char temp[16];
    UCHAR iv[20] = AESIV;
    UCHAR *lpCurInBuff=(UCHAR *)InBuffer;
    
    if(leftnum)
    {
        return 0;
    }
 
    for(i=0;i<blocknum;i++)
    {
    	 memcpy( temp, lpCurInBuff, 16 );
        InvCipher(lpCurInBuff,lpCurInBuff);
 
        for(j = 0; j < 16; j++ )
        {
            lpCurInBuff[j] = (unsigned char)( lpCurInBuff[j] ^ iv[j] );
        }
        memcpy( iv, temp, 16 );
 
        if(i == (blocknum-1))
        {
            memset(temp,0,16);
            if(lpCurInBuff[15] != 0x10)
            {
                if(lpCurInBuff[15] < 0x10)
                {
                    OutLength = InLength - lpCurInBuff[15];
                    memcpy( temp, lpCurInBuff, 16-lpCurInBuff[15] );
                    memcpy( lpCurInBuff, temp, 16 );
                }
                else
                    break;
            }
            else
            {
                OutLength = InLength - 16;
                memcpy( lpCurInBuff, temp, 16 );
            }
        }
        lpCurInBuff+=16;
    }
 
    return OutLength;
 
}


int EncFile(const char* file,const char* file2)
{
    unsigned char* buf=0;
    int size=0, size2=0;
    FILE* fp=0;
    fp=fopen(file,"rb");
    if(!fp)return -1;
    fseek(fp,0,2);
    size=(int)ftell(fp);
    buf=(unsigned char*)malloc(size+1+16);    //buffer should +16
    buf[size]=0;

    fseek(fp,0,0);
    fread(buf,size,1,fp);
    fclose(fp);

   printf("111111 size=%d\n", size);
    size2= OnAesEncrypt_onebuffer(buf,size);
   printf("22222   size2=%d\n", size2);
    fp=fopen(file2,"wb+");
    fwrite(buf,size2,1,fp);
    fclose(fp);
    free(buf);
    return 0;

}

int DecFile(const char* file,const char* file2)
{
    unsigned char* buf=0;
    int size=0;
    FILE* fp=0;
    fp=fopen(file,"rb");
    if(!fp)return -1;
    fseek(fp,0,2);
    size=(int)ftell(fp);
    buf=(unsigned char*)malloc(size+1);
    buf[size]=0;

    fseek(fp,0,0);
    fread(buf,size,1,fp);
    fclose(fp);

   printf("DecFile size=%d\n", size);
    size= OnAesUncrypt_onebuffer(buf,size);

   printf("DecFile2222 size=%d\n", size);
    fp=fopen(file2,"wb+");
    fwrite(buf,size,1,fp);
    fclose(fp);
    free(buf);

    return 0;

}

char* DecFile_to_mem(const char* file, int *len)
{
    char* buf=0;
    int size=0;
    FILE* fp=0;
    fp=fopen(file,"rb");
    if(!fp)return NULL;
    fseek(fp,0,2);
    size=(int)ftell(fp);
    buf=(char*)malloc(size+1);
    buf[size]=0;

    fseek(fp,0,0);
    fread(buf,size,1,fp);
    fclose(fp);

   printf("DecFile size=%d\n", size);
    size= OnAesUncrypt_onebuffer(buf,size);
   printf("DecFile2222 size=%d\n", size);
    *len =size;
	
    return buf;

}

char* strstr_tn(char* s1, char* s2, int length)
{
	int l2;

	l2 = strlen(s2);
	if (!l2)
		return (char *)s1;
	while (length >= l2) {
			length--;
		if (!memcmp(s1, s2, l2))
			return (char *)s1;
		s1++;
	}
	return NULL;
}

int  DLL_VERSION=2;

int parse_misc2_header(char* dec_buf, char * carrierid)
{
	int length,i;
	char * p, *q;
	char region_misc2[2048]={0}; 
	char date_expiry_misc2[64]={0}; 
	char version_misc2[64]={0}; 
	char buf1[64]={0},buf2[64]={0},buf3[64]={0};
	int   ret,year,month,day,version,slot;

	time_t t;
	struct tm * lt;
	
	
	if(dec_buf == NULL){
		printf("test000\n");
		return -1;
	}
	printf("%s\n", dec_buf);
	
	p = strstr(dec_buf, "package_tn_config_misc2_version=002");
	q = strstr_tn(dec_buf, "package_tn_config_misc2_end", 4096);
	if(p == NULL || q == NULL)
	{
		return -2;
	}

	p = strstr(dec_buf, "DateOfExpiry=");
	if(p !=NULL)
	{
		i =p-dec_buf;
		length =0;
		while((dec_buf[i+length] != '\n')&&(dec_buf[i+length] != ' ')&&(dec_buf[i+length] != '\0')){
			date_expiry_misc2[length]= dec_buf[i+length];
			if(++length>64)
				break;
	    }
	}

	p = strstr(dec_buf, "config_version=");
	if(p !=NULL)
	{
		i =p-dec_buf;
		length =0;
		while((dec_buf[i+length] != '\n')&&(dec_buf[i+length] != ' ')&&(dec_buf[i+length] != '\0')){
			version_misc2[length]= dec_buf[i+length];
			if(++length>64)
				break;
	    }
	}

	p = strstr(dec_buf, carrierid);
	if(p !=NULL)
	{
		i =p-dec_buf;
		length =0;
		while((dec_buf[i+length] != '\n')&&(dec_buf[i+length] != ' ')&&(dec_buf[i+length] != '\0')&&(dec_buf[i+length] != '/')){
			region_misc2[length]= dec_buf[i+length];
			if(++length>2048)
				break;
	    }
	}

	printf("test2 %s %s  %s\n",date_expiry_misc2, version_misc2, region_misc2);	
  
	ret =sscanf(date_expiry_misc2,"DateOfExpiry=%[0-9]Y%[0-9]M%[0-9]D",buf1,buf2,buf3); 
	if(ret < 3)
		return -5;
	sscanf(buf1, "%d",&year); 
	sscanf(buf2, "%d",&month);  
	sscanf(buf3, "%d",&day);  
	printf("read expiry: %d %d %d ;%s %s %s\n", year, month,day,buf1,buf2,buf3);

	ret =sscanf(version_misc2,"config_version=%[0-9]",buf1);
	if(ret < 1)
		return -5;
	sscanf(buf1, "%d",&version);
	printf("read version: %d ;%s\n" , version,buf1);

	ret = sscanf(region_misc2, "%[^:]:%[^:]", buf1,buf2);  
	sscanf(buf2, "%d",&slot);
	printf("read slot:%d ;%s\n", slot,buf2);



	if(version < DLL_VERSION)
		return -3;

	time (&t);
	lt = localtime (&t);
	if((lt->tm_year+1900) > year)
		return -4;
	else if((lt->tm_year+1900) == year){
		if((lt->tm_mon +1) > month)
			return -4;
		else if ((lt->tm_mon +1) == month){
			if((lt->tm_mday) > day)
				return -4;
		}
	}

	if((lt->tm_year+1900) < 2019)   // DLL v1  compile  with 2019.5.6
		return -4;
	else if((lt->tm_year+1900) == 2019){
		if((lt->tm_mon +1) < 5)
			return -4;
		else if ((lt->tm_mon +1) == 5){
			if((lt->tm_mday) < 6)
				return -4;
		}
	}

  	return slot;
}


int get_time()
{
	time_t t;
	struct tm * lt;
	time (&t);
	lt = localtime (&t);
	printf ( "%d/%d/%d %d:%d:%d\n",lt->tm_year+1900, lt->tm_mon, lt->tm_mday, lt->tm_hour, lt->tm_min, lt->tm_sec);
	return 0;
}

int get_carrierid(char * carrierid)
{
	FILE* fp=0; 
	int size=0;
	char  carrier_id[64]={0};
	
	fp=fopen("carrierid.ini","rb");   //if carrierid is NULL, for default file carrierid.ini
		if(!fp)return -11;
	fseek(fp,0,2);
	size=(int)ftell(fp);
	if(size > 64)
		return -12;

	fseek(fp,0,0);
	fread(carrier_id,size,1,fp);
	fclose(fp);
	carrier_id[size]=0;
	printf("carrier id %s %d\n",carrier_id, size);
	memcpy(carrierid, carrier_id , size+1);
	return size;
			
}

int checkhash(char*  dec_buf, int size)
{
  	char  hashbuffer[256]={0};   
	char  hashbuffer2[256]={0};   

	StrSHA256(dec_buf,  size -64, hashbuffer2);   //sha256 hash length is 64
	strcpy(&hashbuffer2[64], "saltasjhq8678");
	StrSHA256(&hashbuffer2[0],  64+strlen("saltasjhq8678"), hashbuffer);
	memset(hashbuffer2, 0x0, sizeof(hashbuffer2));
	memcpy(hashbuffer2, &dec_buf[size-64], 64);

	if(0 != strcmp(hashbuffer2, hashbuffer))
	{
		printf("hash miss match!!!!!%s,   %s!!!!!!!\n",hashbuffer, hashbuffer2);
		return -12;
	}
	printf("hash check successful   %s!!!\n", hashbuffer);

	return 0;
}

int parse_misc_add_sig(char* misc_data, char* chipid, int size)
{
	char * p, *q;
  	char  hashbuffer[256]={0};   
	char  hashbuffer2[256]={0};   

	memset(hashbuffer, 0x0, sizeof(hashbuffer));
	memset(hashbuffer2, 0x0, sizeof(hashbuffer2));
	q = strstr_tn(misc_data, "tn_config_end_hash=", size);
	if( q == NULL)
	{
		printf("format error, misc2 data\n");
		return -2;
	}

   	strcpy(hashbuffer, q);
	memcpy(&hashbuffer2[0], &hashbuffer[19], 64);
	
	strcpy(&hashbuffer2[64], chipid);
	//memcpy(&hashbuffer2[64], chipid, 32);
	
	strcpy(&hashbuffer2[96], "jhq867");
	printf("test  sig log, %s\n",hashbuffer2);
	memset(hashbuffer, 0x0, sizeof(hashbuffer));
	sprintf(hashbuffer, "tn_config_sig=");
	StrSHA256(&hashbuffer2[0],  64+32+strlen("jhq867"), &hashbuffer[14]);
	memcpy(q+64+19+1, hashbuffer, strlen(hashbuffer));

	return 0;
}


int islower(int c)
{
	return ((c >= 'a') && (c <= 'z'));
}

int isupper(int c)
{
	return ((c >= 'A') && (c <= 'Z'));
}

int isdigit(int c)
{
	return ((c >= '0') && (c <= '9'));
}

int isalpha(int c)
{
	return isupper(c) || islower(c);
}

//extern "C" _declspec(dllexport)
 int genmisc2(char* miscpath,  char* carrierid , char* chipid, char *data, int *len, int mode = 0)
{
	char* dec_buf=NULL;
	int slot=0;
	char  carrier_id[64]={0};
	int size=0;
	char  misc_data[4096]={0};
	int tmp = 0;
	
	if(mode ==1601)
	{
		for (tmp = 0; tmp < 34; tmp++) {
			if ( (chipid[tmp] == 0 || chipid[tmp] == 0x20) && tmp > 0) {
				break;
			} else if ( !isalpha(chipid[tmp]) && !isdigit(chipid[tmp]))
				return -13;
		}
		printf("chipid  length %d\n", tmp);
	
		if(miscpath == NULL)
			dec_buf =DecFile_to_mem("misc.bin", &size);			//if miscpath is NULL, for default file misc.bin
		else
			dec_buf =DecFile_to_mem(miscpath, &size);

		if(dec_buf ==NULL)
			return -10;

		if(0 !=checkhash(dec_buf, size))
			return -12;
		
		if(carrierid == NULL){
			if(get_carrierid(carrier_id) > 0)
				slot =parse_misc2_header(dec_buf, carrier_id);
		}else
			slot =parse_misc2_header(dec_buf, carrierid);
		
		if(slot > 0)
		{
			memcpy(misc_data, dec_buf+ 4096*slot ,4096);
			parse_misc_add_sig(misc_data, chipid, 4096);
			memcpy(data, misc_data,4096);
			*len = 4096;
		}
		else{
			*len = 0;
			return slot;
		}
		
	}
	else if(mode == 1){
		if(miscpath == NULL)
			return -11;
		dec_buf =DecFile_to_mem(miscpath, &size);
		if(dec_buf ==NULL)
			return -10;

		memcpy(data, dec_buf ,size);
		*len = size;
	}
	else{
		*len = DLL_VERSION;
	}

	if(dec_buf)
		free(dec_buf);

	return 0;
}


int main(int argc, char* argv[], char* envp[])
{
	char * path = "misc2.txt";
	char * path3 = "misc3.txt";
	char * path_sign = "misc2.img";
       char * enable_decrypt  = "false";

      for(int i = 1; i < argc; i++)
      {
         if( !strcmp(argv[i], "-d") || !strcmp(argv[i], "-D") )
         {
             i ++;
             enable_decrypt = argv[i];
         }
 	else if( !strcmp(argv[i], "-p") || !strcmp(argv[i], "-P") )
         {
             i ++;
             path = argv[i];
         }
	else if( !strcmp(argv[i], "-q") || !strcmp(argv[i], "-Q") )
         {
             i ++;
             path_sign = argv[i];
         }
	else if( !strcmp(argv[i], "-g") || !strcmp(argv[i], "-G") )
         {
             i ++;
             path3 = argv[i];
         }
	
       else if( !strcmp(argv[i], "-h") || !strcmp(argv[i], "-H") )
         {
             printf("sign_misc.exe can encrypt and decrypt from misc2.txt<=> misc2.img\n");
             printf("sign_misc.exe [-d]  [-p] [-q] [-n]\n");
             printf( "   [-d true]   with  decrypt misc2.img to misc2.txt, if not set, encrypt misc2.txt to misc2.img , set as true or flase\n");
 	      printf( "   [-p]     set the file name for misc2.txt, if not set ,default as misc2.txt\n");
	      printf( "   [-q]     set the file name for misc2.img, if not set ,default as misc2.img\n");
	      printf( "   [-n true]     normal encrypt, default is false:as 4k align\n");
             
             return 0;
         }
      }

	get_time();
      if((0==strcmp(enable_decrypt,"true")) || ((0==strcmp(enable_decrypt,"decrypt"))))
      {
      		char  data[4096]={0};   //test
		int len; 
		FILE* fp=0; 

		char * chipid= "ac8fe4b95687d44c1269f1becba35876";
		printf("decrypt %s to %s start!\n", path_sign, path3);
		DecFile(path_sign, path3);	
		genmisc2(NULL, NULL, chipid, data , &len,1601);

		
		fp=fopen("misc2_result.img","wb+");  //for test result
		fwrite(data,4096,1,fp);
		fclose(fp);
      }      
       else if(0==strcmp(enable_decrypt,"package4k"))
      {
      		char  data[4096]={0};   
		int size; 
		FILE* fp=0; 

		fp=fopen(path,"rb");  
		if(!fp)return -1;
		fseek(fp,0,2);
		size=(int)ftell(fp);

		fseek(fp,0,0);
		fread(data,size,1,fp);
		fclose(fp);
		
		fp=fopen(path_sign,"wb+");  
		fwrite(data,4096,1,fp);
		fclose(fp);
      }
	else if(0==strcmp(enable_decrypt,"addhash"))
      {
      		char  hashbuffer[256]={0};   
		char  hashbuffer2[256]={0};   
		int size; 
		FILE* fp=0; 

		FileSHA256(path, hashbuffer2);
		strcpy(&hashbuffer2[64], "saltasjhq8678");
		StrSHA256(&hashbuffer2[0],  64+strlen("saltasjhq8678"), hashbuffer);
		
		fp=fopen(path,"ab+");
		if(!fp)return -1;
			fseek(fp,0,2);
		size=(int)ftell(fp);
		fwrite(hashbuffer,64,1,fp);
		fclose(fp);
      }
	
      else{
	 	printf("encrypt  %s to %s start!\n",path, path_sign);
		EncFile(path, path_sign);
     	}
	
	return 0;
}

