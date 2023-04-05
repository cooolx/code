/* Copyright (c) 2010-2016, The Linux Foundation. All rights reserved.

 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided
 *     with the distribution.
 *   * Neither the name of The Linux Foundation nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef _TINNO_BOARD_INFO_H_
#define _TINNO_BOARD_INFO_H_

#define BUFF_MAX 64

#define TINNO_INIT_OK                                           0x0
#define TINNO_INIT_PARTITION_ERROR                  0x01
#define TINNO_INIT_MD5_CAL_ERROR                     0x02
#define TINNO_INIT_MD5_MISMATCH                      0x03
#define TINNO_INIT_LEN_OVERFLOW                     0x04
#define TINNO_INIT_LOCK_MISMATCH                   0x05
#define TINNO_INIT_LOCK_DEBUGVERSION           0x06
#define TINNO_INIT_MD5_SIG_MISMATCH                      0x07
#define TINNO_INIT_MALLOC_FAIL                      0x08

#define TINNO_INIT_PARTITION_NO_SIGNINFO               0x20
#define TINNO_INIT_PARTITION_SIGNINFO_ERROR                  0x22
#define TINNO_INIT_SIGNINFO_HASH_MISMATCH                      0x23
#define TINNO_INIT_LEN_SIGNINFO_OVERFLOW                     0x24
#define TINNO_INIT_SIGNINFO_SIG_MISMATCH                   0x25

#define STR_MISC2_GUID         "misc2_guid"
#define STR_MISC2_CARRIER      "carrier"
#define STR_MISC2_SIMNUMBER    "sim_number"
#define STR_MISC2_MISC2VER     "misc_version"
#define STR_MISC2_HWSKU        "hardware.sku"
#define STR_MISC2_SIMLOCK      "sim_lock"
#define STR_MISC2_BLUR         "Blur_String"
#define STR_MISC2_SVNKIT       "SVNKit"
#define STR_MISC2_ESIMSTAT     "esim_state"

enum {
    E_MISC2_GUID = 0,
    E_CARRIER,
    E_SIM_NUMBER,
    E_MISC_VERSION,
    E_HARDWARE_SKU,
    E_SIM_LOCK,
    E_BLUR_STRING,
    E_SVNKIT,
    E_ESIMSTAT,
    E_PROP_COUNT
};

typedef struct {
    char info[E_PROP_COUNT][BUFF_MAX];
} Misc2Info;

typedef struct {
	UINT32 boardid_idx;
	UINT32 skuid_idx;
} SkuBoardIDSmemType;

extern Misc2Info misc2Info;

int Misc2InfoInit(int key);

void Misc2AddCmdLine(char *buf, int length);
void Misc2SetFdt(void * fdt);

#endif

