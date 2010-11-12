/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
   PDF417 is ISO/IEC 15438:2006

   Good short PDF417 description:
     http://grandzebu.net/index.php?page=/informatique/codbar-en/pdf417.htm

   Free PDF417 specification (in Russian):
     http://protect.gost.ru/v.aspx?control=8&baseC=-1&page=0&month=-1&year=-1&search=&RegNum=1&DocOnPageCount=15&id=122345&pageK=D11DCCF9-B540-4CB2-A514-C3547043FC68

   Online generator:
     http://www.bcmaker.com/demos/pdf417.php
     http://www.terryburton.co.uk/barcodewriter/generator/

   Online decoder:
     http://www.datasymbol.com/barcode-recognition-sdk/barcode-reader/online-barcode-decoder.html
*/

#include "hbzebra.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#define CODE_START       0x2AFF   /* 17 bit width */
#define CODE_STOP       0x2517F   /* 18 bit width */

#define LATCH_TEXT          900
#define LATCH_BYTE          901
#define LATCH_BYTE_FULL     924
#define LATCH_NUMERIC       902
#define SHIFT_BYTE          913

#define SUBMODE_UPPER         1
#define SUBMODE_LOWER         2
#define SUBMODE_MIXED         3
#define SUBMODE_PUNCT         4

#define MAX_COL_COUNT        30
#define MIN_ROW_COUNT         3
#define MAX_ROW_COUNT        90
#define MAX_CODEWORD_COUNT  928


static short s_code[ 3 ][ 929 ] = {
 {
   0x03AB, 0x0F57, 0x3EAF, 0x072B, 0x1E57, 0x7CAF, 0x0315, 0x0E2B,
   0x0615, 0x020A, 0x0C15, 0x040A, 0x03B5, 0x0F6B, 0x3ED7, 0x0735,
   0x1E6B, 0x7CD7, 0x031A, 0x0E35, 0x061A, 0x03BA, 0x0F75, 0x3EEB,
   0x073A, 0x1E75, 0x7CEB, 0x0E3A, 0x3C75, 0x0F7A, 0x3EF5, 0x1E7A,
   0x7CF5, 0x3EFA, 0x5FAF, 0x074B, 0x1E97, 0x7D2F, 0x0325, 0x0E4B,
   0x3C97, 0x0625, 0x1C4B, 0x0212, 0x0C25, 0x384B, 0x0412, 0x1825,
   0x0812, 0x0765, 0x1ECB, 0x7D97, 0x0332, 0x0E65, 0x3CCB, 0x0632,
   0x1C65, 0x78CB, 0x0C32, 0x3865, 0x0772, 0x1EE5, 0x7DCB, 0x0E72,
   0x3CE5, 0x1C72, 0x78E5, 0x1EF2, 0x7DE5, 0x3CF2, 0x78F2, 0x0345,
   0x0E8B, 0x3D17, 0x0645, 0x1C8B, 0x7917, 0x0222, 0x0C45, 0x388B,
   0x0422, 0x1845, 0x0822, 0x1022, 0x0362, 0x0EC5, 0x3D8B, 0x0662,
   0x1CC5, 0x798B, 0x0C62, 0x38C5, 0x1862, 0x3062, 0x0EE2, 0x3DC5,
   0x1CE2, 0x79C5, 0x38E2, 0x3DE2, 0x0685, 0x1D0B, 0x7A17, 0x0242,
   0x0C85, 0x390B, 0x0442, 0x1885, 0x710B, 0x0842, 0x3085, 0x1042,
   0x6085, 0x06C2, 0x1D85, 0x7B0B, 0x0CC2, 0x3985, 0x18C2, 0x7185,
   0x30C2, 0x60C2, 0x7B85, 0x71C2, 0x0282, 0x0D05, 0x3A0B, 0x0482,
   0x1905, 0x720B, 0x0882, 0x3105, 0x1082, 0x6105, 0x2082, 0x0D82,
   0x1982, 0x3182, 0x0502, 0x740B, 0x3205, 0x6205, 0x4102, 0x0753,
   0x1EA7, 0x7D4F, 0x0329, 0x0E53, 0x3CA7, 0x0629, 0x1C53, 0x78A7,
   0x0214, 0x0C29, 0x0414, 0x0769, 0x1ED3, 0x7DA7, 0x0334, 0x0E69,
   0x3CD3, 0x0634, 0x1C69, 0x0C34, 0x1834, 0x0774, 0x1EE9, 0x7DD3,
   0x0E74, 0x3CE9, 0x1C74, 0x3874, 0x1EF4, 0x7DE9, 0x3CF4, 0x7DF4,
   0x035B, 0x0EB7, 0x3D6F, 0x065B, 0x1CB7, 0x796F, 0x022D, 0x0C5B,
   0x38B7, 0x042D, 0x185B, 0x70B7, 0x082D, 0x305B, 0x0349, 0x0E93,
   0x3D27, 0x036D, 0x0649, 0x1C93, 0x7927, 0x066D, 0x1CDB, 0x79B7,
   0x0236, 0x0424, 0x1849, 0x7093, 0x0436, 0x186D, 0x0836, 0x0364,
   0x0EC9, 0x3D93, 0x0376, 0x0664, 0x1CC9, 0x7993, 0x0676, 0x1CED,
   0x79DB, 0x0C76, 0x1864, 0x1876, 0x0EE4, 0x3DC9, 0x0EF6, 0x1CE4,
   0x79C9, 0x1CF6, 0x79ED, 0x38F6, 0x3DE4, 0x3DF6, 0x79E4, 0x79F6,
   0x069B, 0x1D37, 0x7A6F, 0x024D, 0x0C9B, 0x3937, 0x044D, 0x189B,
   0x7137, 0x084D, 0x309B, 0x104D, 0x204D, 0x0689, 0x1D13, 0x7A27,
   0x06CD, 0x0C89, 0x3913, 0x0266, 0x0444, 0x399B, 0x7113, 0x0466,
   0x0844, 0x3089, 0x0866, 0x30CD, 0x6089, 0x2044, 0x06C4, 0x1D89,
   0x7B13, 0x06E6, 0x0CC4, 0x3989, 0x0CE6, 0x39CD, 0x7189, 0x18E6,
   0x30C4, 0x60C4, 0x1DC4, 0x7B89, 0x1DE6, 0x39C4, 0x39E6, 0x71C4,
   0x71E6, 0x7BE6, 0x028D, 0x0D1B, 0x3A37, 0x048D, 0x191B, 0x7237,
   0x088D, 0x311B, 0x108D, 0x611B, 0x208D, 0x408D, 0x0284, 0x0D09,
   0x3A13, 0x02C6, 0x0484, 0x1909, 0x7213, 0x04C6, 0x198D, 0x731B,
   0x08C6, 0x1084, 0x6109, 0x10C6, 0x618D, 0x20C6, 0x0D84, 0x3B09,
   0x0DC6, 0x1984, 0x7309, 0x19C6, 0x738D, 0x31C6, 0x6184, 0x61C6,
   0x3BC6, 0x73C6, 0x050D, 0x1A1B, 0x7437, 0x090D, 0x321B, 0x110D,
   0x621B, 0x210D, 0x410D, 0x0504, 0x1A09, 0x7413, 0x0586, 0x0904,
   0x3209, 0x0986, 0x330D, 0x6209, 0x1186, 0x2104, 0x2186, 0x4104,
   0x1B04, 0x1B86, 0x3386, 0x6386, 0x341B, 0x641B, 0x420D, 0x3409,
   0x1204, 0x1306, 0x2306, 0x4306, 0x0351, 0x0EA3, 0x3D47, 0x0651,
   0x1CA3, 0x0228, 0x0C51, 0x38A3, 0x0428, 0x1851, 0x0828, 0x1028,
   0x0368, 0x0ED1, 0x3DA3, 0x0668, 0x1CD1, 0x79A3, 0x0C68, 0x38D1,
   0x1868, 0x3068, 0x0EE8, 0x3DD1, 0x1CE8, 0x79D1, 0x38E8, 0x3DE8,
   0x79E8, 0x06B3, 0x1D67, 0x7ACF, 0x0259, 0x0CB3, 0x3967, 0x0459,
   0x18B3, 0x7167, 0x0859, 0x30B3, 0x1059, 0x60B3, 0x0691, 0x1D23,
   0x7A47, 0x06D9, 0x0C91, 0x3923, 0x026C, 0x0448, 0x39B3, 0x7123,
   0x046C, 0x18D9, 0x3091, 0x086C, 0x1048, 0x106C, 0x06C8, 0x1D91,
   0x7B23, 0x06EC, 0x0CC8, 0x7BB3, 0x0CEC, 0x39D9, 0x7191, 0x18EC,
   0x30C8, 0x30EC, 0x1DC8, 0x7B91, 0x1DEC, 0x39C8, 0x39EC, 0x71C8,
   0x7BC8, 0x7BEC, 0x02BB, 0x0D77, 0x3AEF, 0x04BB, 0x1977, 0x72EF,
   0x08BB, 0x3177, 0x10BB, 0x6177, 0x20BB, 0x0299, 0x0D33, 0x3A67,
   0x02DD, 0x0499, 0x3B77, 0x7267, 0x04DD, 0x19BB, 0x7377, 0x08DD,
   0x1099, 0x6133, 0x10DD, 0x61BB, 0x4099, 0x0288, 0x0D11, 0x3A23,
   0x02CC, 0x0488, 0x1911, 0x7223, 0x02EE, 0x04CC, 0x1999, 0x7333,
   0x04EE, 0x19DD, 0x73BB, 0x6111, 0x08EE, 0x10CC, 0x6199, 0x10EE,
   0x4088, 0x0D88, 0x3B11, 0x0DCC, 0x1988, 0x7311, 0x0DEE, 0x19CC,
   0x7399, 0x19EE, 0x73DD, 0x6188, 0x61CC, 0x3B88, 0x3BCC, 0x7388,
   0x3BEE, 0x73CC, 0x053B, 0x1A77, 0x74EF, 0x093B, 0x3277, 0x113B,
   0x6277, 0x213B, 0x413B, 0x0519, 0x1A33, 0x7467, 0x059D, 0x0919,
   0x7677, 0x099D, 0x333B, 0x6233, 0x119D, 0x2119, 0x219D, 0x4119,
   0x419D, 0x0508, 0x1A11, 0x7423, 0x058C, 0x0908, 0x3211, 0x05CE,
   0x098C, 0x3319, 0x6211, 0x09CE, 0x339D, 0x2108, 0x11CE, 0x218C,
   0x4108, 0x418C, 0x1B08, 0x7611, 0x1B8C, 0x3308, 0x1BCE, 0x338C,
   0x6308, 0x33CE, 0x638C, 0x7708, 0x77CE, 0x0A3B, 0x3477, 0x123B,
   0x6477, 0x223B, 0x423B, 0x0A19, 0x3433, 0x0B1D, 0x1219, 0x6433,
   0x131D, 0x663B, 0x231D, 0x4219, 0x431D, 0x0A08, 0x3411, 0x0B0C,
   0x1208, 0x6411, 0x0B8E, 0x130C, 0x6619, 0x138E, 0x671D, 0x4208,
   0x238E, 0x430C, 0x438E, 0x370C, 0x378E, 0x678E, 0x6877, 0x443B,
   0x6833, 0x2419, 0x4419, 0x1408, 0x160C, 0x170E, 0x4408, 0x460C,
   0x06A1, 0x0250, 0x0CA1, 0x0450, 0x18A1, 0x7143, 0x0850, 0x30A1,
   0x1050, 0x60A1, 0x06D0, 0x1DA1, 0x7B43, 0x0CD0, 0x39A1, 0x18D0,
   0x71A1, 0x30D0, 0x60D0, 0x1DD0, 0x7BA1, 0x39D0, 0x71D0, 0x7BD0,
   0x02B1, 0x0D63, 0x3AC7, 0x04B1, 0x1963, 0x08B1, 0x3163, 0x10B1,
   0x6163, 0x20B1, 0x0290, 0x0D21, 0x3A43, 0x02D8, 0x0490, 0x3B63,
   0x7243, 0x04D8, 0x19B1, 0x7363, 0x08D8, 0x1090, 0x6121, 0x10D8,
   0x61B1, 0x4090, 0x0D90, 0x3B21, 0x0DD8, 0x1990, 0x7321, 0x19D8,
   0x73B1, 0x31D8, 0x6190, 0x3B90, 0x3BD8, 0x7390, 0x73D8, 0x0573,
   0x1AE7, 0x75CF, 0x0973, 0x32E7, 0x1173, 0x62E7, 0x2173, 0x4173,
   0x0531, 0x1A63, 0x05B9, 0x0931, 0x3263, 0x09B9, 0x3373, 0x6263,
   0x11B9, 0x2131, 0x21B9, 0x4131, 0x41B9, 0x0510, 0x1A21, 0x0598,
   0x0910, 0x7663, 0x05DC, 0x0998, 0x3331, 0x6221, 0x09DC, 0x33B9,
   0x2110, 0x11DC, 0x2198, 0x4110, 0x4198, 0x1B10, 0x7621, 0x1B98,
   0x3310, 0x1BDC, 0x3398, 0x6310, 0x33DC, 0x6398, 0x7710, 0x7798,
   0x77DC, 0x0AF7, 0x35EF, 0x12F7, 0x65EF, 0x22F7, 0x42F7, 0x0A73,
   0x34E7, 0x0B7B, 0x36F7, 0x64E7, 0x137B, 0x66F7, 0x237B, 0x4273,
   0x437B, 0x0A31, 0x3463, 0x0B39, 0x1231, 0x6463, 0x0BBD, 0x1339,
   0x6673, 0x13BD, 0x677B, 0x4231, 0x23BD, 0x4339, 0x43BD, 0x0A10,
   0x3421, 0x0B18, 0x1210, 0x6421, 0x0B9C, 0x1318, 0x6631, 0x0BDE,
   0x139C, 0x6739, 0x4210, 0x13DE, 0x67BD, 0x4318, 0x23DE, 0x3610,
   0x3718, 0x6610, 0x379C, 0x6718, 0x37DE, 0x679C, 0x67DE, 0x14F7,
   0x69EF, 0x24F7, 0x44F7, 0x1473, 0x68E7, 0x167B, 0x6CF7, 0x267B,
   0x4473, 0x467B, 0x1431, 0x6863, 0x1639, 0x2431, 0x173D, 0x2639,
   0x4431, 0x273D, 0x4639, 0x473D, 0x1410, 0x6821, 0x1618, 0x6C31,
   0x171C, 0x2618, 0x4410, 0x179E, 0x271C, 0x4618, 0x279E, 0x471C,
   0x479E, 0x6E18, 0x6F9E, 0x48F7, 0x2C7B, 0x4C7B, 0x2C39, 0x2E3D,
   0x4E3D, 0x2C18, 0x2E1C, 0x2F1E, 0x4F1E, 0x02A0, 0x04A0, 0x1941,
   0x08A0, 0x10A0, 0x20A0, 0x0DA0, 0x19A0, 0x31A0, 0x61A0, 0x3BA0,
   0x73A0, 0x0561, 0x0961, 0x32C3, 0x1161, 0x62C3, 0x2161, 0x4161,
   0x0520, 0x1A41, 0x05B0, 0x1B61, 0x3241, 0x09B0, 0x3361, 0x11B0,
   0x6361, 0x21B0, 0x4120, 0x41B0, 0x1B20, 0x7641, 0x1BB0, 0x7761,
   0x33B0, 0x6320, 0x63B0, 0x7720, 0x77B0, 0x0AE3, 0x12E3, 0x22E3,
   0x42E3, 0x0A61, 0x0B71, 0x36E3, 0x64C3, 0x1371, 0x66E3, 0x2371,
   0x4261, 0x4371, 0x0A20, 0x0B30, 0x1220, 0x6441, 0x0BB8, 0x1330,
   0x2220, 0x13B8, 0x2330, 0x4220, 0x23B8, 0x4330, 0x3620, 0x3730,
   0x6620, 0x37B8, 0x6730, 0x67B8, 0x15E7, 0x25E7, 0x45E7, 0x14E3,
   0x16F3, 0x6DE7, 0x26F3, 0x44E3, 0x46F3, 0x1461, 0x68C3, 0x1671,
   0x6CE3, 0x1779, 0x2671, 0x4461, 0x2779, 0x4671, 0x4779, 0x1420,
   0x6841, 0x1630, 0x6C61, 0x1738, 0x2630, 0x4420, 0x17BC, 0x2738,
   0x4630, 0x27BC, 0x4738, 0x6C20, 0x6E30, 0x6F38, 0x6FBC, 0x2BEF,
   0x4BEF, 0x29E7, 0x2DF7, 0x49E7, 0x4DF7, 0x28E3, 0x2CF3, 0x48E3,
   0x2EFB, 0x4CF3, 0x4EFB, 0x2861, 0x2C71, 0x4861, 0x2E79, 0x4C71,
   0x2F7D },
 {
   0x06AF, 0x1D5F, 0x0257, 0x0CAF, 0x395F, 0x0457, 0x18AF, 0x715F,
   0x0857, 0x30AF, 0x1057, 0x60AF, 0x2057, 0x06D7, 0x1DAF, 0x7B5F,
   0x026B, 0x0CD7, 0x39AF, 0x046B, 0x18D7, 0x71AF, 0x086B, 0x30D7,
   0x106B, 0x60D7, 0x206B, 0x06EB, 0x1DD7, 0x7BAF, 0x0275, 0x0CEB,
   0x39D7, 0x0475, 0x18EB, 0x71D7, 0x0875, 0x30EB, 0x1075, 0x60EB,
   0x2075, 0x06F5, 0x1DEB, 0x7BD7, 0x027A, 0x0CF5, 0x39EB, 0x047A,
   0x18F5, 0x71EB, 0x087A, 0x30F5, 0x107A, 0x60F5, 0x06FA, 0x1DF5,
   0x7BEB, 0x0CFA, 0x39F5, 0x18FA, 0x71F5, 0x30FA, 0x1DFA, 0x7BF5,
   0x39FA, 0x71FA, 0x0297, 0x0D2F, 0x3A5F, 0x0497, 0x192F, 0x725F,
   0x0897, 0x312F, 0x1097, 0x612F, 0x2097, 0x4097, 0x02CB, 0x0D97,
   0x3B2F, 0x04CB, 0x1997, 0x732F, 0x08CB, 0x3197, 0x10CB, 0x6197,
   0x20CB, 0x40CB, 0x02E5, 0x0DCB, 0x3B97, 0x04E5, 0x19CB, 0x7397,
   0x08E5, 0x31CB, 0x10E5, 0x61CB, 0x20E5, 0x40E5, 0x02F2, 0x0DE5,
   0x3BCB, 0x04F2, 0x19E5, 0x73CB, 0x08F2, 0x31E5, 0x10F2, 0x61E5,
   0x20F2, 0x0DF2, 0x3BE5, 0x19F2, 0x73E5, 0x31F2, 0x61F2, 0x3BF2,
   0x73F2, 0x0517, 0x1A2F, 0x745F, 0x0917, 0x322F, 0x1117, 0x622F,
   0x2117, 0x4117, 0x058B, 0x1B17, 0x762F, 0x098B, 0x3317, 0x118B,
   0x6317, 0x218B, 0x418B, 0x05C5, 0x1B8B, 0x7717, 0x09C5, 0x338B,
   0x11C5, 0x638B, 0x21C5, 0x41C5, 0x05E2, 0x1BC5, 0x778B, 0x09E2,
   0x33C5, 0x11E2, 0x63C5, 0x21E2, 0x41E2, 0x1BE2, 0x77C5, 0x33E2,
   0x63E2, 0x77E2, 0x0A17, 0x342F, 0x1217, 0x642F, 0x2217, 0x4217,
   0x0B0B, 0x3617, 0x130B, 0x6617, 0x230B, 0x430B, 0x0B85, 0x370B,
   0x1385, 0x670B, 0x2385, 0x4385, 0x0BC2, 0x3785, 0x13C2, 0x6785,
   0x23C2, 0x43C2, 0x37C2, 0x67C2, 0x1417, 0x682F, 0x2417, 0x4417,
   0x160B, 0x6C17, 0x260B, 0x460B, 0x1705, 0x6E0B, 0x2705, 0x4705,
   0x1782, 0x6F05, 0x2782, 0x4782, 0x2817, 0x4817, 0x2C0B, 0x4C0B,
   0x2E05, 0x4E05, 0x02A7, 0x0D4F, 0x3A9F, 0x04A7, 0x194F, 0x729F,
   0x08A7, 0x314F, 0x10A7, 0x614F, 0x20A7, 0x40A7, 0x02D3, 0x0DA7,
   0x3B4F, 0x04D3, 0x19A7, 0x734F, 0x08D3, 0x31A7, 0x10D3, 0x61A7,
   0x20D3, 0x40D3, 0x02E9, 0x0DD3, 0x3BA7, 0x04E9, 0x19D3, 0x73A7,
   0x08E9, 0x31D3, 0x10E9, 0x61D3, 0x20E9, 0x40E9, 0x02F4, 0x0DE9,
   0x3BD3, 0x04F4, 0x19E9, 0x73D3, 0x08F4, 0x31E9, 0x10F4, 0x61E9,
   0x20F4, 0x0DF4, 0x3BE9, 0x19F4, 0x73E9, 0x31F4, 0x61F4, 0x3BF4,
   0x73F4, 0x056F, 0x1ADF, 0x0FD6, 0x096F, 0x32DF, 0x1F96, 0x116F,
   0x62DF, 0x3F16, 0x216F, 0x416F, 0x0527, 0x1A4F, 0x749F, 0x05B7,
   0x0927, 0x76DF, 0x09B7, 0x336F, 0x624F, 0x11B7, 0x2127, 0x21B7,
   0x4127, 0x41B7, 0x0593, 0x1B27, 0x764F, 0x05DB, 0x0993, 0x3327,
   0x09DB, 0x33B7, 0x6327, 0x11DB, 0x2193, 0x21DB, 0x4193, 0x41DB,
   0x05C9, 0x1B93, 0x7727, 0x05ED, 0x09C9, 0x3393, 0x09ED, 0x33DB,
   0x6393, 0x11ED, 0x21C9, 0x21ED, 0x41C9, 0x41ED, 0x05E4, 0x1BC9,
   0x7793, 0x05F6, 0x09E4, 0x33C9, 0x09F6, 0x33ED, 0x63C9, 0x11F6,
   0x21E4, 0x21F6, 0x41E4, 0x1BE4, 0x77C9, 0x1BF6, 0x33E4, 0x33F6,
   0x63E4, 0x63F6, 0x77E4, 0x0A6F, 0x34DF, 0x1FA6, 0x126F, 0x64DF,
   0x3F26, 0x226F, 0x7E26, 0x426F, 0x0A27, 0x344F, 0x0B37, 0x1227,
   0x644F, 0x1337, 0x666F, 0x2337, 0x4227, 0x4337, 0x0B13, 0x3627,
   0x0B9B, 0x1313, 0x6627, 0x139B, 0x6737, 0x239B, 0x4313, 0x439B,
   0x0B89, 0x3713, 0x0BCD, 0x1389, 0x6713, 0x13CD, 0x679B, 0x23CD,
   0x4389, 0x43CD, 0x0BC4, 0x3789, 0x0BE6, 0x13C4, 0x6789, 0x13E6,
   0x67CD, 0x23E6, 0x43C4, 0x43E6, 0x37C4, 0x37E6, 0x67C4, 0x67E6,
   0x146F, 0x68DF, 0x3F46, 0x246F, 0x7E46, 0x446F, 0x1427, 0x684F,
   0x1637, 0x6C6F, 0x2637, 0x4427, 0x4637, 0x1613, 0x6C27, 0x171B,
   0x2613, 0x271B, 0x4613, 0x471B, 0x1709, 0x6E13, 0x178D, 0x6F1B,
   0x278D, 0x4709, 0x478D, 0x1784, 0x6F09, 0x17C6, 0x2784, 0x27C6,
   0x4784, 0x47C6, 0x6F84, 0x6FC6, 0x286F, 0x7E86, 0x486F, 0x2827,
   0x2C37, 0x4827, 0x4C37, 0x2C13, 0x2E1B, 0x4C13, 0x4E1B, 0x2E09,
   0x2F0D, 0x4E09, 0x4F0D, 0x2F04, 0x2F86, 0x4F04, 0x4F86, 0x506F,
   0x5027, 0x5837, 0x5813, 0x5C1B, 0x5C09, 0x5E0D, 0x0547, 0x1A8F,
   0x751F, 0x0947, 0x328F, 0x1147, 0x628F, 0x2147, 0x4147, 0x05A3,
   0x1B47, 0x768F, 0x09A3, 0x3347, 0x11A3, 0x6347, 0x21A3, 0x41A3,
   0x05D1, 0x1BA3, 0x7747, 0x09D1, 0x33A3, 0x11D1, 0x63A3, 0x21D1,
   0x41D1, 0x05E8, 0x1BD1, 0x77A3, 0x09E8, 0x33D1, 0x11E8, 0x63D1,
   0x21E8, 0x41E8, 0x1BE8, 0x77D1, 0x33E8, 0x63E8, 0x77E8, 0x0ACF,
   0x359F, 0x1FAC, 0x12CF, 0x659F, 0x3F2C, 0x22CF, 0x7E2C, 0x42CF,
   0x0A47, 0x348F, 0x0B67, 0x1247, 0x648F, 0x1367, 0x66CF, 0x2367,
   0x4247, 0x4367, 0x0B23, 0x3647, 0x0BB3, 0x1323, 0x6647, 0x13B3,
   0x6767, 0x23B3, 0x4323, 0x43B3, 0x0B91, 0x3723, 0x0BD9, 0x1391,
   0x6723, 0x13D9, 0x67B3, 0x23D9, 0x4391, 0x43D9, 0x0BC8, 0x3791,
   0x0BEC, 0x13C8, 0x6791, 0x13EC, 0x67D9, 0x23EC, 0x43C8, 0x43EC,
   0x37C8, 0x37EC, 0x67C8, 0x67EC, 0x15DF, 0x0FAE, 0x3F5D, 0x25DF,
   0x1F2E, 0x7E5D, 0x45DF, 0x3E2E, 0x7C2E, 0x14CF, 0x699F, 0x3F4C,
   0x16EF, 0x6DDF, 0x3F6E, 0x7E4C, 0x26EF, 0x44CF, 0x7E6E, 0x46EF,
   0x1447, 0x688F, 0x1667, 0x2447, 0x1777, 0x6EEF, 0x4447, 0x2777,
   0x4667, 0x4777, 0x1623, 0x6C47, 0x1733, 0x2623, 0x17BB, 0x2733,
   0x4623, 0x27BB, 0x4733, 0x47BB, 0x1711, 0x6E23, 0x1799, 0x2711,
   0x17DD, 0x2799, 0x4711, 0x27DD, 0x4799, 0x47DD, 0x1788, 0x6F11,
   0x17CC, 0x2788, 0x17EE, 0x27CC, 0x4788, 0x27EE, 0x47CC, 0x47EE,
   0x6F88, 0x6FCC, 0x29DF, 0x1F4E, 0x7E9D, 0x49DF, 0x3E4E, 0x7C4E,
   0x28CF, 0x7E8C, 0x2CEF, 0x48CF, 0x7ECE, 0x4CEF, 0x2847, 0x2C67,
   0x4847, 0x2E77, 0x4C67, 0x4E77, 0x2C23, 0x2E33, 0x4C23, 0x2F3B,
   0x4E33, 0x4F3B, 0x2E11, 0x2F19, 0x4E11, 0x2F9D, 0x4F19, 0x4F9D,
   0x2F08, 0x2F8C, 0x4F08, 0x2FCE, 0x4F8C, 0x4FCE, 0x51DF, 0x3E8E,
   0x7C8E, 0x50CF, 0x58EF, 0x5047, 0x5867, 0x5C77, 0x5823, 0x5C33,
   0x5E3B, 0x5C11, 0x5E19, 0x5F1D, 0x5E08, 0x5F0C, 0x5F8E, 0x7D0E,
   0x0A87, 0x350F, 0x1287, 0x650F, 0x2287, 0x4287, 0x0B43, 0x3687,
   0x1343, 0x6687, 0x2343, 0x4343, 0x0BA1, 0x3743, 0x13A1, 0x6743,
   0x23A1, 0x43A1, 0x0BD0, 0x37A1, 0x13D0, 0x67A1, 0x23D0, 0x43D0,
   0x37D0, 0x67D0, 0x158F, 0x6B1F, 0x3F58, 0x258F, 0x7E58, 0x458F,
   0x1487, 0x690F, 0x16C7, 0x2487, 0x26C7, 0x4487, 0x46C7, 0x1643,
   0x6C87, 0x1763, 0x2643, 0x2763, 0x4643, 0x4763, 0x1721, 0x6E43,
   0x17B1, 0x2721, 0x27B1, 0x4721, 0x47B1, 0x1790, 0x6F21, 0x17D8,
   0x2790, 0x27D8, 0x4790, 0x47D8, 0x6F90, 0x6FD8, 0x2B9F, 0x1F5C,
   0x7EB9, 0x4B9F, 0x3E5C, 0x7C5C, 0x298F, 0x7E98, 0x2DCF, 0x498F,
   0x7EDC, 0x4DCF, 0x2887, 0x2CC7, 0x4887, 0x2EE7, 0x4CC7, 0x4EE7,
   0x2C43, 0x2E63, 0x4C43, 0x2F73, 0x4E63, 0x4F73, 0x2E21, 0x2F31,
   0x4E21, 0x2FB9, 0x4F31, 0x4FB9, 0x2F10, 0x2F98, 0x4F10, 0x2FDC,
   0x4F98, 0x4FDC, 0x0F5E, 0x3EBD, 0x1E5E, 0x7CBD, 0x3C5E, 0x785E,
   0x539F, 0x3E9C, 0x5BDF, 0x3EDE, 0x7C9C, 0x7CDE, 0x518F, 0x59CF,
   0x5DEF, 0x5087, 0x58C7, 0x5CE7, 0x5EF7, 0x5843, 0x5C63, 0x5E73,
   0x5F7B, 0x5C21, 0x5E31, 0x5F39, 0x5FBD, 0x5E10, 0x5F18, 0x5F9C,
   0x1E9E, 0x7D3D, 0x3C9E, 0x789E, 0x7D1C, 0x7D9E, 0x3D1E, 0x791E,
   0x7A1E, 0x1507, 0x2507, 0x4507, 0x1683, 0x6D07, 0x2683, 0x4683,
   0x1741, 0x6E83, 0x2741, 0x4741, 0x17A0, 0x6F41, 0x27A0, 0x47A0,
   0x6FA0, 0x2B0F, 0x7EB0, 0x4B0F, 0x2907, 0x2D87, 0x4907, 0x4D87,
   0x2C83, 0x2EC3, 0x4C83, 0x4EC3, 0x2E41, 0x2F61, 0x4E41, 0x4F61,
   0x2F20, 0x2FB0, 0x4F20, 0x4FB0, 0x571F, 0x3EB8, 0x7CB8, 0x530F,
   0x5B8F, 0x5107, 0x5987, 0x5DC7, 0x5883, 0x5CC3, 0x5EE3, 0x5C41,
   0x5E61, 0x5F71, 0x5E20, 0x5F30, 0x5FB8, 0x1EBC, 0x7D79, 0x3CBC,
   0x78BC, 0x7D38, 0x7DBC, 0x0EBE, 0x3D7D, 0x1CBE, 0x797D, 0x38BE,
   0x70BE, 0x3D3C, 0x3DBE, 0x793C, 0x79BE, 0x1D3E, 0x7A7D, 0x393E,
   0x713E, 0x7A3C, 0x7B3E, 0x3A3E, 0x723E, 0x743E, 0x2D03, 0x4D03,
   0x2E81, 0x4E81, 0x2F40, 0x4F40, 0x5B07, 0x5903, 0x5D83, 0x5C81,
   0x5EC1, 0x5E40, 0x5F60, 0x7D70, 0x3D78, 0x7978, 0x1D7C, 0x7AF9,
   0x397C, 0x717C, 0x7A78, 0x7B7C, 0x0D7E, 0x3AFD, 0x197E, 0x72FD,
   0x317E, 0x617E, 0x3A7C, 0x3B7E, 0x727C, 0x737E, 0x1A7E, 0x74FD,
   0x327E, 0x627E, 0x747C, 0x767E, 0x347E, 0x647E, 0x7AF0, 0x3AF8,
   0x72F8, 0x1AFC, 0x75F9, 0x32FC, 0x62FC, 0x74F8, 0x76FC, 0x34FC,
   0x64FC },
 {
   0x07D5, 0x1FAB, 0x03CA, 0x0F95, 0x3F2B, 0x078A, 0x1F15, 0x7E2B,
   0x0F0A, 0x3E15, 0x1E0A, 0x0B5F, 0x07DA, 0x1FB5, 0x135F, 0x0F9A,
   0x3F35, 0x235F, 0x1F1A, 0x7E35, 0x435F, 0x3E1A, 0x0BAF, 0x375F,
   0x1FBA, 0x13AF, 0x675F, 0x3F3A, 0x23AF, 0x7E3A, 0x43AF, 0x0BD7,
   0x37AF, 0x13D7, 0x67AF, 0x23D7, 0x43D7, 0x0BEB, 0x37D7, 0x13EB,
   0x67D7, 0x23EB, 0x43EB, 0x0BF5, 0x37EB, 0x13F5, 0x67EB, 0x23F5,
   0x03D2, 0x0FA5, 0x3F4B, 0x0792, 0x1F25, 0x7E4B, 0x0F12, 0x3E25,
   0x1E12, 0x7C25, 0x3C12, 0x165F, 0x0FB2, 0x3F65, 0x265F, 0x1F32,
   0x7E65, 0x465F, 0x3E32, 0x7C32, 0x172F, 0x6E5F, 0x3F72, 0x272F,
   0x7E72, 0x472F, 0x1797, 0x6F2F, 0x2797, 0x4797, 0x17CB, 0x6F97,
   0x27CB, 0x47CB, 0x17E5, 0x6FCB, 0x27E5, 0x47E5, 0x07A2, 0x1F45,
   0x7E8B, 0x0F22, 0x3E45, 0x1E22, 0x7C45, 0x3C22, 0x7822, 0x2C5F,
   0x1F62, 0x7EC5, 0x4C5F, 0x3E62, 0x7C62, 0x2E2F, 0x7EE2, 0x4E2F,
   0x2F17, 0x4F17, 0x2F8B, 0x4F8B, 0x2FC5, 0x4FC5, 0x0F42, 0x3E85,
   0x1E42, 0x7C85, 0x3C42, 0x7842, 0x585F, 0x3EC2, 0x7CC2, 0x5C2F,
   0x5E17, 0x5F0B, 0x1E82, 0x7D05, 0x3C82, 0x7882, 0x7D82, 0x3D02,
   0x7902, 0x03D4, 0x0FA9, 0x3F53, 0x0794, 0x1F29, 0x7E53, 0x0F14,
   0x3E29, 0x1E14, 0x7C29, 0x3C14, 0x169F, 0x0FB4, 0x3F69, 0x269F,
   0x1F34, 0x7E69, 0x469F, 0x3E34, 0x7C34, 0x174F, 0x6E9F, 0x3F74,
   0x274F, 0x7E74, 0x474F, 0x17A7, 0x6F4F, 0x27A7, 0x47A7, 0x17D3,
   0x6FA7, 0x27D3, 0x47D3, 0x17E9, 0x6FD3, 0x27E9, 0x47E9, 0x07AD,
   0x1F5B, 0x7EB7, 0x0396, 0x0F2D, 0x3E5B, 0x0716, 0x1E2D, 0x7C5B,
   0x0E16, 0x3C2D, 0x1C16, 0x782D, 0x3816, 0x07A4, 0x1F49, 0x7E93,
   0x07B6, 0x0F24, 0x3E49, 0x0F36, 0x3E6D, 0x7C49, 0x1E36, 0x3C24,
   0x3C36, 0x7824, 0x7836, 0x2C9F, 0x1F64, 0x7EC9, 0x2EDF, 0x4C9F,
   0x1F76, 0x3E64, 0x4EDF, 0x3E76, 0x7C64, 0x7C76, 0x2E4F, 0x7EE4,
   0x2F6F, 0x4E4F, 0x7EF6, 0x4F6F, 0x2F27, 0x2FB7, 0x4F27, 0x4FB7,
   0x2F93, 0x2FDB, 0x4F93, 0x4FDB, 0x2FC9, 0x4FC9, 0x03A6, 0x0F4D,
   0x3E9B, 0x0726, 0x1E4D, 0x7C9B, 0x0E26, 0x3C4D, 0x1C26, 0x784D,
   0x3826, 0x7026, 0x0F44, 0x3E89, 0x0F66, 0x1E44, 0x7C89, 0x1E66,
   0x7CCD, 0x3C66, 0x7844, 0x7866, 0x589F, 0x3EC4, 0x5CDF, 0x3EE6,
   0x7CC4, 0x7CE6, 0x5C4F, 0x5E6F, 0x5E27, 0x5F37, 0x5F13, 0x5F9B,
   0x5F89, 0x0746, 0x1E8D, 0x7D1B, 0x0E46, 0x3C8D, 0x1C46, 0x788D,
   0x3846, 0x7046, 0x1E84, 0x7D09, 0x1EC6, 0x3C84, 0x3CC6, 0x7884,
   0x78C6, 0x7D84, 0x7DC6, 0x0E86, 0x3D0D, 0x1C86, 0x790D, 0x3886,
   0x7086, 0x3D04, 0x3D86, 0x7904, 0x7986, 0x1D06, 0x7A0D, 0x3906,
   0x7106, 0x7A04, 0x7B06, 0x3A06, 0x7206, 0x07A8, 0x1F51, 0x7EA3,
   0x0F28, 0x3E51, 0x1E28, 0x7C51, 0x3C28, 0x7828, 0x2D1F, 0x1F68,
   0x7ED1, 0x4D1F, 0x3E68, 0x7C68, 0x2E8F, 0x7EE8, 0x4E8F, 0x2F47,
   0x4F47, 0x2FA3, 0x4FA3, 0x2FD1, 0x4FD1, 0x03AC, 0x0F59, 0x3EB3,
   0x072C, 0x1E59, 0x7CB3, 0x0E2C, 0x3C59, 0x1C2C, 0x7859, 0x382C,
   0x702C, 0x0F48, 0x3E91, 0x0F6C, 0x1E48, 0x7C91, 0x1E6C, 0x7CD9,
   0x3C6C, 0x7848, 0x786C, 0x591F, 0x3EC8, 0x5D9F, 0x3EEC, 0x7CC8,
   0x7CEC, 0x5C8F, 0x5ECF, 0x5E47, 0x5F67, 0x5F23, 0x5FB3, 0x5F91,
   0x075D, 0x1EBB, 0x7D77, 0x032E, 0x0E5D, 0x3CBB, 0x062E, 0x1C5D,
   0x78BB, 0x0C2E, 0x385D, 0x182E, 0x705D, 0x302E, 0x074C, 0x1E99,
   0x7D33, 0x076E, 0x0E4C, 0x3C99, 0x0E6E, 0x3CDD, 0x7899, 0x1C6E,
   0x384C, 0x386E, 0x704C, 0x706E, 0x1E88, 0x7D11, 0x1ECC, 0x3C88,
   0x1EEE, 0x3CCC, 0x7888, 0x3CEE, 0x78CC, 0x78EE, 0x7D88, 0x7DCC,
   0x7DEE, 0x034E, 0x0E9D, 0x3D3B, 0x064E, 0x1C9D, 0x793B, 0x0C4E,
   0x389D, 0x184E, 0x709D, 0x304E, 0x604E, 0x0E8C, 0x3D19, 0x0ECE,
   0x1C8C, 0x7919, 0x1CCE, 0x799D, 0x38CE, 0x708C, 0x70CE, 0x3D08,
   0x3D8C, 0x7908, 0x3DCE, 0x798C, 0x79CE, 0x068E, 0x1D1D, 0x7A3B,
   0x0C8E, 0x391D, 0x188E, 0x711D, 0x308E, 0x608E, 0x1D0C, 0x7A19,
   0x1D8E, 0x390C, 0x398E, 0x710C, 0x718E, 0x7A08, 0x7B0C, 0x7B8E,
   0x0D0E, 0x3A1D, 0x190E, 0x721D, 0x310E, 0x610E, 0x3A0C, 0x3B0E,
   0x720C, 0x730E, 0x1A0E, 0x741D, 0x320E, 0x620E, 0x740C, 0x760E,
   0x340E, 0x640E, 0x0F50, 0x3EA1, 0x1E50, 0x7CA1, 0x3C50, 0x7850,
   0x3ED0, 0x7CD0, 0x5D0F, 0x5E87, 0x5F43, 0x5FA1, 0x0758, 0x1EB1,
   0x7D63, 0x0E58, 0x3CB1, 0x1C58, 0x78B1, 0x3858, 0x7058, 0x1E90,
   0x7D21, 0x1ED8, 0x3C90, 0x3CD8, 0x7890, 0x78D8, 0x7D90, 0x7DD8,
   0x035C, 0x0EB9, 0x3D73, 0x065C, 0x1CB9, 0x7973, 0x0C5C, 0x38B9,
   0x185C, 0x70B9, 0x305C, 0x605C, 0x0E98, 0x3D31, 0x0EDC, 0x1C98,
   0x7931, 0x1CDC, 0x3898, 0x38DC, 0x7098, 0x70DC, 0x3D10, 0x3D98,
   0x7910, 0x3DDC, 0x7998, 0x79DC, 0x06BD, 0x1D7B, 0x7AF7, 0x025E,
   0x0CBD, 0x397B, 0x045E, 0x18BD, 0x717B, 0x085E, 0x30BD, 0x105E,
   0x60BD, 0x205E, 0x069C, 0x1D39, 0x7A73, 0x06DE, 0x0C9C, 0x3939,
   0x0CDE, 0x39BD, 0x7139, 0x18DE, 0x309C, 0x30DE, 0x609C, 0x60DE,
   0x1D18, 0x7A31, 0x1D9C, 0x3918, 0x1DDE, 0x399C, 0x7118, 0x39DE,
   0x719C, 0x71DE, 0x7A10, 0x7B18, 0x7B9C, 0x7BDE, 0x029E, 0x0D3D,
   0x3A7B, 0x049E, 0x193D, 0x727B, 0x089E, 0x313D, 0x109E, 0x613D,
   0x209E, 0x409E, 0x0D1C, 0x3A39, 0x0D9E, 0x191C, 0x7239, 0x199E,
   0x733D, 0x319E, 0x611C, 0x619E, 0x3A18, 0x3B1C, 0x7218, 0x3B9E,
   0x731C, 0x739E, 0x051E, 0x1A3D, 0x747B, 0x091E, 0x323D, 0x111E,
   0x623D, 0x211E, 0x411E, 0x1A1C, 0x7439, 0x1B1E, 0x321C, 0x331E,
   0x621C, 0x631E, 0x7418, 0x761C, 0x771E, 0x0A1E, 0x343D, 0x121E,
   0x643D, 0x221E, 0x421E, 0x341C, 0x361E, 0x641C, 0x661E, 0x141E,
   0x683D, 0x241E, 0x441E, 0x681C, 0x6C1E, 0x1EA0, 0x7D41, 0x3CA0,
   0x78A0, 0x7DA0, 0x0EB0, 0x3D61, 0x1CB0, 0x7961, 0x38B0, 0x70B0,
   0x3D20, 0x3DB0, 0x7920, 0x79B0, 0x06B8, 0x1D71, 0x7AE3, 0x0CB8,
   0x3971, 0x18B8, 0x7171, 0x30B8, 0x60B8, 0x1D30, 0x7A61, 0x1DB8,
   0x3930, 0x39B8, 0x7130, 0x71B8, 0x7A20, 0x7B30, 0x7BB8, 0x02BC,
   0x0D79, 0x3AF3, 0x04BC, 0x1979, 0x72F3, 0x08BC, 0x3179, 0x10BC,
   0x6179, 0x20BC, 0x40BC, 0x0D38, 0x3A71, 0x0DBC, 0x1938, 0x7271,
   0x19BC, 0x7379, 0x31BC, 0x6138, 0x61BC, 0x3A30, 0x3B38, 0x7230,
   0x3BBC, 0x7338, 0x73BC, 0x057D, 0x1AFB, 0x75F7, 0x097D, 0x32FB,
   0x117D, 0x62FB, 0x217D, 0x417D, 0x053C, 0x1A79, 0x74F3, 0x05BE,
   0x093C, 0x3279, 0x09BE, 0x337D, 0x6279, 0x11BE, 0x213C, 0x21BE,
   0x413C, 0x41BE, 0x1A38, 0x7471, 0x1B3C, 0x3238, 0x1BBE, 0x333C,
   0x6238, 0x33BE, 0x633C, 0x63BE, 0x7430, 0x7638, 0x773C, 0x77BE,
   0x0A7D, 0x34FB, 0x127D, 0x64FB, 0x227D, 0x427D, 0x0A3C, 0x3479,
   0x0B3E, 0x123C, 0x6479, 0x133E, 0x667D, 0x233E, 0x423C, 0x433E,
   0x3438, 0x363C, 0x6438, 0x373E, 0x663C, 0x673E, 0x147D, 0x68FB,
   0x247D, 0x447D, 0x143C, 0x6879, 0x163E, 0x243C, 0x263E, 0x443C,
   0x463E, 0x6838, 0x6C3C, 0x6E3E, 0x287D, 0x487D, 0x283C, 0x2C3E,
   0x483C, 0x4C3E, 0x3D40, 0x7940, 0x1D60, 0x7AC1, 0x3960, 0x7160,
   0x7A40, 0x7B60, 0x0D70, 0x3AE1, 0x1970, 0x72E1, 0x3170, 0x6170,
   0x3A60, 0x3B70, 0x7260, 0x7370, 0x0578, 0x1AF1, 0x75E3, 0x0978,
   0x32F1, 0x1178, 0x62F1, 0x2178, 0x4178, 0x1A70, 0x74E1, 0x1B78,
   0x76F1, 0x3378, 0x6270, 0x6378, 0x7460, 0x7670, 0x7778, 0x0AF9,
   0x35F3, 0x12F9, 0x65F3, 0x22F9, 0x42F9, 0x0A78, 0x34F1, 0x0B7C,
   0x36F9, 0x64F1, 0x137C, 0x2278, 0x237C, 0x4278, 0x437C, 0x3470,
   0x3678, 0x6470, 0x377C, 0x6678, 0x677C, 0x15FB, 0x6BF7, 0x25FB,
   0x45FB, 0x14F9, 0x69F3, 0x16FD, 0x24F9, 0x26FD, 0x44F9, 0x46FD,
   0x1478, 0x68F1, 0x167C, 0x2478, 0x177E, 0x267C, 0x4478, 0x277E,
   0x467C, 0x477E, 0x6870, 0x6C78, 0x6E7C, 0x6F7E, 0x29FB, 0x49FB,
   0x28F9, 0x2CFD, 0x48F9, 0x4CFD, 0x2878, 0x2C7C, 0x4878, 0x2E7E,
   0x4C7C, 0x4E7E, 0x51FB, 0x50F9, 0x58FD, 0x5078, 0x587C, 0x5C7E,
   0x3AC0, 0x72C0, 0x1AE0, 0x75C1, 0x32E0, 0x62E0, 0x74C0, 0x76E0,
   0x0AF0, 0x35E1, 0x12F0, 0x65E1, 0x22F0, 0x42F0, 0x34E0, 0x36F0,
   0x64E0, 0x66F0, 0x15F1, 0x6BE3, 0x25F1, 0x45F1, 0x14F0, 0x69E1,
   0x16F8, 0x6DF1, 0x26F8, 0x44F0, 0x46F8, 0x68E0, 0x6CF0, 0x6EF8,
   0x2BF3, 0x4BF3, 0x29F1, 0x2DF9, 0x49F1, 0x4DF9, 0x28F0, 0x2CF8,
   0x48F0, 0x2EFC, 0x4CF8, 0x4EFC, 0x53F3, 0x51F1, 0x59F9, 0x50F0,
   0x58F8, 0x5CFC, 0x35C0, 0x65C0, 0x15E0, 0x6BC1, 0x25E0, 0x45E0,
   0x69C0, 0x6DE0, 0x2BE1, 0x4BE1, 0x29E0, 0x2DF0, 0x49E0, 0x4DF0,
   0x57E3 }};

static unsigned short s_rs0[] = {
   27, 917 };

static unsigned short s_rs1[] = {
   522, 568, 723, 809 };

static unsigned short s_rs2[] = {
   237, 308, 436, 284, 646, 653, 428, 379 };

static unsigned short s_rs3[] = {
   274, 562, 232, 755, 599, 524, 801, 132, 295, 116, 442, 428, 295,  42, 176,  65 };

static unsigned short s_rs4[] = {
   361, 575, 922, 525, 176, 586, 640, 321, 536, 742, 677, 742, 687, 284, 193, 517,
   273, 494, 263, 147, 593, 800, 571, 320, 803, 133, 231, 390, 685, 330,  63, 410 };

static unsigned short s_rs5[] = {
   539, 422,   6,  93, 862, 771, 453, 106, 610, 287, 107, 505, 733, 877, 381, 612,
   723, 476, 462, 172, 430, 609, 858, 822, 543, 376, 511, 400, 672, 762, 283, 184,
   440,  35, 519,  31, 460, 594, 225, 535, 517, 352, 605, 158, 651, 201, 488, 502,
   648, 733, 717,  83, 404,  97, 280, 771, 840, 629,   4, 381, 843, 623, 264, 543 };

static unsigned short s_rs6[] = {
   521, 310, 864, 547, 858, 580, 296, 379,  53, 779, 897, 444, 400, 925, 749, 415,
   822,  93, 217, 208, 928, 244, 583, 620, 246, 148, 447, 631, 292, 908, 490, 704,
   516, 258, 457, 907, 594, 723, 674, 292, 272,  96, 684, 432, 686, 606, 860, 569,
   193, 219, 129, 186, 236, 287, 192, 775, 278, 173,  40, 379, 712, 463, 646, 776,
   171, 491, 297, 763, 156, 732,  95, 270, 447,  90, 507,  48, 228, 821, 808, 898,
   784, 663, 627, 378, 382, 262, 380, 602, 754, 336,  89, 614,  87, 432, 670, 616,
   157, 374, 242, 726, 600, 269, 375, 898, 845, 454, 354, 130, 814, 587, 804,  34,
   211, 330, 539, 297, 827, 865,  37, 517, 834, 315, 550,  86, 801,   4, 108, 539 };

static unsigned short s_rs7[] = {
   524, 894,  75, 766, 882, 857,  74, 204,  82, 586, 708, 250, 905, 786, 138, 720,
   858, 194, 311, 913, 275, 190, 375, 850, 438, 733, 194, 280, 201, 280, 828, 757,
   710, 814, 919,  89,  68, 569,  11, 204, 796, 605, 540, 913, 801, 700, 799, 137,
   439, 418, 592, 668, 353, 859, 370, 694, 325, 240, 216, 257, 284, 549, 209, 884,
   315,  70, 329, 793, 490, 274, 877, 162, 749, 812, 684, 461, 334, 376, 849, 521,
   307, 291, 803, 712,  19, 358, 399, 908, 103, 511,  51,   8, 517, 225, 289, 470,
   637, 731,  66, 255, 917, 269, 463, 830, 730, 433, 848, 585, 136, 538, 906,  90,
     2, 290, 743, 199, 655, 903, 329,  49, 802, 580, 355, 588, 188, 462,  10, 134,
   628, 320, 479, 130, 739,  71, 263, 318, 374, 601, 192, 605, 142, 673, 687, 234,
   722, 384, 177, 752, 607, 640, 455, 193, 689, 707, 805, 641,  48,  60, 732, 621,
   895, 544, 261, 852, 655, 309, 697, 755, 756,  60, 231, 773, 434, 421, 726, 528,
   503, 118,  49, 795,  32, 144, 500, 238, 836, 394, 280, 566, 319,   9, 647, 550,
    73, 914, 342, 126,  32, 681, 331, 792, 620,  60, 609, 441, 180, 791, 893, 754,
   605, 383, 228, 749, 760, 213,  54, 297, 134,  54, 834, 299, 922, 191, 910, 532,
   609, 829, 189,  20, 167,  29, 872, 449,  83, 402,  41, 656, 505, 579, 481, 173,
   404, 251, 688,  95, 497, 555, 642, 543, 307, 159, 924, 558, 648,  55, 497,  10 };

static unsigned short s_rs8[] = {
   352,  77, 373, 504,  35, 599, 428, 207, 409, 574, 118, 498, 285, 380, 350, 492,
   197, 265, 920, 155, 914, 299, 229, 643, 294, 871, 306,  88,  87, 193, 352, 781,
   846,  75, 327, 520, 435, 543, 203, 666, 249, 346, 781, 621, 640, 268, 794, 534,
   539, 781, 408, 390, 644, 102, 476, 499, 290, 632, 545,  37, 858, 916, 552,  41,
   542, 289, 122, 272, 383, 800, 485,  98, 752, 472, 761, 107, 784, 860, 658, 741,
   290, 204, 681, 407, 855,  85,  99,  62, 482, 180,  20, 297, 451, 593, 913, 142,
   808, 684, 287, 536, 561,  76, 653, 899, 729, 567, 744, 390, 513, 192, 516, 258,
   240, 518, 794, 395, 768, 848,  51, 610, 384, 168, 190, 826, 328, 596, 786, 303,
   570, 381, 415, 641, 156, 237, 151, 429, 531, 207, 676, 710,  89, 168, 304, 402,
    40, 708, 575, 162, 864, 229,  65, 861, 841, 512, 164, 477, 221,  92, 358, 785,
   288, 357, 850, 836, 827, 736, 707,  94,   8, 494, 114, 521,   2, 499, 851, 543,
   152, 729, 771,  95, 248, 361, 578, 323, 856, 797, 289,  51, 684, 466, 533, 820,
   669,  45, 902, 452, 167, 342, 244, 173,  35, 463, 651,  51, 699, 591, 452, 578,
    37, 124, 298, 332, 552,  43, 427, 119, 662, 777, 475, 850, 764, 364, 578, 911,
   283, 711, 472, 420, 245, 288, 594, 394, 511, 327, 589, 777, 699, 688,  43, 408,
   842, 383, 721, 521, 560, 644, 714, 559,  62, 145, 873, 663, 713, 159, 672, 729,
   624,  59, 193, 417, 158, 209, 563, 564, 343, 693, 109, 608, 563, 365, 181, 772,
   677, 310, 248, 353, 708, 410, 579, 870, 617, 841, 632, 860, 289, 536,  35, 777,
   618, 586, 424, 833,  77, 597, 346, 269, 757, 632, 695, 751, 331, 247, 184,  45,
   787, 680,  18,  66, 407, 369,  54, 492, 228, 613, 830, 922, 437, 519, 644, 905,
   789, 420, 305, 441, 207, 300, 892, 827, 141, 537, 381, 662, 513,  56, 252, 341,
   242, 797, 838, 837, 720, 224, 307, 631,  61,  87, 560, 310, 756, 665, 397, 808,
   851, 309, 473, 795, 378,  31, 647, 915, 459, 806, 590, 731, 425, 216, 548, 249,
   321, 881, 699, 535, 673, 782, 210, 815, 905, 303, 843, 922, 281,  73, 469, 791,
   660, 162, 498, 308, 155, 422, 907, 817, 187,  62,  16, 425, 535, 336, 286, 437,
   375, 273, 610, 296, 183, 923, 116, 667, 751, 353,  62, 366, 691, 379, 687, 842,
    37, 357, 720, 742, 330,   5,  39, 923, 311, 424, 242, 749, 321,  54, 669, 316,
   342, 299, 534, 105, 667, 488, 640, 672, 576, 540, 316, 486, 721, 610,  46, 656,
   447, 171, 616, 464, 190, 531, 297, 321, 762, 752, 533, 175, 134,  14, 381, 433,
   717,  45, 111,  20, 596, 284, 736, 138, 646, 411, 877, 669, 141, 919,  45, 780,
   407, 164, 332, 899, 165, 726, 600, 325, 498, 655, 357, 752, 768, 223, 849, 647,
    63, 310, 863, 251, 366, 304, 282, 738, 675, 410, 389, 244,  31, 121, 303, 263 };


static int _pdf417_ec_size( int iLevel )
{
   return 2 << iLevel;
}

static int _pdf417_default_ec_level( int iDataSize )
{
   /* http://www.idautomation.com/pdf417faq.html */
   if( iDataSize <= 40 )
      return 2;
   if( iDataSize <= 160 )
      return 3;
   if( iDataSize <= 320 )
      return 4;
   return 5;    /* <= 863 */
}

static int _pdf417_width( int iColCount, int iFlags )
{
   return 17 + ( iColCount + 2 ) * 17 + 18 - ( iFlags & HB_ZEBRA_FLAG_PDF417_TRUNCATED ? 34 : 0 );
}

static int _pdf417_left_codeword( int iRow, int iRowCount, int iColCount, int iLevel )
{
   if( iRow % 3 == 0 )
      return ( iRow / 3 ) * 30 + ( iRowCount - 1 ) / 3;
   else if( iRow % 3 == 1 )
      return ( iRow / 3 ) * 30 + iLevel * 3 + ( iRowCount - 1 ) % 3;
   return ( iRow / 3 ) * 30 + iColCount - 1;
}

static int _pdf417_right_codeword( int iRow, int iRowCount, int iColCount, int iLevel )
{
   if( iRow % 3 == 0 )
      return ( iRow / 3 ) * 30 + iColCount - 1;
   else if( iRow % 3 == 1 )
      return ( iRow / 3 ) * 30 + ( iRowCount - 1 ) / 3;
   return ( iRow / 3 ) * 30 + iLevel * 3 + ( iRowCount - 1 ) % 3;
}

static int _pdf417_isdigit( char ch  )
{
   return '0' <= ch && ch <= '9';
}

static int _pdf417_isalpha( char ch  )
{
   return ( ' ' <= ch && ( unsigned char ) ch <= 127 ) || ch == '\t' || ch == '\n' || ch == '\r';
}

static int _pdf417_upperno( char ch  )
{
   if( 'A' <= ch && ch <= 'Z' )
      return ch - 'A';
   else if( ch == ' ' )
      return 26;

   return -1;
}

static int _pdf417_lowerno( char ch  )
{
   if( 'a' <= ch && ch <= 'z' )
      return ch - 'a';
   else if( ch == ' ' )
      return 26;

   return -1;
}

static int _pdf417_mixedno( char ch  )
{
   if( '0' <= ch && ch <= '9' )
      return ch - '0';

   switch( ch )
   {
      case '&':
         return 10;
      case '\r':
         return 11;
      case '\t':
         return 12;
      case ',':
         return 13;
      case ':':
         return 14;
      case '#':
         return 15;
      case '-':
         return 16;
      case '.':
         return 17;
      case '$':
         return 18;
      case '/':
         return 19;
      case '+':
         return 20;
      case '%':
         return 21;
      case '*':
         return 22;
      case '=':
         return 23;
      case '^':
         return 24;
      case ' ':
         return 26;
   }
   return -1;
}

static int _pdf417_punctno( char ch  )
{
   switch( ch )
   {
      case ';':
         return 0;
      case '<':
         return 1;
      case '>':
         return 2;
      case '@':
         return 3;
      case '[':
         return 4;
      case '\\':
         return 5;
      case ']':
         return 6;
      case '_':
         return 7;
      case '`':
         return 8;
      case '~':
         return 9;
      case '!':
         return 10;
      case '\r':
         return 11;
      case '\t':
         return 12;
      case ',':
         return 13;
      case ':':
         return 14;
      case '\n':
         return 15;
      case '-':
         return 16;
      case '.':
         return 17;
      case '$':
         return 18;
      case '/':
         return 19;
      case '"':
         return 20;
      case '|':
         return 21;
      case '*':
         return 22;
      case '(':
         return 23;
      case ')':
         return 24;
      case '?':
         return 25;
      case '{':
         return 26;
      case '}':
         return 27;
      case '\'':
         return 28;
   }
   return -1;
}

static int _pdf417_encode_byte( const char * szCode, int iLen, int * pCW, int iPos )
{
   HB_LONGLONG  ill;
   int  i;

   HB_TRACE( HB_TR_DEBUG, ("encode byte len=%d", iLen));
   if( iLen == 0 )
      return iPos;

   if( iPos + 2 > MAX_CODEWORD_COUNT )
      return -1;

   if( iLen % 6 == 0 )
      pCW[ iPos++ ] = LATCH_BYTE_FULL;
   else
      pCW[ iPos++ ] = LATCH_BYTE;

   i = 0;
   while( i < iLen )
   {
      if( iLen - i >= 6 )
      {
         ill =  ( unsigned char ) szCode[ i++ ];
         ill <<= 8;
         ill += ( unsigned char ) szCode[ i++ ];
         ill <<= 8;
         ill += ( unsigned char ) szCode[ i++ ];
         ill <<= 8;
         ill += ( unsigned char ) szCode[ i++ ];
         ill <<= 8;
         ill += ( unsigned char ) szCode[ i++ ];
         ill <<= 8;
         ill += ( unsigned char ) szCode[ i++ ];

         if( iPos + 5 > MAX_CODEWORD_COUNT )
            return -1;

         pCW[ iPos + 4 ] = ill % 900;
         ill /= 900;
         pCW[ iPos + 3 ] = ill % 900;
         ill /= 900;
         pCW[ iPos + 2 ] = ill % 900;
         ill /= 900;
         pCW[ iPos + 1 ] = ill % 900;
         pCW[ iPos     ] = ( int ) ( ill / 900 );
         iPos += 5;
      }
      else
      {
         if( iPos + iLen - i > MAX_CODEWORD_COUNT )
            return -1;

         for( ; i < iLen; i++ )
            pCW[ iPos++ ] = szCode[ i ];
      }
   }
   return iPos;
}

static int _pdf417_encode_text_add( int * pCW, int iPos, int * i1, int * i2, int no )
{
   * i1 = * i2;
   * i2 = no;
   HB_TRACE( HB_TR_DEBUG, ("text halfcodeword %d", no));
   if( * i1 != -1 )
   {
      if( iPos >= MAX_CODEWORD_COUNT )
         return -1;
      pCW[ iPos++ ] = * i1 * 30 + * i2;
      * i1 = -1;
      * i2 = -1;
   }
   return iPos;
}

static int _pdf417_encode_text( const char * szCode, int iLen, int * pCW, int iPos )
{
   int i, j, i1, i2, no, iSubMode;

   HB_TRACE( HB_TR_DEBUG, ("encode text len=%d", iLen));
   iSubMode = SUBMODE_UPPER;
   i1 = i2 = -1;
   for( i = 0; i < iLen; i++ )
   {
      HB_TRACE( HB_TR_DEBUG, ("submode=%d char=%c", iSubMode, szCode[ i ] ));
      if( iSubMode == SUBMODE_UPPER )
      {
         if( ( no = _pdf417_upperno( szCode[ i ] ) ) != -1 )
         {
         }
         else if( ( no = _pdf417_lowerno( szCode[ i ] ) ) != -1 )
         {
            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 27 /* UPPER->LOWER */ );
            if( iPos == -1 )
               return -1;

            iSubMode = SUBMODE_LOWER;
         }
         else if( ( no = _pdf417_mixedno( szCode[ i ] ) ) != -1 )
         {
            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 28 /* UPPER->MIXED */ );
            if( iPos == -1 )
               return -1;

            iSubMode = SUBMODE_MIXED;
         }
         else /* if( ( no = _pdf417_punctno( szCode[ i ] ) ) != -1 ) */
         {
            no = _pdf417_punctno( szCode[ i ] );
            for( j = i + 1; j < iLen &&
                            _pdf417_punctno( szCode[ j ] ) != -1 &&
                            _pdf417_mixedno( szCode[ j ] ) == -1; j++ );
            if( j - i >= 5 )
            {
               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 28 /* UPPER->MIXED */ );
               if( iPos == -1 )
                  return -1;

               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 25 /* MIXED->PUNCT */ );
               if( iPos == -1 )
                  return -1;

               iSubMode = SUBMODE_PUNCT;
            }
            else
            {
               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 29 /* UPPER->1PUNCT */ );
               if( iPos == -1 )
                  return -1;
            }
         }
      }
      else if( iSubMode == SUBMODE_LOWER )
      {
         if( ( no = _pdf417_lowerno( szCode[ i ] ) ) != -1 )
         {
         }
         else if( ( no = _pdf417_upperno( szCode[ i ] ) ) != -1 )
         {
            for( j = i + 1; j < iLen &&
                            _pdf417_upperno( szCode[ j ] ) != -1 &&
                            szCode[ j ] != ' '; j++ );
            if( j - i >= 4 )
            {
               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 28 /* LOWER->MIXED */ );
               if( iPos == -1 )
                  return -1;

               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 28 /* MIXED->UPPER */ );
               if( iPos == -1 )
                  return -1;

               iSubMode = SUBMODE_UPPER;
            }
            else
            {
               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 27 /* LOWER->1UPPER */ );
               if( iPos == -1 )
                  return -1;
            }
         }
         else if( ( no = _pdf417_mixedno( szCode[ i ] ) ) != -1 )
         {
            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 28 /* LOWER->MIXED */ );
            if( iPos == -1 )
               return -1;

            iSubMode = SUBMODE_MIXED;
         }
         else /* if( ( no = _pdf417_punctno( szCode[ i ] ) ) != -1 ) */
         {
            no = _pdf417_punctno( szCode[ i ] );
            for( j = i + 1; j < iLen &&
                            _pdf417_punctno( szCode[ j ] ) != -1 &&
                            _pdf417_mixedno( szCode[ j ] ) == -1; j++ );
            if( j - i >= 5 )
            {
               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 28 /* LOWER->MIXED */ );
               if( iPos == -1 )
                  return -1;

               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 25 /* MIXED->PUNCT */ );
               if( iPos == -1 )
                  return -1;

               iSubMode = SUBMODE_PUNCT;
            }
            else
            {
               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 29 /* LOWER->1PUNCT */ );
               if( iPos == -1 )
                  return -1;
            }
         }
      }
      else if( iSubMode == SUBMODE_MIXED )
      {
         if( ( no = _pdf417_mixedno( szCode[ i ] ) ) != -1 )
         {
         }
         else if( ( no = _pdf417_upperno( szCode[ i ] ) ) != -1 )
         {
            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 28 /* MIXED->UPPER */ );
            if( iPos == -1 )
               return -1;

            iSubMode = SUBMODE_UPPER;
         }
         else if( ( no = _pdf417_lowerno( szCode[ i ] ) ) != -1 )
         {
            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 27 /* MIXED->LOWER */ );
            if( iPos == -1 )
               return -1;

            iSubMode = SUBMODE_LOWER;
         }
         else /* if( ( no = _pdf417_punctno( szCode[ i ] ) ) != -1 ) */
         {
            no = _pdf417_punctno( szCode[ i ] );
            for( j = i + 1; j < iLen &&
                            _pdf417_punctno( szCode[ j ] ) != -1 &&
                            _pdf417_mixedno( szCode[ j ] ) == -1; j++ );
            if( j - i >= 4 )
            {
               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 25 /* MIXED->PUNCT */ );
               if( iPos == -1 )
                  return -1;

               iSubMode = SUBMODE_PUNCT;
            }
            else
            {
               iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 29 /* MIXED->1PUNCT */ );
               if( iPos == -1 )
                  return -1;
            }
         }
      }
      else /* if( iSubMode == SUBMODE_PUNCT ) */
      {
         if( ( no = _pdf417_punctno( szCode[ i ] ) ) != -1 )
         {
         }
         else if( ( no = _pdf417_upperno( szCode[ i ] ) ) != -1 )
         {
            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 29 /* PUNCT->UPPER */ );
            if( iPos == -1 )
               return -1;

            iSubMode = SUBMODE_UPPER;
         }
         else if( ( no = _pdf417_lowerno( szCode[ i ] ) ) != -1 )
         {
            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 29 /* PUNCT->UPPER */ );
            if( iPos == -1 )
               return -1;

            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 27 /* UPPER->LOWER */ );
            if( iPos == -1 )
               return -1;

            iSubMode = SUBMODE_LOWER;
         }
         else /* if( ( no = _pdf417_mixedno( szCode[ i ] ) ) != -1 ) */
         {
            no = _pdf417_mixedno( szCode[ i ] );
            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 29 /* PUNCT->UPPER */ );
            if( iPos == -1 )
               return -1;

            iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 28 /* UPPER->MIXED */ );
            if( iPos == -1 )
               return -1;

            iSubMode = SUBMODE_MIXED;
         }
      }

      iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, no );
      if( iPos == -1 )
         return -1;
   }

   if( i2 != -1 )
   {
      /* Dummy submode switch to flush half of codeword */
      iPos = _pdf417_encode_text_add( pCW, iPos, &i1, &i2, 29 );
      if( iPos == -1 )
         return -1;
   }
   return iPos;
}

static int _pdf417_encode_numeric( const char * szCode, int iLen, int * pCW, int iPos )
{
   /* Some very long integer (147bit) arithmetics shoud be implemented to encode
      digits in an effective way. I use more simple way and encode digits in groups
      not longer that 18 digits. 64bit integer arithmetics do this job */

   HB_LONGLONG  ill;
   int  i, j, k;

   HB_TRACE( HB_TR_DEBUG, ("encode numeric len=%d", iLen));
   if( iLen == 0 )
      return iPos;

   i = 0;
   while( i < iLen )
   {
      k = iLen - i;
      if( k > 18 )
         k = 18;

      if( iPos + 1 + k / 3 + 1 > MAX_CODEWORD_COUNT )
         return -1;

      pCW[ iPos++ ] = LATCH_NUMERIC;
      ill = 1;
      for( j = 0; j < k; j++ )
      {
         ill *= 10;
         ill += szCode[ i++ ] - '0';
      }
      k = k / 3 + 1;
      for( j = k - 1; j >= 0; j-- )
      {
         pCW[ iPos + j ] = ill % 900;
         ill /= 900;
      }
      iPos += k;
   }
   return iPos;
}

static int _pdf417_encode( const char * szCode, int iLen, int * pCW )
{
   int   i, j, iMode, iPos, iStart;

   /*
      Byte:     6 bytes -> 5 CW                1.2
      Text:     2 characters -> 1 CW           2
      Numeric: 44 digits -> 15 CW              2.933333
   */
   iMode = LATCH_TEXT;
   iStart = 0;
   iPos = 1; /* 0 is reserved for length */
   for( i = 0; i < iLen; i++ )
   {
      if( iMode == LATCH_TEXT )
      {
         if( _pdf417_isdigit( szCode[ i ] ) )
         {
            /*
               Digit in textmode uses 0.5 CW, in numeric mode 0.3409 CW.
               To save 2 CW + average remainig space in text mode we must
               have 2.5 / (0.5-0.3409) = 15.71 digits
            */
            for( j = i + 1; j < iLen && _pdf417_isdigit( szCode[ j ] ); j++ );
            if( j - i >= 16 )
            {
               iPos = _pdf417_encode_text( szCode + iStart, i - iStart, pCW, iPos );
               if( iPos == -1 || iPos >= MAX_CODEWORD_COUNT - 2 )
                  return -1;

               iMode = LATCH_NUMERIC;
               iStart = i;
            }
         }
         else if( ! _pdf417_isalpha( szCode[ i ] ) )
         {
            iPos = _pdf417_encode_text( szCode + iStart, i - iStart, pCW, iPos );
            if( iPos == -1 || iPos >= MAX_CODEWORD_COUNT - 2 )
               return -1;

            /* Switch if we have 3 or more non-alphanumeric values */
            for( j = i + 1; j < iLen && ! _pdf417_isalpha( szCode[ j ] ); j++ );
            if( j - i >= 3 )
            {
               iMode = LATCH_BYTE;
               iStart = i;
            }
            else
            {
               pCW[ iPos++ ] = SHIFT_BYTE;
               pCW[ iPos++ ] = szCode[ i ];
            }
         }
      }
      else if( iMode == LATCH_BYTE )
      {
         if( _pdf417_isdigit( szCode[ i ] ) )
         {
            for( j = i + 1; j < iLen && _pdf417_isdigit( szCode[ i ] ); j++ );
            if( j - i >= 4 )
            {
               iPos = _pdf417_encode_byte( szCode + iStart, i - iStart, pCW, iPos );
               if( iPos == -1 || iPos >= MAX_CODEWORD_COUNT - 2 )
                  return -1;
               iMode = LATCH_NUMERIC;
               iStart = i;
            }
         }
         if( iMode == LATCH_BYTE && _pdf417_isalpha( szCode[ i ] ) )
         {
            for( j = i + 1; j < iLen && _pdf417_isdigit( szCode[ i ] ); j++ );
            if( j - i >= 4 )
            {
               iPos = _pdf417_encode_byte( szCode + iStart, i - iStart, pCW, iPos );
               if( iPos == -1 || iPos >= MAX_CODEWORD_COUNT - 2 )
                  return -1;
               iMode = LATCH_TEXT;
               iStart = i;
            }
         }
      }
      else /* if( iMode == LATCH_NUMERIC ) */
      {
         if( _pdf417_isdigit( szCode[ i ] ) )
         {
         }
         else if( ! _pdf417_isalpha( szCode[ i ] ) )
         {
            iPos = _pdf417_encode_numeric( szCode + iStart, i - iStart, pCW, iPos );
            if( iPos == -1 || iPos >= MAX_CODEWORD_COUNT - 2 )
               return -1;

            for( j = i + 1; j < iLen && ! _pdf417_isalpha( szCode[ j ] ); j++ );
            if( j - i >= 2 )
            {
               iMode = LATCH_BYTE;
               iStart = i;
            }
            else
            {
               for( j = i + 2; j < iLen && _pdf417_isdigit( szCode[ j ] ); j++ );
               if( j - i >= 16 )
               {
                  pCW[ iPos++ ] = SHIFT_BYTE;
                  pCW[ iPos++ ] = szCode[ i ];
               }
               else
               {
                  iMode = LATCH_BYTE;
                  iStart = i;
               }
            }
         }
         else /* if( _pdf417_isalpha( szCode[ i ] ) ) */
         {
            iPos = _pdf417_encode_numeric( szCode + iStart, i - iStart, pCW, iPos );
            if( iPos == -1 || iPos >= MAX_CODEWORD_COUNT - 2 )
               return -1;
            iMode = LATCH_TEXT;
            iStart = i;
         }
      }
   }
   if( iMode == LATCH_TEXT )
      iPos = _pdf417_encode_text( szCode + iStart, i - iStart, pCW, iPos );
   else if( iMode == LATCH_BYTE )
      iPos = _pdf417_encode_byte( szCode + iStart, i - iStart, pCW, iPos );
   else /* if( iMode == LATCH_NUMERIC ) */
      iPos = _pdf417_encode_numeric( szCode + iStart, i - iStart, pCW, iPos );
   return iPos;
}

static void _pdf417_reed_solomon( int * pCW, int iLen, int iLevel )
{
   int * pEC;
   unsigned short * coef;
   int i, j, iM, iECLen;

   iECLen = _pdf417_ec_size( iLevel );

   pEC = pCW + iLen;

   switch( iLevel )
   {
      case 0:
        coef = s_rs0;
        break;
      case 1:
        coef = s_rs1;
        break;
      case 2:
        coef = s_rs2;
        break;
      case 3:
        coef = s_rs3;
        break;
      case 4:
        coef = s_rs4;
        break;
      case 5:
        coef = s_rs5;
        break;
      case 6:
        coef = s_rs6;
        break;
      case 7:
        coef = s_rs7;
        break;
      case 8:
        coef = s_rs8;
        break;
   }

   for( i = 0; i < iECLen; i++ )
      pEC[ i ] = 0;

   for( i = 0; i < iLen; i++ )
   {
      iM = ( pCW[ i ] + pEC[ iECLen - 1 ] ) % 929;
      for( j = iECLen - 1; j >= 0; j-- )
      {
         if( j )
            pEC[ j ] = ( pEC[ j - 1 ] + 929 - ( iM * coef[ j ] ) % 929 ) % 929;
         else
            pEC[ j ] = ( 929 - ( iM * coef[ j ] ) % 929 ) % 929;
      }
   }
   for( i = 0; i < iECLen; i++ )
   {
      if( pEC[ i ] )
         pEC[ i ] = 929 - pEC[ i ];
   }
   for( i = 0; i < iECLen / 2; i++ )
   {
      j = pEC[ i ];
      pEC[ i ] = pEC[ iECLen - 1 - i ];
      pEC[ iECLen - 1 - i ] = j;
   }
}

PHB_ZEBRA hb_zebra_create_pdf417( const char * szCode, HB_SIZE nLen, int iFlags, int iColCount )
{
   PHB_ZEBRA  pZebra;
   int *      pCW;
   int        i, j, iLevel, iRowCount, iDataCount, iCount, iLen = ( int ) nLen;

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_PDF417;

   pCW = ( int * ) hb_xgrab( sizeof( int ) * MAX_CODEWORD_COUNT );
   iDataCount = _pdf417_encode( szCode, iLen, pCW );
   if( iDataCount == -1 )
   {
      hb_xfree( pCW );
      pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
      return pZebra;
   }

   iLevel = iFlags & HB_ZEBRA_FLAG_PDF417_LEVEL_MASK;
   if( iLevel != 0 )
   {
      i = HB_ZEBRA_FLAG_PDF417_LEVEL_MASK;
      while( ( i & 1 ) == 0 )
      {
         iLevel >>= 1;
         i >>= 1;
      }

      iLevel--;

      if( iLevel > 8 )
         iLevel = 8;
   }
   else
      iLevel = _pdf417_default_ec_level( iDataCount );

   if( iDataCount + _pdf417_ec_size( iLevel ) > MAX_CODEWORD_COUNT )
   {
      hb_xfree( pCW );
      pZebra->iError = HB_ZEBRA_ERROR_TOOLARGE;
      return pZebra;
   }

   if( iColCount <= 0 )
   {
      iRowCount = MIN_ROW_COUNT;
      for( ;; )
      {
         iColCount = ( iDataCount + _pdf417_ec_size( iLevel ) + iRowCount - 1 ) / iRowCount;
         /* w:h aspect ration is less than 2:1 for defaul 3x module height */
         HB_TRACE( HB_TR_DEBUG, ("iDataCount=%d iRowCount=%d iColCount=%d", iDataCount, iRowCount, iColCount));
         if( ( _pdf417_width( iColCount, iFlags ) < iRowCount * 3 * 2 || iColCount == 1 ) &&
             iColCount <= MAX_COL_COUNT &&
             iColCount * iRowCount <= MAX_CODEWORD_COUNT ) /* This should solve 928 (= 29 columns * 32 rows) problem */
         {
            HB_TRACE( HB_TR_DEBUG, ("tinka"));
            break;
         }
         iRowCount++;
      }
   }
   else
   {
      if( iColCount > MAX_COL_COUNT )
         iColCount = MAX_COL_COUNT;
      iRowCount = ( iDataCount + _pdf417_ec_size( iLevel ) + iColCount - 1 ) / iColCount;
      if( iRowCount < MIN_ROW_COUNT )
         iRowCount = MIN_ROW_COUNT;
   }

   iCount = iRowCount * iColCount;
   if( iRowCount > MAX_ROW_COUNT || iCount > MAX_CODEWORD_COUNT )
   {
      hb_xfree( pCW );
      pZebra->iError = HB_ZEBRA_ERROR_ARGUMENT;
      return pZebra;
   }

   pCW[ 0 ] = iCount - _pdf417_ec_size( iLevel );

   /* Padding */
   for( ; iDataCount < iCount - _pdf417_ec_size( iLevel ); iDataCount++ )
      pCW[ iDataCount ] = LATCH_TEXT;

   /* Reed-Solomon error correction */
   _pdf417_reed_solomon( pCW, iDataCount, iLevel );

   pZebra->iCol = _pdf417_width( iColCount, iFlags );
   pZebra->szCode = hb_strdup( "" );
   pZebra->pBits = hb_bitbuffer_create();

   HB_TRACE( HB_TR_DEBUG, ("iColCount=%d iRowCount=%d ECCount=%d", iColCount, iRowCount, _pdf417_ec_size( iLevel )));
   for( i = 0; i < iCount; i++ )
   HB_TRACE( HB_TR_DEBUG, ("%d", pCW[ i ]));


   for( i = 0; i < iRowCount; i++ )
   {
      hb_bitbuffer_cat_int( pZebra->pBits, CODE_START, 17 );
      hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
      hb_bitbuffer_cat_int( pZebra->pBits,
                            s_code[ i % 3 ][ _pdf417_left_codeword( i, iRowCount, iColCount, iLevel ) ],
                            15 );
      hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );

      for( j = 0; j < iColCount; j++ )
      {
         hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
         hb_bitbuffer_cat_int( pZebra->pBits,
                               s_code[ i % 3 ][ pCW[ i * iColCount + j ] ],
                               15 );
         hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );
      }

      if( iFlags & HB_ZEBRA_FLAG_PDF417_TRUNCATED )
      {
         hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
      }
      else
      {
         hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
         hb_bitbuffer_cat_int( pZebra->pBits,
                               s_code[ i % 3 ][ _pdf417_right_codeword( i, iRowCount, iColCount, iLevel ) ],
                               15 );
         hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );
         hb_bitbuffer_cat_int( pZebra->pBits, CODE_STOP, 18 );
      }
   }
   hb_xfree( pCW );
   return pZebra;
}


HB_FUNC( HB_ZEBRA_CREATE_PDF417 )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_pdf417( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ), hb_parni( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
