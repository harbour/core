/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2011 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
     QR Code is ISO/IEC18004

     JIS-X-0510  QR Code standard in Japaneese language :)
       http://sourceforge.jp/projects/qrcode/docs/qrcode_specification_ja/en/1/qrcode_specification_ja.pdf

http://en.wikipedia.org/wiki/QR_Code
http://www.denso-wave.com/qrcode/index-e.html
http://www.itsc.org.sg/pdf/synthesis08/Three_QR_Code.pdf
http://www.qrme.co.uk/qr-code-resources/understanding-a-qr-code.html
http://www.swetake.com/qr/index-e.html
http://www.codeproject.com/KB/cs/qrcode.aspx
http://sourceforge.jp/projects/reedsolomon
http://twit88.com/home/
http://qrcode.sourceforge.jp/
http://zxing.org/w/decode.jspx                          Online decoder 
http://blog.qr4.nl/Online-QR-Code_Decoder.aspx          Online decoder (not all codes are decoded)
http://www.thonky.com/qr-code-tutorial/                 Tutorial
http://code.google.com/p/zxing/                         Java library
http://goqr.me/                                         Online encode 
http://www.pclviewer.com/rs2/calculator.html            Reed-solomon ECC calculator
http://raidenii.net/files/datasheets/misc/qr_code.pdf

*/

#include "hbzebra.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* #define DEBUG_CODE */

typedef struct
{
   unsigned char  uiCount;
   unsigned char  uiData;
   unsigned char  uiECC;
} QRBLOCKPARAM;

typedef struct
{
   unsigned int    uiECC;
   QRBLOCKPARAM    block[ 2 ];
} QRLEVEL, * PQRLEVEL;

typedef struct
{
   unsigned int    uiTotal;       /* total number of codewords */
   QRLEVEL         level[ 4 ];   
} QRVERSION, * PQRVERSION;

static const QRVERSION s_version[] =
{
   {  26, {{   7, {{ 1,  19,  7}, { 0,   0,  0}}},   /*  1    2 * 2 */
           {  10, {{ 1,  16, 10}, { 0,   0,  0}}},   /*       4 * 2 */
           {  13, {{ 1,  13, 13}, { 0,   0,  0}}},   /*       6 * 2 */
           {  17, {{ 1,   9, 17}, { 0,   0,  0}}}}}, /*       8 * 2 */
   {  44, {{  10, {{ 1,  34, 10}, { 0,   0,  0}}},   /*  2    4 * 2 */
           {  16, {{ 1,  28, 16}, { 0,   0,  0}}},
           {  22, {{ 1,  22, 22}, { 0,   0,  0}}},
           {  28, {{ 1,  16, 28}, { 0,   0,  0}}}}}, 
   {  70, {{  15, {{ 1,  55, 15}, { 0,   0,  0}}},   /*  3    7 * 2 */
           {  26, {{ 1,  44, 26}, { 0,   0,  0}}}, 
           {  36, {{ 2,  17, 18}, { 0,   0,  0}}}, 
           {  44, {{ 2,  13, 22}, { 0,   0,  0}}}}},
   { 100, {{  20, {{ 1,  80, 20}, { 0,   0,  0}}},   /*  4 */
           {  36, {{ 2,  32, 18}, { 0,   0,  0}}}, 
           {  52, {{ 2,  24, 26}, { 0,   0,  0}}}, 
           {  64, {{ 4,   9, 16}, { 0,   0,  0}}}}},
   { 134, {{  26, {{ 1, 108, 26}, { 0,   0,  0}}},   /*  5 */
           {  48, {{ 2,  43, 24}, { 0,   0,  0}}}, 
           {  72, {{ 2,  15, 18}, { 2,  16, 18}}}, 
           {  88, {{ 2,  11, 22}, { 2,  12, 22}}}}},
   { 172, {{  36, {{ 2,  68, 18}, { 0,   0,  0}}},   /*  6 */
           {  64, {{ 4,  27, 16}, { 0,   0,  0}}}, 
           {  96, {{ 4,  19, 24}, { 0,   0,  0}}}, 
           { 112, {{ 4,  15, 28}, { 0,   0,  0}}}}},
   { 196, {{  40, {{ 2,  78, 20}, { 0,   0,  0}}},   /*  7 */
           {  72, {{ 4,  31, 18}, { 0,   0,  0}}}, 
           { 108, {{ 2,  14, 18}, { 4,  15, 18}}}, 
           { 130, {{ 4,  13, 26}, { 1,  14, 26}}}}},
   { 242, {{  48, {{ 2,  97, 24}, { 0,   0,  0}}},   /*  8 */
           {  88, {{ 2,  38, 22}, { 2,  39, 22}}}, 
           { 132, {{ 4,  18, 22}, { 2,  19, 22}}}, 
           { 156, {{ 4,  14, 26}, { 2,  15, 26}}}}}, 
   { 292, {{  60, {{ 2, 116, 30}, { 0,   0,  0}}},   /*  9 */
           { 110, {{ 3,  36, 22}, { 2,  37, 22}}}, 
           { 160, {{ 4,  16, 20}, { 4,  17, 20}}}, 
           { 192, {{ 4,  12, 24}, { 4,  13, 24}}}}}, 
   { 346, {{  72, {{ 2,  68, 18}, { 2,  69, 18}}},   /* 10 */
           { 130, {{ 4,  43, 26}, { 1,  44, 26}}}, 
           { 192, {{ 6,  19, 24}, { 2,  20, 24}}}, 
           { 224, {{ 6,  15, 28}, { 2,  16, 28}}}}}, 
   { 404, {{  80, {{ 4,  81, 20}, { 0,   0,  0}}},   /* 11 */
           { 150, {{ 1,  50, 30}, { 4,  51, 30}}}, 
           { 224, {{ 4,  22, 28}, { 4,  23, 28}}}, 
           { 264, {{ 3,  12, 24}, { 8,  13, 24}}}}}, 
   { 466, {{  96, {{ 2,  92, 24}, { 2,  93, 24}}},   /* 12 */
           { 176, {{ 6,  36, 22}, { 2,  37, 22}}}, 
           { 260, {{ 4,  20, 26}, { 6,  21, 26}}}, 
           { 308, {{ 7,  14, 28}, { 4,  15, 28}}}}}, 
   { 532, {{ 104, {{ 4, 107, 26}, { 0,   0,  0}}},   /* 13 */
           { 198, {{ 8,  37, 22}, { 1,  38, 22}}}, 
           { 288, {{ 8,  20, 24}, { 4,  21, 24}}}, 
           { 352, {{12,  11, 22}, { 4,  12, 22}}}}}, 
   { 581, {{ 120, {{ 3, 115, 30}, { 1, 116, 30}}},   /* 14 */
           { 216, {{ 4,  40, 24}, { 5,  41, 24}}}, 
           { 320, {{11,  16, 20}, { 5,  17, 20}}}, 
           { 384, {{11,  12, 24}, { 5,  13, 24}}}}}, 
   { 655, {{ 132, {{ 5,  87, 22}, { 1,  88, 22}}},   /* 15 */
           { 240, {{ 5,  41, 24}, { 5,  42, 24}}}, 
           { 360, {{ 5,  24, 30}, { 7,  25, 30}}}, 
           { 432, {{11,  12, 24}, { 7,  13, 24}}}}}, 
   { 733, {{ 144, {{ 5,  98, 24}, { 1,  99, 24}}},   /* 16 */
           { 280, {{ 7,  45, 28}, { 3,  46, 28}}}, 
           { 408, {{15,  19, 24}, { 2,  20, 24}}}, 
           { 480, {{ 3,  15, 30}, {13,  16, 30}}}}}, 
   { 815, {{ 168, {{ 1, 107, 28}, { 5, 108, 28}}},   /* 17 */
           { 308, {{10,  46, 28}, { 1,  47, 28}}}, 
           { 448, {{ 1,  22, 28}, {15,  23, 28}}}, 
           { 532, {{ 2,  14, 28}, {17,  15, 28}}}}}, 
   { 901, {{ 180, {{ 5, 120, 30}, { 1, 121, 30}}},   /* 18 */
           { 338, {{ 9,  43, 26}, { 4,  44, 26}}}, 
           { 504, {{17,  22, 28}, { 1,  23, 28}}}, 
           { 588, {{ 2,  14, 28}, {19,  15, 28}}}}}, 
   { 991, {{ 196, {{ 3, 113, 28}, { 4, 114, 28}}},   /* 19 */
           { 364, {{ 3,  44, 26}, {11,  45, 26}}}, 
           { 546, {{17,  21, 26}, { 4,  22, 26}}}, 
           { 650, {{ 9,  13, 26}, {16,  14, 26}}}}}, 
   {1085, {{ 224, {{ 3, 107, 28}, { 5, 108, 28}}},   /* 20 */
           { 416, {{ 3,  41, 26}, {13,  42, 26}}}, 
           { 600, {{15,  24, 30}, { 5,  25, 30}}}, 
           { 700, {{15,  15, 28}, {10,  16, 28}}}}}, 
   {1156, {{ 224, {{ 4, 116, 28}, { 4, 117, 28}}},   /* 21 */
           { 442, {{17,  42, 26}, { 0,   0,  0}}}, 
           { 644, {{17,  22, 28}, { 6,  23, 28}}}, 
           { 750, {{19,  16, 30}, { 6,  17, 30}}}}}, 
   {1258, {{ 252, {{ 2, 111, 28}, { 7, 112, 28}}},   /* 22 */
           { 476, {{17,  46, 28}, { 0,   0,  0}}}, 
           { 690, {{ 7,  24, 30}, {16,  25, 30}}}, 
           { 816, {{34,  13, 24}, { 0,   0,  0}}}}}, 
   {1364, {{ 270, {{ 4, 121, 30}, { 5, 122, 30}}},   /* 23 */
           { 504, {{ 4,  47, 28}, {14,  48, 28}}}, 
           { 750, {{11,  24, 30}, {14,  25, 30}}}, 
           { 900, {{16,  15, 30}, {14,  16, 30}}}}}, 
   {1474, {{ 300, {{ 6, 117, 30}, { 4, 118, 30}}},   /* 24 */
           { 560, {{ 6,  45, 28}, {14,  46, 28}}}, 
           { 810, {{11,  24, 30}, {16,  25, 30}}}, 
           { 960, {{30,  16, 30}, { 2,  17, 30}}}}}, 
   {1588, {{ 312, {{ 8, 106, 26}, { 4, 107, 26}}},   /* 25 */
           { 588, {{ 8,  47, 28}, {13,  48, 28}}}, 
           { 870, {{ 7,  24, 30}, {22,  25, 30}}}, 
           {1050, {{22,  15, 30}, {13,  16, 30}}}}}, 
   {1706, {{ 336, {{10, 114, 28}, { 2, 115, 28}}},   /* 26 */
           { 644, {{19,  46, 28}, { 4,  47, 28}}}, 
           { 952, {{28,  22, 28}, { 6,  23, 28}}}, 
           {1110, {{33,  16, 30}, { 4,  17, 30}}}}}, 
   {1828, {{ 360, {{ 8, 122, 30}, { 4, 123, 30}}},   /* 27 */
           { 700, {{22,  45, 28}, { 3,  46, 28}}}, 
           {1020, {{ 8,  23, 30}, {26,  24, 30}}}, 
           {1200, {{12,  15, 30}, {28,  16, 30}}}}}, 
   {1921, {{ 390, {{ 3, 117, 30}, {10, 118, 30}}},   /* 28 */
           { 728, {{ 3,  45, 28}, {23,  46, 28}}}, 
           {1050, {{ 4,  24, 30}, {31,  25, 30}}}, 
           {1260, {{11,  15, 30}, {31,  16, 30}}}}}, 
   {2051, {{ 420, {{ 7, 116, 30}, { 7, 117, 30}}},   /* 29 */
           { 784, {{21,  45, 28}, { 7,  46, 28}}}, 
           {1140, {{ 1,  23, 30}, {37,  24, 30}}}, 
           {1350, {{19,  15, 30}, {26,  16, 30}}}}}, 
   {2185, {{ 450, {{ 5, 115, 30}, {10, 116, 30}}},   /* 30 */
           { 812, {{19,  47, 28}, {10,  48, 28}}}, 
           {1200, {{15,  24, 30}, {25,  25, 30}}}, 
           {1440, {{23,  15, 30}, {25,  16, 30}}}}}, 
   {2323, {{ 480, {{13, 115, 30}, { 3, 116, 30}}},   /* 31 */
           { 868, {{ 2,  46, 28}, {29,  47, 28}}}, 
           {1290, {{42,  24, 30}, { 1,  25, 30}}}, 
           {1530, {{23,  15, 30}, {28,  16, 30}}}}}, 
   {2465, {{ 510, {{17, 115, 30}, { 0,   0,  0}}},   /* 32 */
           { 924, {{10,  46, 28}, {23,  47, 28}}}, 
           {1350, {{10,  24, 30}, {35,  25, 30}}}, 
           {1620, {{19,  15, 30}, {35,  16, 30}}}}}, 
   {2611, {{ 540, {{17, 115, 30}, { 1, 116, 30}}},   /* 33 */
           { 980, {{14,  46, 28}, {21,  47, 28}}}, 
           {1440, {{29,  24, 30}, {19,  25, 30}}}, 
           {1710, {{11,  15, 30}, {46,  16, 30}}}}}, 
   {2761, {{ 570, {{13, 115, 30}, { 6, 116, 30}}},   /* 34 */
           {1036, {{14,  46, 28}, {23,  47, 28}}}, 
           {1530, {{44,  24, 30}, { 7,  25, 30}}}, 
           {1800, {{59,  16, 30}, { 1,  17, 30}}}}}, 
   {2876, {{ 570, {{12, 121, 30}, { 7, 122, 30}}},   /* 35 */
           {1064, {{12,  47, 28}, {26,  48, 28}}}, 
           {1590, {{39,  24, 30}, {14,  25, 30}}}, 
           {1890, {{22,  15, 30}, {41,  16, 30}}}}}, 
   {3034, {{ 600, {{ 6, 121, 30}, {14, 122, 30}}},   /* 36 */
           {1120, {{ 6,  47, 28}, {34,  48, 28}}}, 
           {1680, {{46,  24, 30}, {10,  25, 30}}}, 
           {1980, {{ 2,  15, 30}, {64,  16, 30}}}}}, 
   {3196, {{ 630, {{17, 122, 30}, { 4, 123, 30}}},   /* 37 */
           {1204, {{29,  46, 28}, {14,  47, 28}}}, 
           {1770, {{49,  24, 30}, {10,  25, 30}}}, 
           {2100, {{24,  15, 30}, {46,  16, 30}}}}}, 
   {3362, {{ 660, {{ 4, 122, 30}, {18, 123, 30}}},   /* 38 */
           {1260, {{13,  46, 28}, {32,  47, 28}}}, 
           {1860, {{48,  24, 30}, {14,  25, 30}}}, 
           {2220, {{42,  15, 30}, {32,  16, 30}}}}}, 
   {3532, {{ 720, {{20, 117, 30}, { 4, 118, 30}}},   /* 39 */
           {1316, {{40,  47, 28}, { 7,  48, 28}}}, 
           {1950, {{43,  24, 30}, {22,  25, 30}}}, 
           {2310, {{10,  15, 30}, {67,  16, 30}}}}}, 
   {3706, {{ 750, {{19, 118, 30}, { 6, 119, 30}}},   /* 40 */
           {1372, {{18,  47, 28}, {31,  48, 28}}}, 
           {2040, {{34,  24, 30}, {34,  25, 30}}}, 
           {2430, {{20,  15, 30}, {61,  16, 30}}}}}
};


static const unsigned char s_rev[256] = 
{
#define R2(n)      n,     n + 2*64,     n + 1*64,     n + 3*64
#define R4(n)  R2(n), R2(n + 2*16), R2(n + 1*16), R2(n + 3*16)
#define R6(n)  R4(n), R4(n + 2*4 ), R4(n + 1*4 ), R4(n + 3*4 )
    R6(0), R6(2), R6(1), R6(3)
};


/* zero terminated align patterns positions */
static unsigned char s_align01[] = { 0 }; 
static unsigned char s_align02[] = { 6, 18, 0 };
static unsigned char s_align03[] = { 6, 22, 0 };
static unsigned char s_align04[] = { 6, 26, 0 };
static unsigned char s_align05[] = { 6, 30, 0 };
static unsigned char s_align06[] = { 6, 34, 0 };
static unsigned char s_align07[] = { 6, 22, 38, 0 };
static unsigned char s_align08[] = { 6, 24, 42, 0 };
static unsigned char s_align09[] = { 6, 26, 46, 0 };
static unsigned char s_align00[] = { 6, 28, 50, 0 };
static unsigned char s_align11[] = { 6, 30, 54, 0 };
static unsigned char s_align12[] = { 6, 32, 58, 0 };
static unsigned char s_align13[] = { 6, 34, 62, 0 };
static unsigned char s_align14[] = { 6, 26, 46, 66, 0 };
static unsigned char s_align15[] = { 6, 26, 48, 70, 0 };
static unsigned char s_align16[] = { 6, 26, 50, 74, 0 };
static unsigned char s_align17[] = { 6, 30, 54, 78, 0 };
static unsigned char s_align18[] = { 6, 30, 56, 82, 0 };
static unsigned char s_align19[] = { 6, 30, 58, 86, 0 };
static unsigned char s_align10[] = { 6, 34, 62, 90, 0 };
static unsigned char s_align21[] = { 6, 28, 50, 72,  94, 0 };
static unsigned char s_align22[] = { 6, 26, 50, 74,  98, 0 };
static unsigned char s_align23[] = { 6, 30, 54, 78, 102, 0 };
static unsigned char s_align24[] = { 6, 28, 54, 80, 106, 0 };
static unsigned char s_align25[] = { 6, 32, 58, 84, 110, 0 };
static unsigned char s_align26[] = { 6, 30, 58, 86, 114, 0 };
static unsigned char s_align27[] = { 6, 34, 62, 90, 118, 0 };
static unsigned char s_align28[] = { 6, 26, 50, 74,  98, 122, 0 };
static unsigned char s_align29[] = { 6, 30, 54, 78, 102, 126, 0 };
static unsigned char s_align20[] = { 6, 26, 52, 78, 104, 130, 0 };
static unsigned char s_align31[] = { 6, 30, 56, 82, 108, 134, 0 };
static unsigned char s_align32[] = { 6, 34, 60, 86, 112, 138, 0 };
static unsigned char s_align33[] = { 6, 30, 58, 86, 114, 142, 0 };
static unsigned char s_align34[] = { 6, 34, 62, 90, 118, 146, 0 };
static unsigned char s_align35[] = { 6, 30, 54, 78, 102, 126, 150, 0 };
static unsigned char s_align36[] = { 6, 24, 50, 76, 102, 128, 154, 0 };
static unsigned char s_align37[] = { 6, 28, 54, 80, 106, 132, 158, 0 };
static unsigned char s_align38[] = { 6, 32, 58, 84, 110, 136, 162, 0 };
static unsigned char s_align39[] = { 6, 26, 54, 82, 110, 138, 166, 0 };
static unsigned char s_align40[] = { 6, 30, 58, 86, 114, 142, 170, 0 };

static unsigned char * s_align[ 40 ] = { 
   s_align01,
   s_align02,
   s_align03,
   s_align04,
   s_align05,
   s_align06,
   s_align07,
   s_align08,
   s_align09,
   s_align00,
   s_align11,
   s_align12,
   s_align13,
   s_align14,
   s_align15,
   s_align16,
   s_align17,
   s_align18,
   s_align19,
   s_align10,
   s_align21,
   s_align22,
   s_align23,
   s_align24,
   s_align25,
   s_align26,
   s_align27,
   s_align28,
   s_align29,
   s_align20,
   s_align31,
   s_align32,
   s_align33,
   s_align34,
   s_align35,
   s_align36,
   s_align37,
   s_align38,
   s_align39,
   s_align40 };


#ifdef DEBUG_CODE
static int _qr_check_version_table( void )
{
   const QRVERSION * pQRVersion;
   int iV, iL;
   unsigned int uiSumD, uiSumE;
  
   for( iV = 1; iV <= 40; iV++ )
   {
      pQRVersion = & s_version[ iV - 1 ];
      for( iL = 0; iL < 4; iL++ )
      {
         uiSumE = ( unsigned int ) ( pQRVersion->level[ iL ].block[ 0 ].uiCount ) * ( pQRVersion->level[ iL ].block[ 0 ].uiECC ) + 
                  ( unsigned int ) ( pQRVersion->level[ iL ].block[ 1 ].uiCount ) * ( pQRVersion->level[ iL ].block[ 1 ].uiECC );
         if( uiSumE != pQRVersion->level[ iL ].uiECC )
            return iV + 10000 + (iL + 1) * 1000;

         uiSumD = ( unsigned int ) ( pQRVersion->level[ iL ].block[ 0 ].uiCount ) * ( pQRVersion->level[ iL ].block[ 0 ].uiData ) + 
                  ( unsigned int ) ( pQRVersion->level[ iL ].block[ 1 ].uiCount ) * ( pQRVersion->level[ iL ].block[ 1 ].uiData );
         if( uiSumD + uiSumE != pQRVersion->uiTotal )
            return iV + 20000 + (iL + 1) * 1000;

         if( pQRVersion->level[ iL ].block[ 1 ].uiCount > 0 )
         {
            if( pQRVersion->level[ iL ].block[ 0 ].uiData > pQRVersion->level[ iL ].block[ 1 ].uiData )
               return iV + 30000 + (iL + 1) * 1000;

            if( pQRVersion->level[ iL ].block[ 0 ].uiECC != pQRVersion->level[ iL ].block[ 1 ].uiECC )
               return iV + 40000 + (iL + 1) * 1000;
         }
      }
   }
   return 0;
}

HB_FUNC( _QR_CHECK_VERSION_TABLE )
{
  hb_retni( _qr_check_version_table() );
}
#endif


static int _qr_version_crc( int iVersion )
{
   int i, iValue, iVersionRev;
   
   iVersionRev = 0;
   for( i = 0; i < 6; i++ )
   {
      iVersionRev <<= 1;
      if( iVersion & 1 )
         iVersionRev |= 1;
      iVersion >>= 1;
   }

   iValue = iVersionRev;
   for( i = 0; i < 6; i++ )
   {
      if( iValue & 1 )
         iValue ^= 0x149F; /* 0x149F - reversed 12-bit polynom */
      iValue >>= 1;
   }
   iValue <<= 6;
   iValue |= iVersionRev;
   return iValue; /* 18-bit return value */
}


static int _qr_format_crc( int iLevel, int iMask )
{
   int i, iValue, iRev;
   
   iRev = ( ( iLevel & 1 ) ? 0 : 2 ) | ( ( iLevel & 2 ) ? 1 : 0 );  /* 0->1, 1->0, 2->3, 3->2 + reverse bits */
   iRev |= ( ( iMask & 1 ) ? 16 : 0 ) | ( ( iMask & 2 ) ? 8 : 0 ) | ( ( iMask & 4 ) ? 4 : 0 ); /* reverse bits */

   iValue = iRev;
   for( i = 0; i < 5; i++ )
   {
      if( iValue & 1 )
         iValue ^= 0x765;  /* 0x765 - reversed 10-bit polynom */
      iValue >>= 1;
   }
   iValue <<= 5;
   iValue |= iRev;
   return iValue ^ 0x2415; /* 15-bit return value */
}


#ifdef DEBUG_CODE
HB_FUNC( _QR_VERSION_CRC )
{
  hb_retni( _qr_version_crc( hb_parni( 1 ) ) );
}


HB_FUNC( _QR_FORMAT_CRC )
{
  hb_retni( _qr_format_crc( hb_parni( 1 ), hb_parni( 2 ) ) );
}
#endif


static int _qr_versionlength( int iVersion )
{
   return iVersion * 4 + 17;
}


static int _qr_fixed( int iVersion, int iRow, int iCol )
{
   int iLength = _qr_versionlength( iVersion );
   unsigned char * pi, *pj;

   /* position detection markers and versino info */
   if( iRow < 9 && iCol < 9 )   
      return 1;
   if( iRow < 9 && iCol >= iLength - 8 ) 
      return 1;
   if( iRow >= iLength - 8 && iCol < 9 ) 
      return 1;

   /* timing patterns */
   if( iRow == 6 || iCol == 6 )
      return 1;

   /* alignment patterns */
   pi = s_align[ iVersion - 1 ];
   for( ; *pi; pi++ )
   {
      pj = s_align[ iVersion - 1 ];
      for( ; *pj; pj++ )
      {
         if( iRow - 2 <= ( int ) *pi && ( int ) *pi <= iRow + 2 &&
             iCol - 2 <= ( int ) *pj && ( int ) *pj <= iCol + 2 )
         {
            if( ( *pi == 6 && ( int ) *pj > iLength - 10 ) ||
                ( ( int ) *pi > iLength - 10 && *pj == 6 ) )
               break;

            return 1;
         }
      }
   }

   /* format info */
   if( iVersion >= 7 &&
       ( ( iCol < 6 && iRow >= iLength - 11 ) || ( iCol >= iLength - 11 && iRow < 6 ) ) )
      return 1;

   return 0;
}


static PHB_BITBUFFER _qr_interlace( PHB_BITBUFFER pData, unsigned char * pECC, int iVersion, int iLevel )
{
   const QRVERSION * pVersion = &s_version[ iVersion - 1 ];
   const QRLEVEL * pLevel = &( pVersion->level[ iLevel ] );
   PHB_BITBUFFER pRet;
   HB_BYTE * pDataBuf, * pRetBuf;
   unsigned int uiDst, uiSrc, uiPos, uiBlock;

   pRet = hb_bitbuffer_create();
   hb_bitbuffer_set( pRet, pVersion->uiTotal * 8, HB_FALSE ); /* Allocate */

   pRetBuf = hb_bitbuffer_buffer( pRet );
   pDataBuf = hb_bitbuffer_buffer( pData );

   uiDst = 0;
   for( uiPos = 0; uiPos < ( unsigned int ) pLevel->block[ 0 ].uiData || uiPos < ( unsigned int ) pLevel->block[ 1 ].uiData; uiPos++ )
   {
      uiSrc = 0;
      for( uiBlock = 0; uiBlock < ( unsigned int ) pLevel->block[ 0 ].uiCount; uiBlock++ )
      {
         if( uiPos < ( unsigned int ) pLevel->block[ 0 ].uiData )
         {
            pRetBuf[ uiDst++ ] = pDataBuf[ uiPos + uiSrc ];
         }
         uiSrc += pLevel->block[ 0 ].uiData;
      }
      if( pLevel->block[ 1 ].uiCount )
      {
         for( uiBlock = 0; uiBlock < ( unsigned int ) pLevel->block[ 1 ].uiCount; uiBlock++ )
         {
            pRetBuf[ uiDst++ ] = pDataBuf[ uiPos + uiSrc ];
            uiSrc += pLevel->block[ 1 ].uiData;
         }
      }
   }

   for( uiPos = 0; uiPos < ( unsigned int ) pLevel->block[ 0 ].uiECC; uiPos++ )
   {
      uiSrc = 0;
      for( uiBlock = 0; uiBlock < ( unsigned int ) pLevel->block[ 0 ].uiCount; uiBlock++ )
      {
         if( uiPos < ( unsigned int ) pLevel->block[ 0 ].uiECC )
         {
            pRetBuf[ uiDst++ ] = s_rev[ pECC[ uiPos + uiSrc ] ];
         }
         uiSrc += pLevel->block[ 0 ].uiECC;
      }
      if( pLevel->block[ 1 ].uiCount )
      {
         for( uiBlock = 0; uiBlock < ( unsigned int ) pLevel->block[ 1 ].uiCount; uiBlock++ )
         {
            pRetBuf[ uiDst++ ] = s_rev[ pECC[ uiPos + uiSrc ] ];
            uiSrc += pLevel->block[ 1 ].uiECC;
         }
      }
   }

#ifdef DEBUG_CODE
   if( uiDst != pVersion->uiTotal )
   {
      HB_TRACE( HB_TR_ALWAYS, ("ERROR!!! uiDst:%d  pVersion->uiTotal:%d", uiDst, pVersion->uiTotal) ) ;
   }

   for( uiPos = 0; uiPos < pVersion->uiTotal; uiPos++ )
   {
      HB_TRACE( HB_TR_ALWAYS, ("interlaced:%3d %02X", ( int ) s_rev[ ( unsigned char ) pRetBuf[ uiPos ] ], ( int ) s_rev[ ( unsigned char ) pRetBuf[ uiPos ] ]) ) ;
   }
#endif
   return pRet;
}


static int _qr_alphanumeric_no( char ch )
{
   if( '0' <= ch && ch <= '9' )
      return ch - '0';

   if( 'A' <= ch && ch <= 'Z' )
      return ch - 'A' + 10;

   switch( ch )
   {
      case ' ':
         return 36;
      case '$':
         return 37;
      case '%':
         return 38;
      case '*':
         return 39;
      case '+':
         return 40;
      case '-':
         return 41;
      case '.':
         return 42;
      case '/':
         return 43;
      case ':':
         return 44;
   }
   return -1;
}

static int _qr_cci_len( int iVersion, int iMode ) /* Character Count Indicator */
{
   if( iMode == 1 )
      return iVersion <= 9 ? 10 : ( iVersion <= 26 ? 12 : 14 );
   if( iMode == 2 )
      return iVersion <= 9 ? 9 : ( iVersion <= 26 ? 11 : 13 );
   if( iMode == 4 )
      return iVersion <= 9 ? 8 : 16;
   return 0;
}


static int _qr_dataencode( const char * szCode, HB_SIZE nSize, PHB_BITBUFFER pData, int iLevel )
{
   int i, iVersion, iMode, iLen, iDataLen;
   HB_SIZE n;
   char ch;

   /* Select encoding mode */
   iMode = 1;  /* 1=Numeric, 2=Alphanumeric, 4=8-bit, 8=Kanji. Not modes: 0=termibator, 3=Structured append, 7=ECI, 5=FNC1(1), 9=FNC1(2)*/
   for( n = 0; n < nSize; n++ )
   {
      ch = szCode[ n ];
      if( '0' <= ch && ch <= '9' )
      {
      }
      else if( ( 'A' <= ch && ch <= 'Z' ) || 
               ch == ' ' || ch == '$' || ch == '%' || ch == '*' || ch == '+' || ch == '-' || ch == '.' || ch == '/' || ch == ':' )
         iMode = 2;
      else
      {
         iMode = 4;
         break;
      }
   }

   /* Calculate data length in bits */
   if( iMode == 1 )
      iLen = ( nSize / 3 ) * 10 + ( ( nSize % 3 ) == 0 ? 0 : ( ( nSize % 3 ) == 1 ? 4 : 7 ) );
   else if( iMode == 2 )
      iLen = ( nSize / 2 ) * 11 + ( nSize % 2 ) * 6;
   else if( iMode == 4 )
      iLen = nSize * 8;

   iLen += 4; /* Mode indicator length */


   /*  Select version */
   for( i = 1; i <= 40; i++ )
   {
      iDataLen = ( int ) ( s_version[ i - 1 ].uiTotal - s_version[ i - 1 ].level[ iLevel ].uiECC );
      if( iDataLen * 8 >= iLen + _qr_cci_len( i, iMode ) )
      {
         iVersion = i;
         break;
      }
   }
   if( i > 40 )
      return 0; /* Too large */

#ifdef DEBUG_CODE
   HB_TRACE( HB_TR_ALWAYS, ("iMode:%d iLen:%d iDataLen:%d iVersion:%d", iMode, iLen, iDataLen, iVersion) ) ;
#endif

   /* Encode */
   hb_bitbuffer_cat_int_rev( pData, iMode, 4 );
   hb_bitbuffer_cat_int_rev( pData, ( int ) nSize, _qr_cci_len( iVersion, iMode ) );
   if( iMode == 1 )
   {
      for( n = 0; n + 2 < nSize; n += 3 )
      {
         hb_bitbuffer_cat_int_rev( pData, 100 * ( int )( unsigned char ) ( szCode[ n     ] - '0' ) + 
                                           10 * ( int )( unsigned char ) ( szCode[ n + 1 ] - '0' ) + 
                                                ( int )( unsigned char ) ( szCode[ n + 2 ] - '0' ), 10 );
      }
      if( n + 1 == nSize )
         hb_bitbuffer_cat_int_rev( pData, ( int )( unsigned char ) ( szCode[ n ] - '0' ), 4 );
      else
         hb_bitbuffer_cat_int_rev( pData, 10 * ( int )( unsigned char ) ( szCode[ n     ] - '0' ) + 
                                               ( int )( unsigned char ) ( szCode[ n + 1 ] - '0' ), 7 );
   }
   else if( iMode == 2 )
   {
      for( n = 0; n + 1 < nSize; n += 2 )
      {
         hb_bitbuffer_cat_int_rev( pData, 45 * _qr_alphanumeric_no( szCode[ n ] ) + _qr_alphanumeric_no( szCode[ n + 1 ] ), 11 );
      }
      if( n != nSize )
      {
         hb_bitbuffer_cat_int_rev( pData, _qr_alphanumeric_no( szCode[ n ] ), 6 );
      }
   }
   else if( iMode == 4 )
   {
      for( n = 0; n < nSize; n++ )
         hb_bitbuffer_cat_int_rev( pData, ( int ) ( unsigned char ) szCode[ n ], 8 );
   }

   /* Terminator */
   if( iDataLen * 8 >= ( int ) hb_bitbuffer_len( pData ) + 4 )
      hb_bitbuffer_cat_int_rev( pData, 0, 4 );

   /* Padding */
   if( hb_bitbuffer_len( pData ) & 7 )
      hb_bitbuffer_cat_int_rev( pData, 0, 8 - ( hb_bitbuffer_len( pData ) & 7 ) );

   iLen = iDataLen - hb_bitbuffer_len( pData ) / 8;
   for( i = 0; i < iLen; i++ )
   {
      hb_bitbuffer_cat_int_rev( pData, ( i & 1 ) ? 0x11 : 0xEC, 8 );
   }

#ifdef DEBUG_CODE
   for( i = 0; i < iDataLen; i++ )
   {
      HB_TRACE( HB_TR_ALWAYS, ("data:%3d %02X", s_rev[ * (hb_bitbuffer_buffer(pData) + i) ], s_rev[ * (hb_bitbuffer_buffer(pData) + i) ]) );
   }
#endif
   return iVersion;
}


static void _reed_solomon_encode( unsigned char * pData, int iDataLen, unsigned char * pECC, int iECCLen, int * pPoly, int * pExp, int * pLog, int iMod )
{
   int   i, j;
   unsigned char iM;

   for( i = 0; i < iECCLen; i++ )
      pECC[ i ] = 0;

   for( i = 0; i < iDataLen; i++ )
   {
      iM = s_rev[ pData[ i ] ] ^ pECC[ iECCLen - 1 ];
      for( j = iECCLen - 1; j > 0; j-- )
      {
         if( iM && pPoly[ j ] )
            pECC[ j ] = ( unsigned char ) ( pECC[ j - 1 ] ^ pExp[ ( pLog[ iM ] + pLog[ pPoly[ j ] ] ) % iMod ] );
         else
            pECC[ j ] = pECC[ j - 1 ];
      }
      if( iM && pPoly[ 0 ] )
         pECC[ 0 ] = ( unsigned char ) ( pExp[ ( pLog[ iM ] + pLog[ pPoly[ 0 ] ] ) % iMod ] );
      else
         pECC[ 0 ] = 0;
   }
}


static unsigned char * _qr_checksum( PHB_BITBUFFER pData, int iVersion, int iLevel )
{
   const QRVERSION * pVersion = &s_version[ iVersion - 1 ];
   const QRLEVEL * pLevel = &( pVersion->level[ iLevel ] );
   HB_BYTE * pDataBuf = hb_bitbuffer_buffer( pData );
   int * pPoly, * pExp, * pLog;
   int i, j, iBits, iMod, iPoly, iECCLen, iIndex;
   unsigned char * pECC, * pECCPtr, ui, ui2;

   /* Init Galois field. Parameters: iPoly */
   iPoly = 0x11D;

   j = iPoly;
   for( iBits = 0; j > 1; iBits++ )
      j >>= 1;

   iMod = ( 1 << iBits ) - 1;
   pExp = ( int * ) hb_xgrab( sizeof( int ) * iMod );          /* exponent function */
   pLog = ( int * ) hb_xgrab( sizeof( int ) * ( iMod + 1 ) );  /* logarithm function */
   j = 1;
   pLog[ 0 ] = iMod;
   for( i = 0; i < iMod; i++ )
   {
      pExp[ i ] = j;
      pLog[ j ] = i;
      j <<= 1;
      if( j & ( 1 << iBits ) )
         j ^= iPoly;
   }

   /* Init Reed-Solomonn encode. Parameters: iECCLen, iIndex */
   iECCLen = pLevel->block[ 0 ].uiECC;
   iIndex = 0; /* why this parameter is different from DataMatrix ??? */

   pPoly = ( int * ) hb_xgrab( sizeof( int ) * ( iECCLen + 1 ) );
   pPoly[ 0 ] = 1;
   for( i = 1; i <= iECCLen; i++ )
   {
      pPoly[ i ] = 1;
      for( j = i - 1; j > 0; j-- )
      {
         if( pPoly[ j ] )
            pPoly[ j ] = pExp[ ( pLog[ pPoly[ j ] ] + iIndex ) % iMod ];

         pPoly[ j ] ^= pPoly[ j - 1 ];
      }
      pPoly[ 0 ] = pExp[ ( pLog[ pPoly[ 0 ] ] + iIndex ) % iMod ];
      iIndex++;
   }

#ifdef DEBUG_CODE
   for( i = 0; i <= iECCLen; i++ )
   {
      HB_TRACE( HB_TR_ALWAYS, ("POLY[%3d %02X]:%3d %02X", i, i, pPoly[ i ], pPoly[ i ]) ) ;
   }
#endif

   pECC = ( unsigned char * ) hb_xgrab( pLevel->block[ 0 ].uiECC * ( pLevel->block[ 0 ].uiCount + pLevel->block[ 1 ].uiCount ) );
   pECCPtr = pECC;

   /* Divide data into blocks and do Reed-Solomon encoding for each block */
   for( ui = 0; ui < pLevel->block[ 0 ].uiCount; ui++ )
   {
      /* Calculate Reed-Solomon ECC for one block */
      _reed_solomon_encode( pDataBuf, pLevel->block[ 0 ].uiData, pECCPtr, iECCLen, pPoly, pExp, pLog, iMod );
      pDataBuf += pLevel->block[ 0 ].uiData;
      for( i = 0; i < iECCLen / 2; i++ )
      {
        ui2 = pECCPtr[ i ];
        pECCPtr[ i ] = pECCPtr[ iECCLen - 1 - i ];
        pECCPtr[ iECCLen - 1 - i ] = ui2;
      }
      pECCPtr += iECCLen;
   }
   for( ui = 0; ui < pLevel->block[ 1 ].uiCount; ui++ )
   {
      /* Calculate Reed-Solomon ECC for one block */
      _reed_solomon_encode( pDataBuf, pLevel->block[ 1 ].uiData, pECCPtr, iECCLen, pPoly, pExp, pLog, iMod );
      pDataBuf += pLevel->block[ 1 ].uiData;
      for( i = 0; i < iECCLen / 2; i++ )
      {
        ui2 = pECCPtr[ i ];
        pECCPtr[ i ] = pECCPtr[ iECCLen - 1 - i ];
        pECCPtr[ iECCLen - 1 - i ] = ui2;
      }
      pECCPtr += iECCLen;
   }

   hb_xfree( pExp );
   hb_xfree( pLog );
   hb_xfree( pPoly );

#ifdef DEBUG_CODE
   iECCLen = pLevel->block[ 0 ].uiECC * ( pLevel->block[ 0 ].uiCount + pLevel->block[ 1 ].uiCount );
   for( i = 0; i < iECCLen; i++ )
   {
      HB_TRACE( HB_TR_ALWAYS, ("ecc:%3d %02X", ( int )( unsigned char ) pECC[ i ], ( int )( unsigned char ) pECC[ i ]) ) ;
   }
#endif
   return pECC;
}


static void _qr_draw( PHB_BITBUFFER pBits, PHB_BITBUFFER pCWBits, int iVersion )
{
   int i, j, no, up, right, iLength;
   unsigned char *pi, *pj;

   HB_SYMBOL_UNUSED( pCWBits );

   iLength = _qr_versionlength( iVersion );

   /* draw position detection markers */
   for( i = 0; i < 7; i++ )
   {
      hb_bitbuffer_set( pBits, i, 1 );
      hb_bitbuffer_set( pBits, i + 6 * iLength, 1 );
      hb_bitbuffer_set( pBits, i * iLength, 1 );
      hb_bitbuffer_set( pBits, i * iLength + 6, 1 );

      hb_bitbuffer_set( pBits, i + ( iLength - 7 ) * iLength, 1 );
      hb_bitbuffer_set( pBits, i + ( iLength - 1 ) * iLength, 1 );
      hb_bitbuffer_set( pBits, ( iLength - 7 + i ) * iLength, 1 );
      hb_bitbuffer_set( pBits, ( iLength - 7 + i ) * iLength + 6, 1 );

      hb_bitbuffer_set( pBits, iLength - 7 + i, 1 );
      hb_bitbuffer_set( pBits, iLength - 7 + i + 6 * iLength, 1 );
      hb_bitbuffer_set( pBits, iLength - 7 + i * iLength, 1 );
      hb_bitbuffer_set( pBits, iLength - 1 + i * iLength, 1 );
   }
   for( i = 2; i < 5; i++ )
   {
      for( j = 2; j < 5; j++ )
      {
         hb_bitbuffer_set( pBits, i * iLength + j, 1 );
         hb_bitbuffer_set( pBits, i * iLength + j + iLength - 7, 1 );
         hb_bitbuffer_set( pBits, ( i + iLength - 7 ) * iLength + j, 1 );
      }
   }
   /* draw timing patterns */
   for( i = 8; i < iLength - 8; i += 2 )
   {
      hb_bitbuffer_set( pBits, i + 6 * iLength, 1 );
      hb_bitbuffer_set( pBits, i * iLength + 6, 1 );
   }
   /* draw alignment patterns */
   pi = s_align[ iVersion - 1 ];
   for( ; *pi; pi++ )
   {
      pj = s_align[ iVersion - 1 ];
      for( ; *pj; pj++ )
      {
         if( ( *pi > 10 && *pi < iLength - 10 ) || 
             ( *pj > 10 && *pj < iLength - 10 ) || 
             ( *pi > 10 && *pj > 10 ))
         {
            hb_bitbuffer_set( pBits, iLength * *pi + *pj - 1, 0 );
            hb_bitbuffer_set( pBits, iLength * *pi + *pj + 1, 0 );
            hb_bitbuffer_set( pBits, iLength * ( *pi - 1 ) + *pj, 0 );
            hb_bitbuffer_set( pBits, iLength * ( *pi + 1 ) + *pj, 0 );

            hb_bitbuffer_set( pBits, iLength * *pi + *pj, 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi - 2 ) + *pj - 2, 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi - 2 ) + *pj - 1, 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi - 2 ) + *pj    , 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi - 2 ) + *pj + 1, 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi - 2 ) + *pj + 2, 1 );
            hb_bitbuffer_set( pBits, ( *pi - 1 ) * iLength + *pj - 2, 1 );
            hb_bitbuffer_set( pBits, *pi * iLength + *pj - 2, 1 );
            hb_bitbuffer_set( pBits, ( *pi + 1 ) * iLength + *pj - 2, 1 );
            hb_bitbuffer_set( pBits, ( *pi - 1 ) * iLength + *pj + 2, 1 );
            hb_bitbuffer_set( pBits, *pi * iLength + *pj + 2, 1 );
            hb_bitbuffer_set( pBits, ( *pi + 1 ) * iLength + *pj + 2, 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi + 2 ) + *pj - 2, 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi + 2 ) + *pj - 1, 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi + 2 ) + *pj    , 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi + 2 ) + *pj + 1, 1 );
            hb_bitbuffer_set( pBits, iLength * ( *pi + 2 ) + *pj + 2, 1 );
         }
      }
   }

   /* Dark module */
   hb_bitbuffer_set( pBits, 8 + iLength * ( iLength - 8 ), 1 );


   /* Draw data. Note: pCWBits == NULL is used only for debugging */
   if( pCWBits ) 
   {
      i = j = iLength - 1;
      right = 1;
      up = 1;
      no = 0;
      while( i >= 0 && j >= 0 )
      {
         if( ! _qr_fixed( iVersion, i, j ) )
            hb_bitbuffer_set( pBits, i * iLength + j, hb_bitbuffer_get( pCWBits, no++ ) );
    
         if( right )
         {
            j--;
         }
         else
         {
            if( up )
            {
               if( i > 0 )
               {
                  i--;
                  j++;
               }
               else
               {
                  up = 0;
                  j--;
               }
            }
            else
            {
               if( i < iLength - 1 )
               {
                  i++;
                  j++;
               }
               else
               {
                  up = 1;
                  j--;
               }
            }
         }
         right = ! right;
         if( j == 6 )
            j--;
      }
   }
}


static int _qr_penalty( PHB_BITBUFFER pBits, int iVersion )
{
   int i, j, k, iPenalty = 0, iLen = _qr_versionlength( iVersion );
   HB_BOOL bBitLast, bBit;

   /* 1. Same color modules in row/column */
   for( i = 0; i < iLen; i++ )
   {
      /* Row */
      bBitLast = hb_bitbuffer_get( pBits, i * iLen );
      k = 1;
      for( j = 1; j < iLen; j++ )
      {
         bBit = hb_bitbuffer_get( pBits, i * iLen + j );
         if( bBit != bBitLast )
         {
            if( k >= 5 )
               iPenalty += 3 + ( k - 5 );

            bBitLast = bBit;
            k = 1;
         }
         else
            k++;
      }
      if( k >= 5 )
         iPenalty = 3 + ( k - 5 );

      /* Col */
      bBitLast = hb_bitbuffer_get( pBits, i );
      k = 1;
      for( j = 1; j < iLen; j++ )
      {
         bBit = hb_bitbuffer_get( pBits, i + iLen * j );
         if( bBit != bBitLast )
         {
            if( k >= 5 )
               iPenalty += 3 + ( k - 5 );

            bBitLast = bBit;
            k = 1;
         }
         else
            k++;
      }
      if( k >= 5 )
         iPenalty = 3 + ( k - 5 );
   }

   /* 2. Block of same color modules */
   /* Instead of looking for non-overlapped MxN block, we can search for 2x2 overlapping blocks, */
   /* penalty value is the same for both of these methods                                        */
   for( i = 0; i < iLen - 1; i++ )
   {
      for( j = 0; j < iLen - 1; j++ )
      {
         bBit = hb_bitbuffer_get( pBits, i * iLen + j );
         if( hb_bitbuffer_get( pBits, i * iLen + j + 1 ) == bBit && 
             hb_bitbuffer_get( pBits, ( i + 1 ) * iLen + j ) == bBit && 
             hb_bitbuffer_get( pBits, ( i + 1 ) * iLen + j + 1 ) == bBit )
            iPenalty += 3;
      }
   }

   /* 3. Black-White-Black-Black-Black-White-Black modules */
   for( i = 0; i < iLen - 6; i++ )
   {
      for( j = 0; j < iLen - 6; j++ )
      {
         if( hb_bitbuffer_get( pBits, i * iLen + j     ) == HB_TRUE && 
             hb_bitbuffer_get( pBits, i * iLen + j + 1 ) == HB_FALSE && 
             hb_bitbuffer_get( pBits, i * iLen + j + 2 ) == HB_TRUE && 
             hb_bitbuffer_get( pBits, i * iLen + j + 3 ) == HB_TRUE && 
             hb_bitbuffer_get( pBits, i * iLen + j + 4 ) == HB_TRUE && 
             hb_bitbuffer_get( pBits, i * iLen + j + 5 ) == HB_FALSE && 
             hb_bitbuffer_get( pBits, i * iLen + j + 6 ) == HB_TRUE )
            iPenalty += 40;

         if( hb_bitbuffer_get( pBits, i         * iLen + j ) == HB_TRUE && 
             hb_bitbuffer_get( pBits, ( i + 1 ) * iLen + j ) == HB_FALSE && 
             hb_bitbuffer_get( pBits, ( i + 2 ) * iLen + j ) == HB_TRUE && 
             hb_bitbuffer_get( pBits, ( i + 3 ) * iLen + j ) == HB_TRUE && 
             hb_bitbuffer_get( pBits, ( i + 4 ) * iLen + j ) == HB_TRUE && 
             hb_bitbuffer_get( pBits, ( i + 5 ) * iLen + j ) == HB_FALSE && 
             hb_bitbuffer_get( pBits, ( i + 6 ) * iLen + j ) == HB_TRUE )
            iPenalty += 40;
      }
   }

   /* 4. Proportion of color modules */
   k = 0;
   for( i = 0; i < iLen; i++ )
      for( j = 0; j < iLen; j++ )
         if( hb_bitbuffer_get( pBits, i * iLen + j ) )
            k++;
   k = k * 100 / ( iLen * iLen );
   if( k < 50 )
      k = 100 - k;
   iPenalty += ( k / 5 ) * 10;

   return iPenalty;
}


static void _qr_mask_pattern( PHB_BITBUFFER pBits, int iVersion, int iMask )
{
   int i, j, k, iLen = _qr_versionlength( iVersion );

   for( i = 0; i < iLen; i++ )
   {
      for( j = 0; j < iLen; j++ )
      {
         if( ! _qr_fixed( iVersion, i, j ) )
         {
            switch( iMask )
            {
               case 0:
                  k = ( i + j ) % 2 == 0;
                  break;
               case 1:
                  k = i % 2 == 0;
                  break;
               case 2:
                  k = j % 3 == 0;
                  break;
               case 3:
                  k = ( i + j ) % 3 == 0;
                  break;
               case 4:
                  k = ( i / 2 + j / 3 ) % 2 == 0;
                  break;
               case 5:
                  k = ( i * j ) % 2 + ( i * j ) % 3 == 0;
                  break;
               case 6:
                  k = ( ( i * j ) % 2 + ( i * j ) % 3 ) % 2 == 0;
                  break;
               case 7:
                  k = ( ( i * j ) % 3 + ( i + j ) % 2 ) % 2 == 0;
                  break;
            }
            if( k ) 
               hb_bitbuffer_not( pBits, i * iLen + j );
         }
      }
   }
}

             
static int _qr_mask( PHB_BITBUFFER pBits, int iVersion )
{
   int i, iPenaltyMin = 0, iMaskMin, iPenalty;

   for( i = 0; i < 8; i++ )
   {
      _qr_mask_pattern( pBits, iVersion, i );
      iPenalty = _qr_penalty( pBits, iVersion );
#ifdef DEBUG_CODE
      HB_TRACE( HB_TR_ALWAYS, ("mask:%d penalty:%d", i, iPenalty) );
#endif
      if( i == 0 || iPenalty < iPenaltyMin )
      {
         iPenaltyMin = iPenalty;
         iMaskMin = i;
      }
      _qr_mask_pattern( pBits, iVersion, i );
   }
#ifdef DEBUG_CODE
   HB_TRACE( HB_TR_ALWAYS, ("mask:%d", iMaskMin) );
//   iMaskMin = 0;
   HB_TRACE( HB_TR_ALWAYS, ("mask applied:%d", iMaskMin) );
#endif
   _qr_mask_pattern( pBits, iVersion, iMaskMin );
   return iMaskMin;
}


static void _qr_draw_version_format( PHB_BITBUFFER pBits, int iVersion, int iLevel, int iMask )
{
   int i, iCRC, iLen = _qr_versionlength( iVersion );

   if( iVersion >= 7 )
   {
      iCRC = _qr_version_crc( iVersion );
      for( i = 0; i < 18; i++ )
      {
         if( iCRC & ( 1 << 17 ) )
         {
            hb_bitbuffer_set( pBits, iLen - 11 + ( i % 3 ) + iLen * ( i / 3 ), 1 );
            hb_bitbuffer_set( pBits, ( iLen - 11 + ( i % 3 ) ) * iLen + ( i / 3 ), 1 );
         }
         iCRC <<= 1;
      }
   }
   iCRC = _qr_format_crc( iLevel, iMask );
   for( i = 0; i < 15; i++ )
   {
      if( iCRC & ( 1 << 14 ) )
      {
         /* Top left */
         if( i <= 7 )
            hb_bitbuffer_set( pBits, ( i + ( i >= 6 ? 1 : 0 ) ) * iLen + 8, 1 );
         else
            hb_bitbuffer_set( pBits, ( 14 - i ) + ( i == 8 ? 1 : 0 ) + iLen * 8, 1 );

         /* Top right and bottom left */
         if( i <= 7 )
            hb_bitbuffer_set( pBits, iLen - 1 - i + iLen * 8, 1 );
         else
            hb_bitbuffer_set( pBits, ( iLen - 15 + i ) * iLen + 8, 1 );
      }
      iCRC <<= 1;
   }

#if 0
   /* _qr_fixed() test code */
   for( i = 0; i < iLen; i++ )
   {
     int j;
     for( j = 0; j < iLen; j++ )
        hb_bitbuffer_set( pBits, i * iLen + j, _qr_fixed( iVersion, i, j ) );
   }
#endif
}


PHB_ZEBRA hb_zebra_create_qrcode( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA       pZebra;
   PHB_BITBUFFER   pData, pFinal;
   unsigned char * pECC;
   int             iVersion, iLevel, iMask;

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_QRCODE;

   if( nLen > 7089 )
   {
      pZebra->iError = HB_ZEBRA_ERROR_TOOLARGE;
      return pZebra;
   }

   switch( iFlags & HB_ZEBRA_FLAG_QR_LEVEL_MASK )
   {
      case HB_ZEBRA_FLAG_QR_LEVEL_M:
         iLevel = 1;
         break;
      case HB_ZEBRA_FLAG_QR_LEVEL_Q:
         iLevel = 2;
         break;
      case HB_ZEBRA_FLAG_QR_LEVEL_H:
         iLevel = 3;
         break;
      default:
         iLevel = 0;
         break;
   }

#ifdef DEBUG_CODE
   HB_TRACE( HB_TR_ALWAYS, ("qr1 iLevel:%d", iLevel) );
#endif

   pData = hb_bitbuffer_create();
   iVersion = _qr_dataencode( szCode, nLen, pData, iLevel );

#ifdef DEBUG_CODE
   HB_TRACE( HB_TR_ALWAYS, ("qr3 iVersion:%d", iVersion) );
#endif

   if( iVersion == 0 )
   {
      hb_bitbuffer_destroy( pData );
      pZebra->iError = HB_ZEBRA_ERROR_TOOLARGE;
      return pZebra;
   }

   pZebra->iCol = _qr_versionlength( iVersion );

   pZebra->szCode = hb_strdup( szCode );

   pECC = _qr_checksum( pData, iVersion, iLevel );

   pFinal = _qr_interlace( pData, pECC, iVersion, iLevel );
   hb_bitbuffer_destroy( pData );
   hb_xfree( pECC );

   pZebra->pBits = hb_bitbuffer_create();
   _qr_draw( pZebra->pBits, pFinal, iVersion );
   hb_bitbuffer_destroy( pFinal );

   iMask = _qr_mask( pZebra->pBits, iVersion );

   _qr_draw_version_format(pZebra->pBits, iVersion, iLevel, iMask );
   return pZebra;
}


HB_FUNC( HB_ZEBRA_CREATE_QRCODE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_qrcode( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
