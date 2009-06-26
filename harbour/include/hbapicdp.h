/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.harbour-project.org
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

#ifndef HB_APICDP_H_
#define HB_APICDP_H_

#ifndef HB_CDP_SUPPORT_OFF

#include "hbapi.h"
#include "hbinit.h"

HB_EXTERN_BEGIN

/* This hack is needed to force preprocessing if id is also a macro */
#define HB_CODEPAGE_REQUEST( id )      HB_CODEPAGE_REQUEST_( id )
#define HB_CODEPAGE_REQUEST_( id )     HB_FUNC_EXTERN( HB_CODEPAGE_##id ); \
                                       void hb_codepage_ForceLink_##id( void ) \
                                       { \
                                          HB_FUNC_EXEC( HB_CODEPAGE_##id ); \
                                       }
#define HB_CODEPAGE_ANNOUNCE( id )     HB_FUNC( HB_CODEPAGE_##id ) {}

#define HB_CODEPAGE_INIT( id )         HB_CODEPAGE_ANNOUNCE( id ) \
                                       HB_CALL_ON_STARTUP_BEGIN( hb_codepage_Init_##id ) \
                                       hb_cdpRegister( &s_codepage ); \
                                       HB_CALL_ON_STARTUP_END( hb_codepage_Init_##id )


typedef struct _HB_UNITABLE
{
   const char *   uniID;
   int            nChars;
   BOOL           lMulti;
   USHORT *       uniCodes;
} HB_UNITABLE, * PHB_UNITABLE;

typedef struct _HB_MULTICHAR
{
   char  cLast[2];
   char  cFirst[2];
   int   nCode;
} HB_MULTICHAR, * PHB_MULTICHAR;

typedef struct _HB_CODEPAGE
{
   const char *   id;
   const char *   uniID;
   PHB_UNITABLE   uniTable;
   int            nChars;
   const char *   CharsUpper;
   const char *   CharsLower;
   BOOL           lLatin;
   BOOL           lAccEqual;
   BOOL           lAccInterleave;
   BOOL           lSort;
   BOOL           lChClone;
   BYTE *         s_chars;
   BYTE *         s_upper;
   BYTE *         s_lower;
   BYTE *         s_accent;
   int            nMulti;
   PHB_MULTICHAR  multi;
} HB_CODEPAGE, * PHB_CODEPAGE;

#define HB_CPID_437        "cp437"
#define HB_CPID_737        "cp737"
#define HB_CPID_775        "cp775"
#define HB_CPID_850        "cp850"
#define HB_CPID_852        "cp852"
#define HB_CPID_855        "cp855"
#define HB_CPID_857        "cp857"
#define HB_CPID_860        "cp860"
#define HB_CPID_861        "cp861"
#define HB_CPID_862        "cp862"
#define HB_CPID_863        "cp863"
#define HB_CPID_864        "cp864"
#define HB_CPID_865        "cp865"
#define HB_CPID_866        "cp866"
#define HB_CPID_869        "cp869"
#define HB_CPID_874        "cp874"
#define HB_CPID_1250       "cp1250"
#define HB_CPID_1251       "cp1251"
#define HB_CPID_1252       "cp1252"
#define HB_CPID_1253       "cp1253"
#define HB_CPID_1254       "cp1254"
#define HB_CPID_1255       "cp1255"
#define HB_CPID_1256       "cp1256"
#define HB_CPID_1257       "cp1257"
#define HB_CPID_1258       "cp1258"
#define HB_CPID_8859_1     "iso8859-1"
#define HB_CPID_8859_1B    "iso8859-1b"
#define HB_CPID_8859_2     "iso8859-2"
#define HB_CPID_8859_3     "iso8859-3"
#define HB_CPID_8859_4     "iso8859-4"
#define HB_CPID_8859_5     "iso8859-5"
#define HB_CPID_8859_6     "iso8859-6"
#define HB_CPID_8859_7     "iso8859-7"
#define HB_CPID_8859_8     "iso8859-8"
#define HB_CPID_8859_9     "iso8859-9"
#define HB_CPID_8859_10    "iso8859-10"
#define HB_CPID_8859_11    "iso8859-11"
#define HB_CPID_8859_13    "iso8859-13"
#define HB_CPID_8859_14    "iso8859-14"
#define HB_CPID_8859_15    "iso8859-15"
#define HB_CPID_8859_16    "iso8859-16"
#define HB_CPID_KOI_8      "koi-8"
#define HB_CPID_KOI_8U     "koi-8u"
#define HB_CPID_KAM        "kamenicky"
#define HB_CPID_MAZ        "plmaz"
#define HB_CPID_MIK        "bg-mik"
#define HB_CPID_037        "cp037"
#define HB_CPID_424        "cp424"
#define HB_CPID_500        "cp500"
#define HB_CPID_856        "cp856"
#define HB_CPID_875        "cp875"
#define HB_CPID_1006       "cp1006"
#define HB_CPID_1026       "cp1026"
#define HB_CPID_10000      "macroman"
#define HB_CPID_10006      "macgreek"
#define HB_CPID_10007      "maccyrillic"
#define HB_CPID_10029      "maccentraleurope"
#define HB_CPID_10079      "maciceland"
#define HB_CPID_10081      "macturkish"
#define HB_CPID_ATARIST    "atarist"
#define HB_CPID_NEXTSTEP   "nextstep"
#define HB_CPID_USASCII    "us-ascii"

#define HB_UNITB_437       &hb_uniTbl_437
#define HB_UNITB_737       &hb_uniTbl_737
#define HB_UNITB_775       &hb_uniTbl_775
#define HB_UNITB_850       &hb_uniTbl_850
#define HB_UNITB_852       &hb_uniTbl_852
#define HB_UNITB_855       &hb_uniTbl_855
#define HB_UNITB_857       &hb_uniTbl_857
#define HB_UNITB_860       &hb_uniTbl_860
#define HB_UNITB_861       &hb_uniTbl_861
#define HB_UNITB_862       &hb_uniTbl_862
#define HB_UNITB_863       &hb_uniTbl_863
#define HB_UNITB_864       &hb_uniTbl_864
#define HB_UNITB_865       &hb_uniTbl_865
#define HB_UNITB_866       &hb_uniTbl_866
#define HB_UNITB_869       &hb_uniTbl_869
#define HB_UNITB_874       &hb_uniTbl_874
#define HB_UNITB_1250      &hb_uniTbl_1250
#define HB_UNITB_1251      &hb_uniTbl_1251
#define HB_UNITB_1252      &hb_uniTbl_1252
#define HB_UNITB_1253      &hb_uniTbl_1253
#define HB_UNITB_1254      &hb_uniTbl_1254
#define HB_UNITB_1255      &hb_uniTbl_1255
#define HB_UNITB_1256      &hb_uniTbl_1256
#define HB_UNITB_1257      &hb_uniTbl_1257
#define HB_UNITB_1258      &hb_uniTbl_1258
#define HB_UNITB_8859_1    &hb_uniTbl_8859_1
#define HB_UNITB_8859_1B   &hb_uniTbl_8859_1b
#define HB_UNITB_8859_2    &hb_uniTbl_8859_2
#define HB_UNITB_8859_3    &hb_uniTbl_8859_3
#define HB_UNITB_8859_4    &hb_uniTbl_8859_4
#define HB_UNITB_8859_5    &hb_uniTbl_8859_5
#define HB_UNITB_8859_6    &hb_uniTbl_8859_6
#define HB_UNITB_8859_7    &hb_uniTbl_8859_7
#define HB_UNITB_8859_8    &hb_uniTbl_8859_8
#define HB_UNITB_8859_9    &hb_uniTbl_8859_9
#define HB_UNITB_8859_10   &hb_uniTbl_8859_10
#define HB_UNITB_8859_11   &hb_uniTbl_8859_11
#define HB_UNITB_8859_13   &hb_uniTbl_8859_13
#define HB_UNITB_8859_14   &hb_uniTbl_8859_14
#define HB_UNITB_8859_15   &hb_uniTbl_8859_15
#define HB_UNITB_8859_16   &hb_uniTbl_8859_16
#define HB_UNITB_KOI_8     &hb_uniTbl_KOI_8
#define HB_UNITB_KOI_8U    &hb_uniTbl_KOI_8U
#define HB_UNITB_KAM       &hb_uniTbl_kamenicky
#define HB_UNITB_MAZ       &hb_uniTbl_mazovia
#define HB_UNITB_MIK       &hb_uniTbl_MIK
#define HB_UNITB_037       &hb_uniTbl_037
#define HB_UNITB_424       &hb_uniTbl_424
#define HB_UNITB_500       &hb_uniTbl_500
#define HB_UNITB_856       &hb_uniTbl_856
#define HB_UNITB_875       &hb_uniTbl_875
#define HB_UNITB_1006      &hb_uniTbl_1006
#define HB_UNITB_1026      &hb_uniTbl_1026
#define HB_UNITB_10000     &hb_uniTbl_10000
#define HB_UNITB_10006     &hb_uniTbl_10006
#define HB_UNITB_10007     &hb_uniTbl_10007
#define HB_UNITB_10029     &hb_uniTbl_10029
#define HB_UNITB_10079     &hb_uniTbl_10079
#define HB_UNITB_10081     &hb_uniTbl_10081
#define HB_UNITB_ATARIST   &hb_uniTbl_ATARIST
#define HB_UNITB_NEXTSTEP  &hb_uniTbl_NEXTSTEP
#define HB_UNITB_USASCII   &hb_uniTbl_USASCII
#define HB_UNITB_UNDEF     NULL /* ((PHB_UNITABLE) (-1)) */

extern HB_UNITABLE hb_uniTbl_437;
extern HB_UNITABLE hb_uniTbl_737;
extern HB_UNITABLE hb_uniTbl_775;
extern HB_UNITABLE hb_uniTbl_850;
extern HB_UNITABLE hb_uniTbl_852;
extern HB_UNITABLE hb_uniTbl_855;
extern HB_UNITABLE hb_uniTbl_857;
extern HB_UNITABLE hb_uniTbl_860;
extern HB_UNITABLE hb_uniTbl_861;
extern HB_UNITABLE hb_uniTbl_862;
extern HB_UNITABLE hb_uniTbl_863;
extern HB_UNITABLE hb_uniTbl_864;
extern HB_UNITABLE hb_uniTbl_865;
extern HB_UNITABLE hb_uniTbl_866;
extern HB_UNITABLE hb_uniTbl_869;
extern HB_UNITABLE hb_uniTbl_874;
extern HB_UNITABLE hb_uniTbl_1250;
extern HB_UNITABLE hb_uniTbl_1251;
extern HB_UNITABLE hb_uniTbl_1252;
extern HB_UNITABLE hb_uniTbl_1253;
extern HB_UNITABLE hb_uniTbl_1254;
extern HB_UNITABLE hb_uniTbl_1255;
extern HB_UNITABLE hb_uniTbl_1256;
extern HB_UNITABLE hb_uniTbl_1257;
extern HB_UNITABLE hb_uniTbl_1258;
extern HB_UNITABLE hb_uniTbl_8859_1;
extern HB_UNITABLE hb_uniTbl_8859_1b;
extern HB_UNITABLE hb_uniTbl_8859_2;
extern HB_UNITABLE hb_uniTbl_8859_3;
extern HB_UNITABLE hb_uniTbl_8859_4;
extern HB_UNITABLE hb_uniTbl_8859_5;
extern HB_UNITABLE hb_uniTbl_8859_6;
extern HB_UNITABLE hb_uniTbl_8859_7;
extern HB_UNITABLE hb_uniTbl_8859_8;
extern HB_UNITABLE hb_uniTbl_8859_9;
extern HB_UNITABLE hb_uniTbl_8859_10;
extern HB_UNITABLE hb_uniTbl_8859_11;
extern HB_UNITABLE hb_uniTbl_8859_13;
extern HB_UNITABLE hb_uniTbl_8859_14;
extern HB_UNITABLE hb_uniTbl_8859_15;
extern HB_UNITABLE hb_uniTbl_8859_16;
extern HB_UNITABLE hb_uniTbl_KOI_8;
extern HB_UNITABLE hb_uniTbl_KOI_8U;
extern HB_UNITABLE hb_uniTbl_kamenicky;
extern HB_UNITABLE hb_uniTbl_mazovia;
extern HB_UNITABLE hb_uniTbl_MIK;
extern HB_UNITABLE hb_uniTbl_037;
extern HB_UNITABLE hb_uniTbl_424;
extern HB_UNITABLE hb_uniTbl_500;
extern HB_UNITABLE hb_uniTbl_856;
extern HB_UNITABLE hb_uniTbl_875;
extern HB_UNITABLE hb_uniTbl_1006;
extern HB_UNITABLE hb_uniTbl_1026;
extern HB_UNITABLE hb_uniTbl_10000;
extern HB_UNITABLE hb_uniTbl_10006;
extern HB_UNITABLE hb_uniTbl_10007;
extern HB_UNITABLE hb_uniTbl_10029;
extern HB_UNITABLE hb_uniTbl_10079;
extern HB_UNITABLE hb_uniTbl_10081;
extern HB_UNITABLE hb_uniTbl_ATARIST;
extern HB_UNITABLE hb_uniTbl_NEXTSTEP;
extern HB_UNITABLE hb_uniTbl_USASCII;

extern HB_EXPORT PHB_CODEPAGE  hb_vmCDP( void );
extern HB_EXPORT void          hb_vmSetCDP( PHB_CODEPAGE pCDP );

extern HB_EXPORT BOOL          hb_cdpRegister( PHB_CODEPAGE );
extern HB_EXPORT const char *  hb_cdpSelectID( const char * );
extern HB_EXPORT const char *  hb_cdpID( void );
extern HB_EXPORT PHB_CODEPAGE  hb_cdpSelect( PHB_CODEPAGE );
extern HB_EXPORT PHB_CODEPAGE  hb_cdpFind( const char * );
extern HB_EXPORT void          hb_cdpTranslate( char *, PHB_CODEPAGE, PHB_CODEPAGE );
extern HB_EXPORT void          hb_cdpnTranslate( char *, PHB_CODEPAGE, PHB_CODEPAGE, ULONG );
extern HB_EXPORT int           hb_cdpcmp( const char *, ULONG, const char *, ULONG, PHB_CODEPAGE, BOOL );
extern HB_EXPORT int           hb_cdpicmp( const char *, ULONG, const char *, ULONG, PHB_CODEPAGE, BOOL );
extern HB_EXPORT int           hb_cdpchrcmp( char, char, PHB_CODEPAGE );
extern HB_EXPORT void          hb_cdpReleaseAll( void );

extern HB_EXPORT USHORT        hb_cdpGetU16( PHB_CODEPAGE, BOOL, UCHAR );
extern HB_EXPORT UCHAR         hb_cdpGetChar( PHB_CODEPAGE, BOOL, USHORT );
extern HB_EXPORT BOOL          hb_cdpGetFromUTF8( PHB_CODEPAGE, BOOL, UCHAR, int *, USHORT * );
extern HB_EXPORT ULONG         hb_cdpStrnToUTF8( PHB_CODEPAGE, BOOL, const char *, ULONG, char * );
extern HB_EXPORT ULONG         hb_cdpStrnToU16( PHB_CODEPAGE, BOOL, const char *, ULONG, char * );
extern HB_EXPORT ULONG         hb_cdpStringInUTF8Length( PHB_CODEPAGE, BOOL, const char *, ULONG );
extern HB_EXPORT ULONG         hb_cdpUTF8ToStrn( PHB_CODEPAGE, BOOL, const char *, ULONG, char *, ULONG );
extern HB_EXPORT ULONG         hb_cdpUTF8StringLength( const char *, ULONG );
extern HB_EXPORT char *        hb_cdpUTF8StringSubstr( const char *, ULONG, ULONG, ULONG, ULONG * );
extern HB_EXPORT ULONG         hb_cdpUTF8StringPeek( const char *, ULONG, ULONG );

HB_EXTERN_END

#else

typedef void * PHB_CODEPAGE;

#endif /* HB_CDP_SUPPORT_OFF */

#endif /* HB_APICDP_H_ */
