/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_APICDP_H_
#define HB_APICDP_H_

#include "hbapi.h"
#include "hbinit.h"

HB_EXTERN_BEGIN

/* This hack is needed to force preprocessing if id is also a macro */
#define HB_CODEPAGE_REQUEST( id )      HB_CODEPAGE_REQUEST_( id )
#define HB_CODEPAGE_REQUEST_( id )     HB_FUNC_EXTERN( HB_CODEPAGE_##id ); \
                                       extern void hb_codepage_ForceLink_##id( void ); \
                                       void hb_codepage_ForceLink_##id( void ) \
                                       { \
                                          HB_FUNC_EXEC( HB_CODEPAGE_##id ); \
                                       }
#define HB_CODEPAGE_ANNOUNCE( id )     HB_CODEPAGE_ANNOUNCE_( id )
#define HB_CODEPAGE_ANNOUNCE_( id )    HB_FUNC( HB_CODEPAGE_##id ) {}


#if defined( HB_OS_WIN )
   typedef wchar_t         HB_WCHAR;
#else
   typedef unsigned short  HB_WCHAR;
#endif


/* forward declaration */
struct _HB_CODEPAGE;

#define HB_CODEPAGE_PTR       struct _HB_CODEPAGE *

#define HB_CDPCHAR_GET( c, s, n, i, w )   (c)->wcharGet( c, s, n, i, w )
#define HB_CDPCHAR_PUT( c, s, n, i, w )   (c)->wcharPut( c, s, n, i, w )
#define HB_CDPCHAR_LEN( c, w )            (c)->wcharLen( c, w )

#define HB_CDP_GET_FUNC( func ) HB_BOOL func( HB_CODEPAGE_PTR cdp, const char * pSrc, HB_SIZE nLen, HB_SIZE * pnIndex, HB_WCHAR * wc )
typedef HB_CDP_GET_FUNC( ( * PHB_CDP_GET_FUNC ) );

#define HB_CDP_PUT_FUNC( func ) HB_BOOL func( HB_CODEPAGE_PTR cdp, char * pDst, HB_SIZE nLen, HB_SIZE * pnIndex, HB_WCHAR wc )
typedef HB_CDP_PUT_FUNC( ( * PHB_CDP_PUT_FUNC ) );

#define HB_CDP_LEN_FUNC( func ) int func( HB_CODEPAGE_PTR cdp, HB_WCHAR wc )
typedef HB_CDP_LEN_FUNC( ( * PHB_CDP_LEN_FUNC ) );


typedef struct _HB_UNITABLE
{
   const char *      uniID;
   const HB_WCHAR *  uniCodes;
   HB_UCHAR *        uniTrans;
   HB_WCHAR          wcMax;
} HB_UNITABLE, * PHB_UNITABLE;

typedef struct _HB_MULTICHAR
{
   char     cFirst[ 2 ];
   char     cLast[ 2 ];
   int      sortUp;
   int      sortLo;
   HB_WCHAR wcUp;
   HB_WCHAR wcLo;
} HB_MULTICHAR, * PHB_MULTICHAR;

typedef struct _HB_CODEPAGE
{
   const char *            id;
   const char *            info;
   PHB_UNITABLE            uniTable;
   const HB_UCHAR *        flags;
   const HB_UCHAR *        upper;
   const HB_UCHAR *        lower;
   const HB_UCHAR *        sort;
   const HB_UCHAR *        acc;
   int                     nACSort;
   HB_BOOL                 fCustom;
   PHB_CDP_GET_FUNC        wcharGet;
   PHB_CDP_PUT_FUNC        wcharPut;
   PHB_CDP_LEN_FUNC        wcharLen;
   int                     nMulti;
   int                     nMultiUC;
   PHB_MULTICHAR           multi;
   void *                  buffer;
   struct _HB_CODEPAGE *   next;
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
#define HB_CPID_1125       "cp1125"
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
#define HB_CPID_MAZ        "mazovia"
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
#define HB_CPID_646YU      "iso646-yu"

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
#define HB_UNITB_1125      &hb_uniTbl_1125
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
#define HB_UNITB_646YU     &hb_uniTbl_646YU
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
extern HB_UNITABLE hb_uniTbl_1125;
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
extern HB_UNITABLE hb_uniTbl_646YU;

extern HB_EXPORT PHB_CODEPAGE hb_vmCDP( void );
extern HB_EXPORT void         hb_vmSetCDP( PHB_CODEPAGE pCDP );


/* character flags */
#define HB_CDP_DIGIT    0x01
#define HB_CDP_ALPHA    0x02
#define HB_CDP_LOWER    0x04
#define HB_CDP_UPPER    0x08
#define HB_CDP_MULTI1   0x10
#define HB_CDP_MULTI2   0x20

/* accented character sorting */
#define HB_CDP_ACSORT_NONE          0     /* no special sorting for accented
                                             characters */
#define HB_CDP_ACSORT_EQUAL         1     /* accented characters have the same
                                             weight as corresponding unaccented
                                             ones */
#define HB_CDP_ACSORT_INTERLEAVED   2     /* accented characters sort after
                                             their unaccented counterparts only
                                             if the unaccented versions of all
                                             characters being compared are the
                                             same ( interleaving ) */

/* letter case sensitive sorting */
#define HB_CDP_CSSORT_UPLO          0     /* upper letters first then lower
                                             ones */
#define HB_CDP_CSSORT_MIXED         1     /* upper and lower letters are
                                             mixed */
#define HB_CDP_CSSORT_IGNORE        2     /* ignore case */

/* byte order */
#define HB_CDP_ENDIAN_NATIVE        0
#define HB_CDP_ENDIAN_LITTLE        1
#define HB_CDP_ENDIAN_BIG           2

extern HB_EXPORT HB_BOOL      hb_cdpRegisterRaw( PHB_CODEPAGE cdp );
extern HB_EXPORT HB_BOOL      hb_cdpRegisterNew( const char * id,
                                                 const char * info,
                                                 PHB_UNITABLE uniTable,
                                                 const char * pszUpper,
                                                 const char * pszLower,
                                                 unsigned int nACSort,
                                                 unsigned int nCaseSort );
extern HB_EXPORT void         hb_cdpBuildTransTable( PHB_UNITABLE uniTable );
extern HB_EXPORT void         hb_cdpReleaseAll( void );
extern HB_EXPORT const char * hb_cdpID( void );
extern HB_EXPORT PHB_CODEPAGE hb_cdpSelect( PHB_CODEPAGE cdp );
extern HB_EXPORT const char * hb_cdpSelectID( const char * id );
extern HB_EXPORT PHB_CODEPAGE hb_cdpFind( const char * id );
extern HB_EXPORT PHB_CODEPAGE hb_cdpFindExt( const char * id );
extern HB_EXPORT const char ** hb_cdpList( void ); /* Caller must release the pointer */

extern HB_EXPORT HB_BOOL      hb_cdpIsDigit( PHB_CODEPAGE cdp, int iChar );
extern HB_EXPORT HB_BOOL      hb_cdpIsAlpha( PHB_CODEPAGE cdp, int iChar );
extern HB_EXPORT HB_BOOL      hb_cdpIsLower( PHB_CODEPAGE cdp, int iChar );
extern HB_EXPORT HB_BOOL      hb_cdpIsUpper( PHB_CODEPAGE cdp, int iChar );
extern HB_EXPORT int          hb_cdpcmp( const char * szFirst, HB_SIZE nLenFirst, const char * szSecond, HB_SIZE nLenSecond, PHB_CODEPAGE cdp, HB_BOOL fExact );
extern HB_EXPORT int          hb_cdpicmp( const char * szFirst, HB_SIZE nLenFirst, const char * szSecond, HB_SIZE nLenSecond, PHB_CODEPAGE cdp, HB_BOOL fExact );
extern HB_EXPORT int          hb_cdpchrcmp( char cFirst, char cSecond, PHB_CODEPAGE cdp );

extern HB_EXPORT char *       hb_cdpDup( const char *, PHB_CODEPAGE, PHB_CODEPAGE );
extern HB_EXPORT char *       hb_cdpnDup( const char *, HB_SIZE *, PHB_CODEPAGE, PHB_CODEPAGE );
extern HB_EXPORT const char * hb_cdpnDup2( const char *, HB_SIZE, char *, HB_SIZE *, PHB_CODEPAGE, PHB_CODEPAGE );
extern HB_EXPORT const char * hb_cdpnDup3( const char *, HB_SIZE, char *, HB_SIZE *, char **, HB_SIZE *, PHB_CODEPAGE, PHB_CODEPAGE );
extern HB_EXPORT HB_SIZE      hb_cdpnDupLen( const char *, HB_SIZE, PHB_CODEPAGE, PHB_CODEPAGE );
extern HB_EXPORT HB_SIZE      hb_cdpnDup2Len( const char *, HB_SIZE, HB_SIZE, PHB_CODEPAGE, PHB_CODEPAGE );

extern HB_EXPORT int          hb_cdpTranslateChar( int iChar, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut );
extern HB_EXPORT int          hb_cdpTranslateDispChar( int iChar, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut );
extern HB_EXPORT HB_SIZE      hb_cdpTransLen( const char * pSrc, HB_SIZE nSrc, HB_SIZE nMax, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut );
extern HB_EXPORT HB_SIZE      hb_cdpTransTo( const char * pSrc, HB_SIZE nSrc, char * pDst, HB_SIZE nDst, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut );

extern HB_EXPORT HB_WCHAR     hb_cdpGetU16( PHB_CODEPAGE cdp, HB_UCHAR ch );
extern HB_EXPORT HB_WCHAR     hb_cdpGetU16Disp( PHB_CODEPAGE cdp, HB_UCHAR ch );
extern HB_EXPORT HB_UCHAR     hb_cdpGetChar( PHB_CODEPAGE cdp, HB_WCHAR wc );
extern HB_EXPORT HB_BOOL      hb_cdpGetFromUTF8( PHB_CODEPAGE cdp, HB_UCHAR ch, int * n, HB_WCHAR * pwc );

extern HB_EXPORT HB_SIZE      hb_cdpUTF8StringLength( const char * pSrc, HB_SIZE nLen );
extern HB_EXPORT HB_SIZE      hb_cdpUTF8StringAt( const char * szNeedle, HB_SIZE nLenN, const char * szHaystack, HB_SIZE nLenH, HB_SIZE nStart, HB_SIZE nEnd, HB_BOOL fReverse );
extern HB_EXPORT HB_SIZE      hb_cdpUTF8StringPeek( const char * pSrc, HB_SIZE nLen, HB_SIZE nPos );
extern HB_EXPORT char *       hb_cdpUTF8StringSubstr( const char * pSrc, HB_SIZE nLen, HB_SIZE nFrom, HB_SIZE nCount, HB_SIZE * pnDest );

extern HB_EXPORT HB_SIZE      hb_cdpUTF8AsStrLen( PHB_CODEPAGE cdp, const char * pSrc, HB_SIZE nSrc, HB_SIZE nMax );
extern HB_EXPORT HB_SIZE      hb_cdpUTF8ToStr( PHB_CODEPAGE cdp, const char * pSrc, HB_SIZE nSrc, char * pDst, HB_SIZE nDst );
extern HB_EXPORT HB_SIZE      hb_cdpStrAsUTF8Len( PHB_CODEPAGE cdp, const char * pSrc, HB_SIZE nSrc, HB_SIZE nMax );
extern HB_EXPORT HB_SIZE      hb_cdpStrToUTF8( PHB_CODEPAGE cdp, const char * pSrc, HB_SIZE nSrc, char * pDst, HB_SIZE nDst );
extern HB_EXPORT HB_SIZE      hb_cdpStrToUTF8Disp( PHB_CODEPAGE cdp, const char * pSrc, HB_SIZE nSrc, char * pDst, HB_SIZE nDst );

extern HB_EXPORT HB_SIZE      hb_cdpU16AsStrLen( PHB_CODEPAGE cdp, const HB_WCHAR * pSrc, HB_SIZE nSrc, HB_SIZE nMax );
extern HB_EXPORT HB_SIZE      hb_cdpU16ToStr( PHB_CODEPAGE cdp, int iEndian, const HB_WCHAR * pSrc, HB_SIZE nSrc, char * pDst, HB_SIZE nDst );
extern HB_EXPORT HB_SIZE      hb_cdpStrAsU16Len( PHB_CODEPAGE cdp, const char * pSrc, HB_SIZE nSrc, HB_SIZE nMax );
extern HB_EXPORT HB_SIZE      hb_cdpStrToU16( PHB_CODEPAGE cdp, int iEndian, const char * pSrc, HB_SIZE nSrc, HB_WCHAR * pDst, HB_SIZE nDst );

extern HB_EXPORT int          hb_cdpUTF8CharSize( HB_WCHAR wc );
extern HB_EXPORT int          hb_cdpU16CharToUTF8( char * szUTF8, HB_WCHAR wc );
extern HB_EXPORT HB_BOOL      hb_cdpUTF8ToU16NextChar( HB_UCHAR ucChar, int * n, HB_WCHAR * pwc );

extern HB_EXPORT PHB_ITEM     hb_itemDeserializeCP( const char ** pBufferPtr, HB_SIZE * pnSize, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut );
extern HB_EXPORT char *       hb_itemSerializeCP( PHB_ITEM pItem, HB_BOOL fNumSize, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, HB_SIZE * pnSize );

HB_EXTERN_END

#endif /* HB_APICDP_H_ */
