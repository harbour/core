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

#include <ctype.h>

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
   char *   uniID;
   int      nChars;
   BOOL     lMulti;
   USHORT * uniCodes;
} HB_UNITABLE, * PHB_UNITABLE;

typedef struct _HB_MULTICHAR
{
   char  cLast[2];
   char  cFirst[2];
   int   nCode;
} HB_MULTICHAR, * PHB_MULTICHAR;

typedef struct _HB_CODEPAGE
{
   char *         id;
   char *         uniID;
   PHB_UNITABLE   uniTable;
   int            nChars;
   char *         CharsUpper;
   char *         CharsLower;
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
#define HB_CPID_850        "cp850"
#define HB_CPID_852        "cp852"
#define HB_CPID_857        "cp857"
#define HB_CPID_866        "cp866"
#define HB_CPID_1250       "cp1250"
#define HB_CPID_1251       "cp1251"
#define HB_CPID_1253       "cp1253"
#define HB_CPID_1254       "cp1254"
#define HB_CPID_1257       "cp1257"
#define HB_CPID_8859_1     "iso8859-1"
#define HB_CPID_8859_1B    "iso8859-1b"
#define HB_CPID_8859_2     "iso8859-2"
#define HB_CPID_8859_5     "iso8859-5"
#define HB_CPID_KOI_8      "koi-8"
#define HB_CPID_KOI_8U     "koi-8u"
#define HB_CPID_MAZ        "plmaz"
#define HB_CPID_KAM        "cskam"
#define HB_UNITB_437       &hb_uniTbl_437
#define HB_UNITB_737       &hb_uniTbl_737
#define HB_UNITB_850       &hb_uniTbl_850
#define HB_UNITB_852       &hb_uniTbl_852
#define HB_UNITB_857       &hb_uniTbl_857
#define HB_UNITB_866       &hb_uniTbl_866
#define HB_UNITB_1250      &hb_uniTbl_1250
#define HB_UNITB_1251      &hb_uniTbl_1251
#define HB_UNITB_1253      &hb_uniTbl_1253
#define HB_UNITB_1254      &hb_uniTbl_1254
#define HB_UNITB_1257      &hb_uniTbl_1257
#define HB_UNITB_8859_1    &hb_uniTbl_8859_1
#define HB_UNITB_8859_1B   &hb_uniTbl_8859_1b
#define HB_UNITB_8859_2    &hb_uniTbl_8859_2
#define HB_UNITB_8859_5    &hb_uniTbl_8859_5
#define HB_UNITB_KOI_8     &hb_uniTbl_KOI_8
#define HB_UNITB_KOI_8U    &hb_uniTbl_KOI_8U
#define HB_UNITB_KAM       &hb_uniTbl_kam
#define HB_UNITB_MAZ       &hb_uniTbl_mazovia
#define HB_UNITB_UNDEF     NULL /* ((PHB_UNITABLE) (-1)) */

extern HB_UNITABLE hb_uniTbl_437;
extern HB_UNITABLE hb_uniTbl_737;
extern HB_UNITABLE hb_uniTbl_850;
extern HB_UNITABLE hb_uniTbl_852;
extern HB_UNITABLE hb_uniTbl_857;
extern HB_UNITABLE hb_uniTbl_866;
extern HB_UNITABLE hb_uniTbl_1250;
extern HB_UNITABLE hb_uniTbl_1251;
extern HB_UNITABLE hb_uniTbl_1253;
extern HB_UNITABLE hb_uniTbl_1254;
extern HB_UNITABLE hb_uniTbl_1257;
extern HB_UNITABLE hb_uniTbl_8859_1;
extern HB_UNITABLE hb_uniTbl_8859_1b;
extern HB_UNITABLE hb_uniTbl_8859_2;
extern HB_UNITABLE hb_uniTbl_8859_5;
extern HB_UNITABLE hb_uniTbl_KOI_8;
extern HB_UNITABLE hb_uniTbl_KOI_8U;
extern HB_UNITABLE hb_uniTbl_mazovia;
extern HB_UNITABLE hb_uniTbl_kam;

extern HB_EXPORT BOOL          hb_cdpRegister( PHB_CODEPAGE );
extern HB_EXPORT char *        hb_cdpSelectID( const char * );
extern HB_EXPORT PHB_CODEPAGE  hb_cdpSelect( PHB_CODEPAGE );
extern HB_EXPORT PHB_CODEPAGE  hb_cdpFind( const char * );
extern HB_EXPORT void          hb_cdpTranslate( char *, PHB_CODEPAGE, PHB_CODEPAGE );
extern HB_EXPORT void          hb_cdpnTranslate( char *, PHB_CODEPAGE, PHB_CODEPAGE, ULONG );
extern HB_EXPORT int           hb_cdpcmp( const char *, ULONG, const char *, ULONG, PHB_CODEPAGE, BOOL );
extern HB_EXPORT int           hb_cdpicmp( const char *, ULONG, const char *, ULONG, PHB_CODEPAGE, BOOL );
extern HB_EXPORT int           hb_cdpchrcmp( char, char, PHB_CODEPAGE );
extern HB_EXPORT void          hb_cdpReleaseAll( void );
       
extern HB_EXPORT USHORT        hb_cdpGetU16( PHB_CODEPAGE, BOOL, BYTE );
extern HB_EXPORT BOOL          hb_cdpGetFromUTF8( PHB_CODEPAGE, BOOL, BYTE, int *, USHORT * );
extern HB_EXPORT ULONG         hb_cdpStrnToUTF8( PHB_CODEPAGE, BOOL, const BYTE *, ULONG, BYTE * );
extern HB_EXPORT ULONG         hb_cdpStrnToU16( PHB_CODEPAGE, BOOL, const BYTE *, ULONG, BYTE * );
extern HB_EXPORT ULONG         hb_cdpStringInUTF8Length( PHB_CODEPAGE, BOOL, const BYTE *, ULONG );
extern HB_EXPORT ULONG         hb_cdpUTF8ToStrn( PHB_CODEPAGE, BOOL, const BYTE *, ULONG, BYTE *, ULONG );
extern HB_EXPORT ULONG         hb_cdpUTF8StringLength( const BYTE *, ULONG );
extern HB_EXPORT BYTE *        hb_cdpUTF8StringSubstr( const BYTE *, ULONG, ULONG, ULONG, ULONG * );
extern HB_EXPORT ULONG         hb_cdpUTF8StringPeek( const BYTE *, ULONG, ULONG );

extern PHB_CODEPAGE hb_cdp_page;

HB_EXTERN_END

#else

typedef void * PHB_CODEPAGE;

#endif /* HB_CDP_SUPPORT_OFF */

#endif /* HB_APICDP_H_ */
