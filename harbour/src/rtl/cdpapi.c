/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The CodePages API
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbvm.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapicdp.h"
#include "hbthread.h"


/* character flags */
#define HB_CDP_DIGIT    0x01
#define HB_CDP_ALPHA    0x02
#define HB_CDP_LOWER    0x04
#define HB_CDP_UPPER    0x08
#define HB_CDP_MULTI1   0x10
#define HB_CDP_MULTI2   0x20


/* Now we are using only 16bit Unicode values so the maximum size
 * of single character encoded in UTF8 is 3 though ISO 10646 Universal
 * Character Set (UCS) occupies even a 31-bit code space and to encode
 * all UCS values we will need 6 bytes. Now in practice no one uses
 * Unicode character over 0xFFFF but it may change in the future so
 * it's safer to use macro for maximum UTF8 character size. [druzus]
 */
#define HB_MAX_UTF8        3


/* MT macros */
#define HB_CDP_LOCK           hb_threadEnterCriticalSection( &s_cdpMtx );
#define HB_CDP_UNLOCK         hb_threadLeaveCriticalSection( &s_cdpMtx );
static HB_CRITICAL_NEW( s_cdpMtx );


#define NUMBER_OF_CHARS    256

static const HB_WCHAR s_uniCodes[ NUMBER_OF_CHARS ] =
{
   0x0020, 0x263A, 0x263B, 0x2665, 0x2666, 0x2663, 0x2660, 0x2022,
   0x25D8, 0x25CB, 0x25D9, 0x2642, 0x2640, 0x266A, 0x266B, 0x263C,
   0x25BA, 0x25C4, 0x2195, 0x203C, 0x00B6, 0x00A7, 0x25AC, 0x21A8,
   0x2191, 0x2193, 0x2192, 0x2190, 0x2319, 0x2194, 0x25B2, 0x25BC,
   0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027,
   0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F,
   0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
   0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
   0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047,
   0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F,
   0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057,
   0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F,
   0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067,
   0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F,
   0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077,
   0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F,
   0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7,
   0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
   0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9,
   0x00FF, 0x00D6, 0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192,
   0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
   0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
   0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
   0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
   0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
   0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
   0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
   0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
   0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
   0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
   0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
   0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0
};

HB_UNITABLE hb_uniTbl_437 = { HB_CPID_437, s_uniCodes, NULL, 0 };

static HB_CODEPAGE s_en_codepage =
   { "EN", "English CP-437", HB_UNITB_437, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, NULL, NULL, NULL };

HB_UNITABLE hb_uniTbl_UTF8 = { HB_CPID_437, s_uniCodes, NULL, 0 };

/* pseudo codepage for translations only */
static HB_CODEPAGE s_utf8_codepage =
   { "UTF8", "UTF-8", &hb_uniTbl_UTF8, NULL, NULL, NULL, NULL, NULL, 0, 0, 0, NULL, NULL, NULL };

HB_CODEPAGE_ANNOUNCE( EN )

static PHB_CODEPAGE s_cdpList = NULL;


/*
 * basic CP functions
 */
BOOL hb_cdpIsDigit( PHB_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_DIGIT ) != 0;
   else
      return HB_ISDIGIT( iChar );
}

BOOL hb_cdpIsAlpha( PHB_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_ALPHA ) != 0;
   else
      return HB_ISALPHA( iChar );
}

BOOL hb_cdpIsLower( PHB_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_LOWER ) != 0;
   else
      return HB_ISLOWER( iChar );
}

BOOL hb_cdpIsUpper( PHB_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_UPPER ) != 0;
   else
      return HB_ISUPPER( iChar );
}

BOOL hb_charIsDigit( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_DIGIT ) != 0;
   else
      return HB_ISDIGIT( iChar );
}

BOOL hb_charIsAlpha( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_ALPHA ) != 0;
   else
      return HB_ISALPHA( iChar );
}

BOOL hb_charIsLower( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_LOWER ) != 0;
   else
      return HB_ISLOWER( iChar );
}

BOOL hb_charIsUpper( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_UPPER ) != 0;
   else
      return HB_ISUPPER( iChar );
}

int hb_charLower( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return cdp->lower[ iChar & 0x0ff ];
   else
      return HB_TOLOWER( iChar );
}

int hb_charUpper( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return cdp->upper[ iChar & 0x0ff ];
   else
      return HB_TOUPPER( iChar );
}

char * hb_strLower( char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strLower(%s, %lu)", szText, ulLen));

   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      ULONG u;

      if( cdp )
         for( u = 0; u < ulLen; u++ )
            szText[ u ] = ( char ) cdp->lower[ ( unsigned char ) szText[ u ] ];
      else
         for( u = 0; u < ulLen; u++ )
            szText[ u ] = HB_TOLOWER( szText[ u ] );
   }

   return szText;
}

char * hb_strUpper( char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strUpper(%s, %lu)", szText, ulLen));

   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      ULONG u;

      if( cdp )
         for( u = 0; u < ulLen; u++ )
            szText[ u ] = ( char ) cdp->upper[ ( unsigned char ) szText[ u ] ];
      else
         for( u = 0; u < ulLen; u++ )
            szText[ u ] = HB_TOUPPER( szText[ u ] );
   }

   return szText;
}

/*
 * comparison
 */
int hb_cdpchrcmp( char cFirst, char cSecond, PHB_CODEPAGE cdp )
{
   if( cFirst == cSecond )
      return 0;

   if( cdp->sort )
   {
      int n1 = cdp->sort[ ( unsigned char ) cFirst ],
          n2 = cdp->sort[ ( unsigned char ) cSecond ];

      if( !cdp->nMulti || ( n1 != 0 && n2 != 0 ) )
      {
         if( n1 == n2 )
         {
            if( cdp->acc )
            {
               n1 = cdp->acc[ ( unsigned char ) cFirst ];
               n2 = cdp->acc[ ( unsigned char ) cSecond ];
            }
            else
               return 0;
         }
         return ( n1 < n2 ) ? -1 : 1;
      }
   }

   return ( ( unsigned char ) cFirst < ( unsigned char ) cSecond ) ? -1 : 1;
}

static int hb_cdpMultiWeight( PHB_CODEPAGE cdp, const char *szChar )
{
   PHB_MULTICHAR pmulti = cdp->multi;
   int i;

   for( i = cdp->nMulti; i; --i, ++pmulti )
   {
      if( ( szChar[ 0 ] == pmulti->cFirst[ 0 ] ||
            szChar[ 0 ] == pmulti->cFirst[ 1 ] ) &&
          ( szChar[ 1 ] == pmulti->cLast[ 0 ] ||
            szChar[ 1 ] == pmulti->cLast[ 1 ] ) )
      {
         return ( szChar[ 0 ]  == pmulti->cFirst[ 0 ] ) ?
                pmulti->sortUp : pmulti->sortLo;
      }
   }

   return 0;
}

int hb_cdpcmp( const char *szFirst, ULONG ulLenFirst,
               const char *szSecond, ULONG ulLenSecond,
               PHB_CODEPAGE cdp, BOOL fExact )
{
   int iRet = 0, iAcc = 0, n, n1, n2;
   ULONG ul, ulLen;

   ulLen = ulLenFirst < ulLenSecond ? ulLenFirst : ulLenSecond;
   if( cdp->sort == NULL )
   {
      iRet = memcmp( szFirst, szSecond, ulLen );
   }
   else if( cdp->nMulti )
   {
      for( ul = 0; ul < ulLen; ++szFirst, ++szSecond, ++ul )
      {
         unsigned char u1 = ( unsigned char ) * szFirst;
         unsigned char u2 = ( unsigned char ) * szSecond;

         n1 = cdp->sort[ u1 ];
         if( ( cdp->flags[ u1 ] & HB_CDP_MULTI1 ) != 0 &&
             ( ul < ulLenFirst - 1 ) &&
             ( cdp->flags[ ( unsigned char ) szFirst[ 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            n = hb_cdpMultiWeight( cdp, szFirst );
            if( n != 0 )
            {
               n1 = n;
               ++szFirst;
               if( --ulLenFirst < ulLen )
                  ulLen = ulLenFirst;
            }
         }
         n2 = cdp->sort[ u2 ];
         if( ( cdp->flags[ u2 ] & HB_CDP_MULTI1 ) != 0 &&
             ( ul < ulLenSecond - 1 ) &&
             ( cdp->flags[ ( unsigned char ) szSecond[ 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            n = hb_cdpMultiWeight( cdp, szSecond );
            if( n != 0 )
            {
               n2 = n;
               ++szSecond;
               if( --ulLenSecond < ulLen )
                  ulLen = ulLenSecond;
            }
         }
         if( n1 != n2 )
         {
            if( n1 == 0 || n2 == 0 )
               /* One of characters doesn't belong to the national characters */
               iRet = ( u1 < u2 ) ? -1 : 1;
            else
               iRet = ( n1 < n2 ) ? -1 : 1;
            break;
         }
         else if( u1 != u2 )
         {
            if( n1 == 0 )
            {
               iRet = ( u1 < u2 ) ? -1 : 1;
               break;
            }
            if( iAcc == 0 && ( fExact || ( ulLenFirst == ulLenSecond && cdp->acc ) ) )
            {
               if( cdp->acc )
                  iAcc = ( cdp->acc[ u1 ] < cdp->acc[ u2 ] ) ? -1 : 1;
               else
                  iAcc = ( u1 < u2 ) ? -1 : 1;
            }
         }
      }
   }
   else
   {
      for( ul = 0; ul < ulLen; ++szFirst, ++szSecond, ++ul )
      {
         if( *szFirst != *szSecond )
         {
            n1 = ( unsigned char ) cdp->sort[ ( unsigned char ) * szFirst ];
            n2 = ( unsigned char ) cdp->sort[ ( unsigned char ) * szSecond ];
            if( n1 != n2 )
            {
               iRet = ( n1 < n2 ) ? -1 : 1;
               break;
            }
            if( iAcc == 0 && ( fExact || ( ulLenFirst == ulLenSecond && cdp->acc ) ) )
            {
               if( cdp->acc )
                  iAcc = ( cdp->acc[ ( unsigned char ) * szFirst ] <
                           cdp->acc[ ( unsigned char ) * szSecond ] ) ? -1 : 1;
               else
                  iAcc = ( ( unsigned char ) * szFirst < ( unsigned char ) * szSecond ) ? -1 : 1;
            }
         }
      }
   }

   if( !iRet )
   {
      if( iAcc )
         iRet = iAcc;
      else if( ulLenSecond > ulLenFirst )
         iRet = -1;
      else if( fExact && ulLenSecond < ulLenFirst )
         iRet = 1;
   }

   return iRet;
}

static int hb_cdpMultiWeightI( PHB_CODEPAGE cdp, const char *szChar )
{
   PHB_MULTICHAR pmulti = cdp->multi;
   int i;

   for( i = cdp->nMulti; i; --i, ++pmulti )
   {
      if( ( szChar[ 0 ] == pmulti->cFirst[ 0 ] ||
            szChar[ 0 ] == pmulti->cFirst[ 1 ] ) &&
          ( szChar[ 1 ] == pmulti->cLast[ 0 ] ||
            szChar[ 1 ] == pmulti->cLast[ 1 ] ) )
      {
         return pmulti->sortUp;
      }
   }

   return 0;
}

int hb_cdpicmp( const char *szFirst, ULONG ulLenFirst,
                const char *szSecond, ULONG ulLenSecond,
                PHB_CODEPAGE cdp, BOOL fExact )
{
   int iRet = 0, iAcc = 0, n, n1, n2, u1, u2;
   ULONG ul, ulLen;

   ulLen = ulLenFirst < ulLenSecond ? ulLenFirst : ulLenSecond;
   if( cdp->sort == NULL )
   {
      for( ul = 0; ul < ulLen; ++szFirst, ++szSecond, ++ul )
      {
         u1 = cdp->upper[ ( unsigned char ) * szFirst ];
         u2 = cdp->upper[ ( unsigned char ) * szSecond ];
         if( u1 != u2 )
         {
            iRet = ( u1 < u2 ) ? -1 : 1;
            break;
         }
      }
   }
   else if( cdp->nMulti )
   {
      for( ul = 0; ul < ulLen; ++szFirst, ++szSecond, ++ul )
      {
         u1 = cdp->upper[ ( unsigned char ) * szFirst ];
         u2 = cdp->upper[ ( unsigned char ) * szSecond ];

         if( ( cdp->flags[ u1 ] & HB_CDP_MULTI1 ) != 0 &&
             ( ul < ulLenFirst - 1 ) &&
             ( cdp->flags[ ( unsigned char ) szFirst[ 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            n = hb_cdpMultiWeightI( cdp, szFirst );
            if( n != 0 )
            {
               n1 = n;
               ++szFirst;
               if( --ulLenFirst < ulLen )
                  ulLen = ulLenFirst;
            }
            else
               n1 = cdp->sort[ u1 ];
         }
         else
            n1 = cdp->sort[ u1 ];

         if( ( cdp->flags[ u2 ] & HB_CDP_MULTI1 ) != 0 &&
             ( ul < ulLenSecond - 1 ) &&
             ( cdp->flags[ ( unsigned char ) szSecond[ 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            n = hb_cdpMultiWeightI( cdp, szSecond );
            if( n != 0 )
            {
               n2 = n;
               ++szSecond;
               if( --ulLenSecond < ulLen )
                  ulLen = ulLenSecond;
            }
            else
               n2 = cdp->sort[ u2 ];
         }
         else
            n2 = cdp->sort[ u2 ];

         if( n1 != n2 )
         {
            if( n1 == 0 || n2 == 0 )
               /* One of characters doesn't belong to the national characters */
               iRet = ( u1 < u2 ) ? -1 : 1;
            else
               iRet = ( n1 < n2 ) ? -1 : 1;
            break;
         }
         else if( u1 != u2 )
         {
            if( n1 == 0 )
            {
               iRet = ( u1 < u2 ) ? -1 : 1;
               break;
            }
            if( iAcc == 0 && ( fExact || ( ulLenFirst == ulLenSecond && cdp->acc ) ) )
            {
               if( cdp->acc )
                  iAcc = ( cdp->acc[ u1 ] < cdp->acc[ u2 ] ) ? -1 : 1;
               else
                  iAcc = ( u1 < u2 ) ? -1 : 1;
            }
         }
      }
   }
   else
   {
      for( ul = 0; ul < ulLen; ++szFirst, ++szSecond, ++ul )
      {
         u1 = cdp->upper[ ( unsigned char ) * szFirst ];
         u2 = cdp->upper[ ( unsigned char ) * szSecond ];

         if( u1 != u2 )
         {
            n1 = ( unsigned char ) cdp->sort[ u1 ];
            n2 = ( unsigned char ) cdp->sort[ u2 ];
            if( n1 != n2 )
            {
               iRet = ( n1 < n2 ) ? -1 : 1;
               break;
            }
            if( iAcc == 0 && ( fExact || ( ulLenFirst == ulLenSecond && cdp->acc ) ) )
            {
               if( cdp->acc )
                  iAcc = ( cdp->acc[ u1 ] < cdp->acc[ u2 ] ) ? -1 : 1;
               else
                  iAcc = ( u1 < u2 ) ? -1 : 1;
            }
         }
      }
   }

   if( !iRet )
   {
      if( iAcc )
         iRet = iAcc;
      else if( ulLenSecond > ulLenFirst )
         iRet = -1;
      else if( fExact && ulLenSecond < ulLenFirst )
         iRet = 1;
   }

   return iRet;
}

/*
 * conversions
 */
static void hb_cdpBuildTransTable( PHB_UNITABLE uniTable )
{
   HB_WCHAR wcMax = 0;
   int i;

   HB_CDP_LOCK
   if( uniTable->uniTrans == NULL )
   {
      unsigned char * uniTrans;

      for( i = 0; i < 256; ++i )
      {
         HB_WCHAR wc = uniTable->uniCodes[ i ];
         if( wc > wcMax )
            wcMax = wc;
      }
      uniTrans = ( unsigned char * )
                           hb_xgrab( ( wcMax + 1 ) * sizeof( unsigned char ) );
      memset( uniTrans, '\0', ( wcMax + 1 ) * sizeof( unsigned char ) );
      for( i = 0; i < 256; ++i )
         uniTrans[ uniTable->uniCodes[ i ] ] = ( unsigned char ) i;

      uniTable->wcMax = wcMax;
      uniTable->uniTrans = uniTrans;
   }
   HB_CDP_UNLOCK
}

/*
 * UTF8 conversions
 */
static int utf8Size( HB_WCHAR wc )
{
   if( wc < 0x0080 )
      return 1;
   else if( wc < 0x0800 )
      return 2;
   else                         /* if( wc <= 0xffff ) */
      return 3;
}

static int u16toutf8( char * szUTF8, HB_WCHAR wc )
{
   int n;

   if( wc < 0x0080 )
   {
      szUTF8[0] = wc & 0xff;
      n = 1;
   }
   else if( wc < 0x0800 )
   {
      szUTF8[0] = 0xc0 | ( ( wc >> 6 ) & 0x1f );
      szUTF8[1] = 0x80 | ( wc & 0x3f );
      n = 2;
   }
   else                         /* if( wc <= 0xffff ) */
   {
      szUTF8[0] = 0xe0 | ( ( wc >> 12 ) & 0x0f );
      szUTF8[1] = 0x80 | ( ( wc >> 6 ) & 0x3f );
      szUTF8[2] = 0x80 | ( wc & 0x3f );
      n = 3;
   }
/*
   else
   {
      n = 0;
   }
*/
   return n;
}

static BOOL utf8tou16nextchar( unsigned char ucChar, int *n, HB_WCHAR * pwc )
{
   if( *n > 0 )
   {
      if( ( ucChar & 0xc0 ) != 0x80 )
         return FALSE;
      *pwc = ( *pwc << 6 ) | ( ucChar & 0x3f );
      ( *n )--;
      return TRUE;
   }

   *n = 0;
   *pwc = ucChar;
   if( ucChar >= 0xc0 )
   {
      if( ucChar < 0xe0 )
      {
         *pwc &= 0x1f;
         *n = 1;
      }
      else if( ucChar < 0xf0 )
      {
         *pwc &= 0x0f;
         *n = 2;
      }
      else if( ucChar < 0xf8 )
      {
         *pwc &= 0x07;
         *n = 3;
      }
      else if( ucChar < 0xfc )
      {
         *pwc &= 0x03;
         *n = 4;
      }
      else if( ucChar < 0xfe )
      {
         *pwc &= 0x01;
         *n = 5;
      }
   }
   return TRUE;
}

static ULONG utf8pos( const char * szUTF8, ULONG ulLen, ULONG ulUTF8Pos )
{
   if( ulUTF8Pos )
   {
      ULONG ul, ul2;
      HB_WCHAR uc;
      int n = 0;

      for( ul = ul2 = 0; ul < ulLen; ++ul )
      {
         if( utf8tou16nextchar( ( UCHAR ) szUTF8[ ul ], &n, &uc ) )
         {
            if( n == 0 )
            {
               if( --ulUTF8Pos == 0 )
                  return ul2 + 1;
               ul2 = ul + 1;
            }
         }
      }
   }
   return 0;
}

ULONG hb_cdpUTF8StringLength( const char * pSrc, ULONG ulLen )
{
   ULONG ul, ulDst;
   HB_WCHAR uc;
   int n = 0;

   for( ul = ulDst = 0; ul < ulLen; ++ul )
   {
      if( utf8tou16nextchar( ( UCHAR ) pSrc[ ul ], &n, &uc ) )
      {
         if( n == 0 )
            ++ulDst;
      }
   }

   return ulDst;
}

ULONG hb_cdpUTF8StringPeek( const char * pSrc, ULONG ulLen, ULONG ulPos )
{
   if( ulLen )
   {
      ULONG ul;
      HB_WCHAR uc = 0;
      int n = 0;

      for( ul = 0; ul < ulLen && ulPos; ++ul )
      {
         if( utf8tou16nextchar( ( UCHAR ) pSrc[ ul ], &n, &uc ) )
         {
            if( n == 0 )
               --ulPos;
         }
      }

      if( ul < ulLen )
      {
         n = 0;
         do
         {
            if( utf8tou16nextchar( ( UCHAR ) pSrc[ ul ], &n, &uc ) )
            {
               if( n == 0 )
                  return uc;
            }
         }
         while( ++ul < ulLen );
      }
   }

   return 0;
}

/* caller must free the returned buffer if not NULL */
char * hb_cdpUTF8StringSubstr( const char * pSrc, ULONG ulLen,
                               ULONG ulFrom, ULONG ulCount, ULONG * pulDest )
{
   ULONG ul, ulCnt, ulDst = 0;
   HB_WCHAR uc;
   int n;
   char *pDst = NULL;

   if( ulCount && ulLen )
   {
      n = 0;
      for( ul = 0; ul < ulLen && ulFrom; ++ul )
      {
         if( utf8tou16nextchar( pSrc[ ul ], &n, &uc ) )
         {
            if( n == 0 )
               --ulFrom;
         }
      }

      if( ul < ulLen )
      {
         ulFrom = ul;
         ulCnt = ulCount;
         n = 0;
         do
         {
            if( utf8tou16nextchar( pSrc[ ul ], &n, &uc ) )
            {
               if( n == 0 )
                  --ulCnt;
            }
         }
         while( ++ul < ulLen && ulCnt );

         ulDst = ul - ulFrom;
         pDst = ( char * ) hb_xgrab( ulDst + 1 );
         memcpy( pDst, &pSrc[ ulFrom ], ulDst );
         pDst[ ulDst ] = '\0';
      }
   }

   if( pulDest )
      *pulDest = ulDst;

   return pDst;
}

BOOL hb_cdpGetFromUTF8( PHB_CODEPAGE cdp, BOOL fCtrl, unsigned char ch,
                        int * n, HB_WCHAR * pwc )
{
   if( utf8tou16nextchar( ch, n, pwc ) )
   {
      if( *n == 0 && cdp && ( fCtrl || *pwc >= 32 ) )
      {
         if( cdp->uniTable->uniTrans == NULL )
            hb_cdpBuildTransTable( cdp->uniTable );

         if( *pwc <= cdp->uniTable->wcMax )
         {
            unsigned char uc = cdp->uniTable->uniTrans[ *pwc ];
            if( uc )
               *pwc = uc;
         }
      }
      return TRUE;
   }
   return FALSE;
}

ULONG hb_cdpStrAsUTF8Len( PHB_CODEPAGE cdp, BOOL fCtrl,
                          const char * pSrc, ULONG ulSrc,
                          ULONG ulMax )
{
   const HB_WCHAR * uniCodes;
   ULONG ulS, ulD, u;
   int i;

   uniCodes = cdp->uniTable->uniCodes;
   for( ulS = ulD = 0; ulS < ulSrc; ++ulS )
   {
      unsigned char uc = ( unsigned char ) pSrc[ ulS ];
      HB_WCHAR wc;

      if( !fCtrl && uc < 32 )
         wc = uc;
      else
      {
         wc = uniCodes[ uc ];
         if( cdp->nMultiUC &&
             ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
             ulS + 1 < ulSrc &&
             ( cdp->flags[ ( unsigned char ) pSrc[ ulS + 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            for( i = 0; i < cdp->nMulti; ++i )
            {
               if( pSrc[ ulS + 1 ] == cdp->multi[ i ].cLast[ 0 ] ||
                   pSrc[ ulS + 1 ] == cdp->multi[ i ].cLast[ 1 ] )
               {
                  if( pSrc[ ulS ]  == cdp->multi[ i ].cFirst[ 0 ] )
                  {
                     wc = cdp->multi[ i ].wcUp;
                     ++ulS;
                     break;
                  }
                  else if( pSrc[ ulS ]  == cdp->multi[ i ].cFirst[ 1 ] )
                  {
                     wc = cdp->multi[ i ].wcLo;
                     ++ulS;
                     break;
                  }
               }
            }
         }
      }
      u = utf8Size( wc );
      if( ulMax && ulD + u > ulMax )
         break;
      ulD += u;
   }

   return ulD;
}

ULONG hb_cdpStrToUTF8( PHB_CODEPAGE cdp, BOOL fCtrl,
                       const char * pSrc, ULONG ulSrc,
                       char * pDst, ULONG ulDst )
{
   const HB_WCHAR * uniCodes;
   ULONG ulS, ulD, u;
   int i;

   uniCodes = cdp->uniTable->uniCodes;
   for( ulS = ulD = 0; ulS < ulSrc && ulD < ulDst; ++ulS )
   {
      unsigned char uc = ( unsigned char ) pSrc[ ulS ];
      HB_WCHAR wc;

      if( !fCtrl && uc < 32 )
         wc = uc;
      else
      {
         wc = uniCodes[ uc ];
         if( cdp->nMultiUC &&
             ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
             ulS + 1 < ulSrc &&
             ( cdp->flags[ ( unsigned char ) pSrc[ ulS + 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            for( i = 0; i < cdp->nMulti; ++i )
            {
               if( pSrc[ ulS + 1 ] == cdp->multi[ i ].cLast[ 0 ] ||
                   pSrc[ ulS + 1 ] == cdp->multi[ i ].cLast[ 1 ] )
               {
                  if( pSrc[ ulS ]  == cdp->multi[ i ].cFirst[ 0 ] )
                  {
                     wc = cdp->multi[ i ].wcUp;
                     ++ulS;
                     break;
                  }
                  else if( pSrc[ ulS ]  == cdp->multi[ i ].cFirst[ 1 ] )
                  {
                     wc = cdp->multi[ i ].wcLo;
                     ++ulS;
                     break;
                  }
               }
            }
         }
      }
      u = utf8Size( wc );
      if( ulD + u <= ulDst )
      {
         u16toutf8( &pDst[ ulD ], wc );
         ulD += u;
      }
      else
         break;
   }
   if( ulD < ulDst )
      pDst[ ulD ] = '\0';

   return ulD;
}

ULONG hb_cdpUTF8AsStrLen( PHB_CODEPAGE cdp, BOOL fCtrl,
                          const char * pSrc, ULONG ulSrc,
                          ULONG ulMax )
{
   unsigned char * uniTrans;
   HB_WCHAR wcMax, wc = 0;
   ULONG ulS, ulD;
   int n = 0, i;

   if( cdp->uniTable->uniTrans == NULL )
      hb_cdpBuildTransTable( cdp->uniTable );
   uniTrans = cdp->uniTable->uniTrans;
   wcMax = cdp->uniTable->wcMax;

   for( ulS = ulD = 0; ulS < ulSrc; ++ulS )
   {
      if( utf8tou16nextchar( ( unsigned char ) pSrc[ ulS ], &n, &wc ) )
      {
         if( n == 0 )
         {
            ++ulD;
            if( ulMax && ulD >= ulMax )
               break;
            if( wc && cdp->nMultiUC && ( fCtrl || wc >= 32 ) &&
                ( wc > wcMax || uniTrans[ wc ] == 0 ) )
            {
               for( i = 0; i < cdp->nMulti; ++i )
               {
                  if( wc == cdp->multi[ i ].wcUp ||
                      wc == cdp->multi[ i ].wcLo )
                  {
                     ++ulD;
                     break;
                  }
               }
               if( ulMax && ulD >= ulMax )
                  break;
            }
         }
      }
   }

   return ulD;
}

ULONG hb_cdpUTF8ToStr( PHB_CODEPAGE cdp, BOOL fCtrl,
                       const char * pSrc, ULONG ulSrc,
                       char * pDst, ULONG ulDst )
{
   unsigned char * uniTrans;
   HB_WCHAR wcMax, wc = 0;
   ULONG ulS, ulD;
   int n = 0, i;

   if( cdp->uniTable->uniTrans == NULL )
      hb_cdpBuildTransTable( cdp->uniTable );
   uniTrans = cdp->uniTable->uniTrans;
   wcMax = cdp->uniTable->wcMax;

   for( ulS = ulD = 0; ulS < ulSrc && ulD < ulDst; ++ulS )
   {
      if( utf8tou16nextchar( ( unsigned char ) pSrc[ ulS ], &n, &wc ) )
      {
         if( n == 0 )
         {
            if( !fCtrl && wc < 32 )
               pDst[ ulD++ ] = ( unsigned char ) wc;
            else if( wc <= wcMax && uniTrans[ wc ] )
               pDst[ ulD++ ] = uniTrans[ wc ];
            else
            {
               if( wc && cdp->nMultiUC )
               {
                  for( i = 0; i < cdp->nMulti; ++i )
                  {
                     if( wc == cdp->multi[ i ].wcUp )
                     {
                        pDst[ ulD++ ] = cdp->multi[ i ].cFirst[ 0 ];
                        if( ulD < ulDst )
                           pDst[ ulD++ ] = cdp->multi[ i ].cLast[ 0 ];
                        break;
                     }
                     if( wc == cdp->multi[ i ].wcLo )
                     {
                        pDst[ ulD++ ] = cdp->multi[ i ].cFirst[ 1 ];
                        if( ulD < ulDst )
                           pDst[ ulD++ ] = cdp->multi[ i ].cLast[ 1 ];
                        break;
                     }
                  }
                  if( i < cdp->nMulti )
                     continue;
               }
               pDst[ ulD++ ] = wc >= 0x100 ? '?' : ( unsigned char ) wc;
            }
         }
      }
   }

   if( ulD < ulDst )
      pDst[ ulD ] = '\0';

   return ulD;
}

/*
 * U16 (hb wide char) conversions
 */
HB_WCHAR hb_cdpGetU16( PHB_CODEPAGE cdp, BOOL fCtrl, unsigned char ch )
{
   if( cdp && ( fCtrl || ch >= 32 ) )
      return cdp->uniTable->uniCodes[ ch ];
   else
      return ch;
}

unsigned char hb_cdpGetChar( PHB_CODEPAGE cdp, BOOL fCtrl, HB_WCHAR wc )
{
   if( cdp && ( fCtrl || wc >= 32 ) )
   {
      if( cdp->uniTable->uniTrans == NULL )
         hb_cdpBuildTransTable( cdp->uniTable );

      if( wc <= cdp->uniTable->wcMax )
      {
         unsigned char uc = cdp->uniTable->uniTrans[ wc ];
         if( uc )
            wc = uc;
      }
   }
   return wc >= 0x100 ? '?' : ( UCHAR ) wc;
}

ULONG hb_cdpStrAsU16Len( PHB_CODEPAGE cdp, BOOL fCtrl,
                         const char * pSrc, ULONG ulSrc,
                         ULONG ulMax )
{
   if( cdp->nMultiUC )
   {
      ULONG ulS, ulD;
      int i;

      for( ulS = ulD = 0; ulS < ulSrc; ++ulS )
      {
         unsigned char uc = ( unsigned char ) pSrc[ ulS ];

         if( fCtrl || uc >= 32 )
         {
            if( ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
                ulS + 1 < ulSrc &&
                ( cdp->flags[ ( unsigned char ) pSrc[ ulS + 1 ] ] & HB_CDP_MULTI2 ) != 0 )
            {
               for( i = 0; i < cdp->nMulti; ++i )
               {
                  if( pSrc[ ulS + 1 ] == cdp->multi[ i ].cLast[ 0 ] ||
                      pSrc[ ulS + 1 ] == cdp->multi[ i ].cLast[ 1 ] )
                  {
                     if( pSrc[ ulS ]  == cdp->multi[ i ].cFirst[ 0 ] )
                     {
                        ++ulS;
                        break;
                     }
                     else if( pSrc[ ulS ]  == cdp->multi[ i ].cFirst[ 1 ] )
                     {
                        ++ulS;
                        break;
                     }
                  }
               }
            }
         }
         ++ulD;
         if( ulMax && ulD >= ulMax )
            break;
      }
      return ulD;
   }

   return ulSrc;
}

ULONG hb_cdpStrToU16( PHB_CODEPAGE cdp, BOOL fCtrl, int iEndian,
                      const char * pSrc, ULONG ulSrc,
                      HB_WCHAR * pDst, ULONG ulDst )
{
   const HB_WCHAR * uniCodes;
   ULONG ulS, ulD;
   int i;

   uniCodes = cdp->uniTable->uniCodes;
   for( ulS = ulD = 0; ulS < ulSrc && ulD < ulDst; ++ulS )
   {
      unsigned char uc = ( unsigned char ) pSrc[ ulS ];
      HB_WCHAR wc;

      if( !fCtrl && uc < 32 )
         wc = uc;
      else
      {
         wc = uniCodes[ uc ];
         if( cdp->nMultiUC &&
             ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
             ulS + 1 < ulSrc &&
             ( cdp->flags[ ( unsigned char ) pSrc[ ulS + 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            for( i = 0; i < cdp->nMulti; ++i )
            {
               if( pSrc[ ulS + 1 ] == cdp->multi[ i ].cLast[ 0 ] ||
                   pSrc[ ulS + 1 ] == cdp->multi[ i ].cLast[ 1 ] )
               {
                  if( pSrc[ ulS ]  == cdp->multi[ i ].cFirst[ 0 ] )
                  {
                     wc = cdp->multi[ i ].wcUp;
                     ++ulS;
                     break;
                  }
                  else if( pSrc[ ulS ]  == cdp->multi[ i ].cFirst[ 1 ] )
                  {
                     wc = cdp->multi[ i ].wcLo;
                     ++ulS;
                     break;
                  }
               }
            }
         }
      }
#if !defined( HB_BIG_ENDIAN ) && !defined( HB_LITTLE_ENDIAN )
      if( iEndian == HB_CDP_ENDIAN_LITTLE )
         HB_PUT_LE_UINT16( &pDst[ ulD ], wc );
      else if( iEndian == HB_CDP_ENDIAN_BIG )
         HB_PUT_BE_UINT16( &pDst[ ulD ], wc );
      else
         pDst[ ulD ] = wc;
      ++ulD;
#else
#  if defined( HB_BIG_ENDIAN )
      if( iEndian == HB_CDP_ENDIAN_LITTLE )
#  else
      if( iEndian == HB_CDP_ENDIAN_BIG )
#  endif
         wc = HB_SWAP_UINT16( wc );
      pDst[ ulD++ ] = wc;
#endif
   }
   if( ulD < ulDst )
      pDst[ ulD ] = '\0';

   return ulD;
}

ULONG hb_cdpU16AsStrLen( PHB_CODEPAGE cdp, BOOL fCtrl,
                         const HB_WCHAR * pSrc, ULONG ulSrc,
                         ULONG ulMax )
{
   unsigned char * uniTrans;
   HB_WCHAR wcMax, wc;
   ULONG ulS, ulD;
   int i;

   if( cdp->uniTable->uniTrans == NULL )
      hb_cdpBuildTransTable( cdp->uniTable );
   uniTrans = cdp->uniTable->uniTrans;
   wcMax = cdp->uniTable->wcMax;

   for( ulS = ulD = 0; ulS < ulSrc; ++ulS )
   {
      wc = pSrc[ ulS ];
      ++ulD;
      if( ulMax && ulD >= ulMax )
         break;
      if( wc && cdp->nMultiUC && ( fCtrl || wc >= 32 ) &&
          ( wc > wcMax || uniTrans[ wc ] == 0 ) )
      {
         for( i = 0; i < cdp->nMulti; ++i )
         {
            if( wc == cdp->multi[ i ].wcUp ||
                wc == cdp->multi[ i ].wcLo )
            {
               ++ulD;
               break;
            }
         }
         if( ulMax && ulD >= ulMax )
            break;
      }
   }

   return ulD;
}

ULONG hb_cdpU16ToStr( PHB_CODEPAGE cdp, BOOL fCtrl, int iEndian,
                      const HB_WCHAR * pSrc, ULONG ulSrc,
                      char * pDst, ULONG ulDst )
{
   unsigned char * uniTrans;
   HB_WCHAR wcMax, wc;
   ULONG ulS, ulD;
   int i;

   if( cdp->uniTable->uniTrans == NULL )
      hb_cdpBuildTransTable( cdp->uniTable );
   uniTrans = cdp->uniTable->uniTrans;
   wcMax = cdp->uniTable->wcMax;

   for( ulS = ulD = 0; ulS < ulSrc && ulD < ulDst; ++ulS )
   {
#if !defined( HB_BIG_ENDIAN ) && !defined( HB_LITTLE_ENDIAN )
      if( iEndian == HB_CDP_ENDIAN_LITTLE )
         wc = HB_GET_LE_UINT16( &pSrc[ ulS ] );
      else if( iEndian == HB_CDP_ENDIAN_BIG )
         wc = HB_GET_BE_UINT16( &pSrc[ ulS ] );
      else
         wc = pSrc[ ulS ];
#else
      wc = pSrc[ ulS ];
#  if defined( HB_BIG_ENDIAN )
      if( iEndian == HB_CDP_ENDIAN_LITTLE )
#  else
      if( iEndian == HB_CDP_ENDIAN_BIG )
#  endif
         wc = HB_SWAP_UINT16( wc );
#endif
      if( !fCtrl && wc < 32 )
         pDst[ ulD++ ] = ( unsigned char ) wc;
      else if( wc <= wcMax && uniTrans[ wc ] )
         pDst[ ulD++ ] = uniTrans[ wc ];
      else
      {
         if( wc && cdp->nMultiUC )
         {
            for( i = 0; i < cdp->nMulti; ++i )
            {
               if( wc == cdp->multi[ i ].wcUp )
               {
                  pDst[ ulD++ ] = cdp->multi[ i ].cFirst[ 0 ];
                  if( ulD < ulDst )
                     pDst[ ulD++ ] = cdp->multi[ i ].cLast[ 0 ];
                  break;
               }
               if( wc == cdp->multi[ i ].wcLo )
               {
                  pDst[ ulD++ ] = cdp->multi[ i ].cFirst[ 1 ];
                  if( ulD < ulDst )
                     pDst[ ulD++ ] = cdp->multi[ i ].cLast[ 1 ];
                  break;
               }
            }
            if( i < cdp->nMulti )
               continue;
         }
         pDst[ ulD++ ] = wc >= 0x100 ? '?' : ( unsigned char ) wc;
      }
   }

   if( ulD < ulDst )
      pDst[ ulD ] = '\0';

   return ulD;
}


/*
 * CP translations
 */
ULONG hb_cdpTransLen( const char * pSrc, ULONG ulSrc, ULONG ulMax,
                      PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   ULONG ulSize;

   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable )
   {
      if( cdpIn == &s_utf8_codepage )
         return hb_cdpUTF8AsStrLen( cdpOut, FALSE, pSrc, ulSrc, ulMax );
      else if( cdpOut == &s_utf8_codepage )
         return hb_cdpStrAsUTF8Len( cdpIn, FALSE, pSrc, ulSrc, ulMax );
      else
      {
         unsigned char * uniTrans;
         HB_WCHAR wcMax;

         if( cdpOut->uniTable->uniTrans == NULL )
            hb_cdpBuildTransTable( cdpOut->uniTable );
         uniTrans = cdpOut->uniTable->uniTrans;
         wcMax = cdpOut->uniTable->wcMax;

         if( cdpIn->nMultiUC || cdpOut->nMultiUC )
         {
            ULONG ul;
            int i;

            for( ul = ulSize = 0; ul < ulSrc && ( ulMax == 0 || ulSize < ulMax ); ++ul, ++ulSize )
            {
               unsigned char uc = ( unsigned char ) pSrc[ ul ];
               HB_WCHAR wc = cdpIn->uniTable->uniCodes[ uc ];

               if( cdpIn->nMultiUC &&
                   ( cdpIn->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
                   ul + 1 < ulSrc &&
                   ( cdpIn->flags[ ( unsigned char ) pSrc[ ul + 1 ] ] & HB_CDP_MULTI2 ) != 0 )
               {
                  for( i = 0; i < cdpIn->nMulti; ++i )
                  {
                     if( pSrc[ ul + 1 ] == cdpIn->multi[ i ].cLast[ 0 ] ||
                         pSrc[ ul + 1 ] == cdpIn->multi[ i ].cLast[ 1 ] )
                     {
                        if( pSrc[ ul ]  == cdpIn->multi[ i ].cFirst[ 0 ] )
                        {
                           wc = cdpIn->multi[ i ].wcUp;
                           ++ul;
                           break;
                        }
                        else if( pSrc[ ul ]  == cdpIn->multi[ i ].cFirst[ 1 ] )
                        {
                           wc = cdpIn->multi[ i ].wcLo;
                           ++ul;
                           break;
                        }
                     }
                  }
               }

               if( wc && ( wc > wcMax || uniTrans[ wc ] ) && cdpOut->nMultiUC &&
                   ( ulMax == 0 || ulSize + 1 < ulMax ) )
               {
                  for( i = 0; i < cdpOut->nMulti; ++i )
                  {
                     if( wc == cdpOut->multi[ i ].wcUp ||
                         wc == cdpOut->multi[ i ].wcLo )
                     {
                           ++ulSize;
                           break;
                     }
                  }
               }
            }
         }
         else
            ulSize = ( ulMax && ulSrc > ulMax ) ? ulMax : ulSrc;
      }
   }
   else
      ulSize = ( ulMax && ulSrc > ulMax ) ? ulMax : ulSrc;

   return ulSize;
}

ULONG hb_cdpTransTo( const char * pSrc, ULONG ulSrc,
                     char * pDst, ULONG ulDst,
                     PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   ULONG ulSize;

   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable )
   {
      if( cdpIn == &s_utf8_codepage )
         return hb_cdpUTF8ToStr( cdpOut, FALSE, pSrc, ulSrc, pDst, ulDst );
      else if( cdpOut == &s_utf8_codepage )
         return hb_cdpStrToUTF8( cdpIn, FALSE, pSrc, ulSrc, pDst, ulDst );
      else
      {
         unsigned char * uniTrans;
         HB_WCHAR wcMax;

         if( cdpOut->uniTable->uniTrans == NULL )
            hb_cdpBuildTransTable( cdpOut->uniTable );
         uniTrans = cdpOut->uniTable->uniTrans;
         wcMax = cdpOut->uniTable->wcMax;

         if( cdpIn->nMultiUC || cdpOut->nMultiUC )
         {
            ULONG ul;
            int i;

            for( ul = ulSize = 0; ul < ulSrc && ulSize < ulDst; ++ul, ++ulSize )
            {
               unsigned char uc = ( unsigned char ) pSrc[ ul ];
               HB_WCHAR wc = cdpIn->uniTable->uniCodes[ uc ];

               if( cdpIn->nMultiUC &&
                   ( cdpIn->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
                   ul + 1 < ulSrc &&
                   ( cdpIn->flags[ ( unsigned char ) pSrc[ ul + 1 ] ] & HB_CDP_MULTI2 ) != 0 )
               {
                  for( i = 0; i < cdpIn->nMulti; ++i )
                  {
                     if( pSrc[ ul + 1 ] == cdpIn->multi[ i ].cLast[ 0 ] ||
                         pSrc[ ul + 1 ] == cdpIn->multi[ i ].cLast[ 1 ] )
                     {
                        if( pSrc[ ul ]  == cdpIn->multi[ i ].cFirst[ 0 ] )
                        {
                           wc = cdpIn->multi[ i ].wcUp;
                           ++ul;
                           break;
                        }
                        else if( pSrc[ ul ]  == cdpIn->multi[ i ].cFirst[ 1 ] )
                        {
                           wc = cdpIn->multi[ i ].wcLo;
                           ++ul;
                           break;
                        }
                     }
                  }
               }

               if( wc )
               {
                  if( wc <= wcMax && uniTrans[ wc ] )
                     uc = uniTrans[ wc ];
                  else if( cdpOut->nMultiUC )
                  {
                     for( i = 0; i < cdpOut->nMulti; ++i )
                     {
                        if( wc == cdpOut->multi[ i ].wcUp )
                        {
                           if( ulSize + 1 < ulDst )
                           {
                              pDst[ ulSize++ ] = cdpOut->multi[ i ].cFirst[ 0 ];
                              uc = cdpOut->multi[ i ].cLast[ 0 ];
                           }
                           else
                              uc = cdpOut->multi[ i ].cFirst[ 0 ];
                           break;
                        }
                        if( wc == cdpOut->multi[ i ].wcLo )
                        {
                           if( ulSize + 1 < ulDst )
                           {
                              pDst[ ulSize++ ] = cdpOut->multi[ i ].cFirst[ 1 ];
                              uc = cdpOut->multi[ i ].cLast[ 1 ];
                           }
                           else
                              uc = cdpOut->multi[ i ].cFirst[ 1 ];
                           break;
                        }
                     }
                  }
               }

               pDst[ ulSize ] = uc;
            }
         }
         else
         {
            if( ulSrc > ulDst )
               ulSrc = ulDst;
            for( ulSize = 0; ulSize < ulSrc; ++ulSize )
            {
               unsigned char uc = ( unsigned char ) pSrc[ ulSize ];
               HB_WCHAR wc = cdpIn->uniTable->uniCodes[ uc ];
               if( wc && wc <= wcMax && uniTrans[ wc ] )
                  uc = uniTrans[ wc ];
               pDst[ ulSize ] = uc;
            }
         }
      }
   }
   else
   {
      ulSize = ( ulSrc > ulDst ) ? ulDst : ulSrc;
      memcpy( pDst, pSrc, ulSize );
   }

   if( ulSize < ulDst )
      pDst[ ulSize ] = '\0';

   return ulSize;
}

int hb_cdpTranslateChar( int iChar, BOOL fCtrl, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable &&
       iChar >= ( fCtrl ? 32 : 0 ) && iChar < 256 )
   {
      HB_WCHAR wc;

      if( cdpOut->uniTable->uniTrans == NULL )
         hb_cdpBuildTransTable( cdpOut->uniTable );

      wc = cdpIn->uniTable->uniCodes[ iChar ];
      if( wc && wc <= cdpOut->uniTable->wcMax &&
          cdpOut->uniTable->uniTrans[ wc ] )
         iChar = cdpOut->uniTable->uniTrans[ wc ];
   }

   return iChar;
}

ULONG hb_cdpnDupLen( const char * pSrc, ULONG ulSrc,
                     PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   return hb_cdpTransLen( pSrc, ulSrc, 0, cdpIn, cdpOut );
}

ULONG hb_cdpnDup2Len( const char * pSrc, ULONG ulSrc, ULONG ulMax,
                      PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   return hb_cdpTransLen( pSrc, ulSrc, ulMax, cdpIn, cdpOut );
}

char * hb_cdpnDup( const char * pSrc, ULONG * pulLen,
                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   char * pDst;
   ULONG ulDst;

   ulDst = hb_cdpTransLen( pSrc, *pulLen, 0, cdpIn, cdpOut );
   pDst = ( char * ) hb_xgrab( ulDst + 1 );
   hb_cdpTransTo( pSrc, *pulLen, pDst, ulDst + 1, cdpIn, cdpOut );
   *pulLen = ulDst;

   return pDst;
}

const char * hb_cdpnDup2( const char * pSrc, ULONG ulSrc,
                          char * pDst, ULONG * pulDst,
                          PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   * pulDst = hb_cdpTransTo( pSrc, ulSrc, pDst, *pulDst, cdpIn, cdpOut );
   return pDst;
}

const char * hb_cdpnDup3( const char * pSrc, ULONG ulSrc,
                          char * pDst, ULONG * pulDst,
                          char ** pFree, ULONG * pulSize,
                          PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable && ulSrc )
   {
      char * pPrev = NULL;
      ULONG ulDst = hb_cdpTransLen( pSrc, ulSrc, 0, cdpIn, cdpOut );

      if( pDst == NULL )
      {
         pDst = *pFree;
         if( pDst == NULL && *pulSize > 0 )
            pDst = ( char * ) pSrc;
      }

      if( ulDst >= *pulSize || ( pDst == pSrc &&
          ( cdpOut == &s_utf8_codepage || cdpOut->nMultiUC ) ) )
      {
         pPrev = *pFree;
         pDst = *pFree = ( char * ) hb_xgrab( ulDst + 1 );
         *pulSize = ulDst + 1;
      }

      ulDst = hb_cdpTransTo( pSrc, ulSrc, pDst, *pulSize, cdpIn, cdpOut );

      if( pPrev )
         hb_xfree( pPrev );
      if( pulDst )
         *pulDst = ulDst;
      return pDst;
   }

   if( pulDst )
      *pulDst = ulSrc;

   return pSrc;
}

char * hb_cdpDup( const char * pszSrc, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   ULONG ulLen = strlen( pszSrc );
   return hb_cdpnDup( pszSrc, &ulLen, cdpIn, cdpOut );
}


/*
 * CP management
 */
static PHB_CODEPAGE hb_buildCodePage( const char * id, const char * info,
                                      PHB_UNITABLE uniTable,
                                      const char * pszUpper,
                                      const char * pszLower,
                                      unsigned int nACSort )
{
   BOOL lSort, fError;
   int iMulti, iAcc, iAccUp, iAccLo, iSortUp, iSortLo, i;
   const char * pup, * plo;
   unsigned char ucUp, ucLo, ucUp2, ucLo2;
   ULONG ulSize, ul;
   unsigned char * buffer, * flags, * upper, * lower, * sort, * acc;
   unsigned char used[ 256 ];
   PHB_CODEPAGE cdp;
   PHB_MULTICHAR multi;

   memset( used, '\0', sizeof( used ) );

   iMulti = iAcc = iSortUp = iSortLo = 0;
   fError = lSort = FALSE;

   ucUp2 = ucLo2 = 0;
   pup = pszUpper;
   plo = pszLower;
   for( ;; )
   {
      ucUp = ( unsigned char ) *pup++;
      ucLo = ( unsigned char ) *plo++;
      if( ucUp == 0 || ucLo == 0 )
      {
         if( ucUp || ucLo )
            fError = TRUE;
         break;
      }
      if( ucUp == '.' )
      {
         if( ucLo == '.' &&
             pup[ 0 ] && pup[ 1 ] &&
             ( pup[ 2 ] == '.' || pup[ 2 ] == '=' ) &&
             plo[ 0 ] && plo[ 1 ] &&
             ( plo[ 2 ] == '.' || plo[ 2 ] == '=' ) )
         {
            ucUp = ( unsigned char ) *pup;
            ucLo = ( unsigned char ) *plo;

            if( ( ucUp != ' ' || ucLo != ' ' ) &&
                ( ucUp == *pup || ( ucUp != ' ' && *pup != ' ' ) ) &&
                ( ucLo == *plo || ( ucLo != ' ' && *plo != ' ' ) ) )
            {
               if( ucUp != ' ' )
                  ++iSortLo;
               pup += 2;
               plo += 2;
               if( *pup == '=' )
               {
                  do
                     ++pup;
                  while( HB_ISXDIGIT( *pup ) );
               }
               if( *plo == '=' )
               {
                  do
                     ++plo;
                  while( HB_ISXDIGIT( *plo ) );
               }
               if( *pup == '.' && *plo == '.' )
               {
                  lSort = TRUE;
                  iMulti++;
                  pup++;
                  plo++;
                  continue;
               }
            }
         }
         fError = TRUE;
         break;
      }
      if( ucUp == '~' )
      {
         if( ucLo != '~' || *pup == '\0' || *plo == '\0' )
         {
            fError = TRUE;
            break;
         }
         ucUp = ( unsigned char ) *pup++;
         ucLo = ( unsigned char ) *plo++;
         ++iAcc;
      }
      if( used[ ucUp ] != 0 )
         ucUp = ' ';
      if( used[ ucLo ] != 0 )
         ucLo = ' ';
      if( ucUp == ' ' && ucLo == ' ' )
      {
         fError = TRUE;
         break;
      }
      if( ucUp != ' ' )
      {
         used[ ucUp ] = 1;
         ++iSortLo;
         if( ucUp < ucUp2 )
            lSort = TRUE;
         ucUp2 = ucUp;
      }
      if( ucLo != ' ' )
      {
         used[ ucLo ] = 1;
         if( ucLo < ucLo2 )
            lSort = TRUE;
         ucLo2 = ucLo;
      }
   }

   if( iMulti > 64 )
      fError = TRUE;

   if( fError || nACSort > HB_CDP_ACSORT_INTERLEAVED )
      hb_errInternal( 9994, "Harbour CP (%s) initialization failure", id, NULL );

   if( iAcc == 0 )
      nACSort = HB_CDP_ACSORT_NONE;
   else if( nACSort != HB_CDP_ACSORT_NONE )
      lSort = TRUE;

   ulSize = 0x300;
   if( lSort )
   {
      ulSize += 0x100;
      if( nACSort == HB_CDP_ACSORT_INTERLEAVED )
         ulSize += 0x100;
   }
   ul = ulSize;
   ulSize += sizeof( HB_CODEPAGE );
   if( iMulti )
      ulSize += iMulti * sizeof( HB_MULTICHAR );

   buffer = ( unsigned char * ) hb_xgrab( ulSize );
   memset( buffer, '\0', ulSize );
   cdp = ( PHB_CODEPAGE ) &buffer[ ul ];
   cdp->buffer = buffer;

   cdp->flags = flags = buffer;
   buffer += 0x100;
   cdp->upper = upper = buffer;
   buffer += 0x100;
   cdp->lower = lower = buffer;
   buffer += 0x100;
   sort = acc = NULL;
   if( lSort )
   {
      cdp->sort = sort = buffer;
      buffer += 0x100;
      if( nACSort == HB_CDP_ACSORT_INTERLEAVED )
      {
         cdp->acc = acc = buffer;
         buffer += 0x100;
      }
   }
   if( iMulti )
      cdp->multi = ( PHB_MULTICHAR ) &buffer[ sizeof( HB_CODEPAGE ) ];

   cdp->id = id;
   cdp->info = info;
   cdp->uniTable = uniTable;
   cdp->nACSort = nACSort;
   cdp->nMulti = iMulti;
   for( i = 0; i < 0x100; ++i )
   {
      if( HB_ISDIGIT( i ) )
         flags[ i ] |= HB_CDP_DIGIT;
      if( HB_ISALPHA( i ) )
         flags[ i ] |= HB_CDP_ALPHA;
      if( HB_ISUPPER( i ) )
         flags[ i ] |= HB_CDP_UPPER;
      if( HB_ISLOWER( i ) )
         flags[ i ] |= HB_CDP_LOWER;
      upper[ i ] = ( unsigned char ) HB_TOUPPER( i );
      lower[ i ] = ( unsigned char ) HB_TOLOWER( i );
   }

   iAccUp = iAccLo = 0;
   multi = cdp->multi;
   pup = pszUpper;
   plo = pszLower;
   ucUp2 = ucLo2 = 255;
   memset( used, '\0', sizeof( used ) );
   while( *pup )
   {
      ucUp = ( unsigned char ) *pup++;
      ucLo = ( unsigned char ) *plo++;
      if( ucUp == '.' )
      {
         multi->cFirst[ 0 ] = *pup++;
         multi->cLast [ 0 ] = *pup++;
         multi->cFirst[ 1 ] = *plo++;
         multi->cLast [ 1 ] = *plo++;
         if( multi->cFirst[ 0 ] != ' ' )
         {
            flags[ ( unsigned char ) multi->cFirst[ 0 ] ] |= HB_CDP_MULTI1;
            flags[ ( unsigned char ) multi->cLast [ 0 ] ] |= HB_CDP_MULTI2;
            multi->sortUp = ++iSortUp;
         }
         if( multi->cFirst[ 1 ] != ' ' )
         {
            flags[ ( unsigned char ) multi->cFirst[ 1 ] ] |= HB_CDP_MULTI1;
            flags[ ( unsigned char ) multi->cLast [ 1 ] ] |= HB_CDP_MULTI2;
            multi->sortLo = ++iSortLo;
         }
         if( *pup == '=' )
         {
            ++pup;
            while( HB_ISXDIGIT( *pup ) )
            {
               multi->wcUp = ( multi->wcUp << 4 ) |
                             ( *pup >= 'a' ? ( *pup - 'a' + 10 ) :
                               ( *pup >= 'A' ? ( *pup - 'A' + 10 ) :
                                               ( *pup - '0' ) ) );
               ++pup;
            }
         }
         pup++;
         if( *plo == '=' )
         {
            ++plo;
            while( HB_ISXDIGIT( *plo ) )
            {
               multi->wcLo = ( multi->wcLo << 4 ) |
                             ( *plo >= 'a' ? ( *plo - 'a' + 10 ) :
                               ( *plo >= 'A' ? ( *plo - 'A' + 10 ) :
                                               ( *plo - '0' ) ) );
               ++plo;
            }
         }
         plo++;
         if( multi->wcUp || multi->wcLo )
            cdp->nMultiUC++;
         multi++;
      }
      else
      {
         iAcc = 0;
         if( ucUp == '~' )
         {
            iAcc = 1;
            ucUp = ( unsigned char ) *pup++;
            ucLo = ( unsigned char ) *plo++;
         }
         if( ucUp != ' ' )
         {
            flags[ ucUp ] |= HB_CDP_ALPHA;
            flags[ ucUp ] |= HB_CDP_UPPER;
            if( ucLo != ' ' && ( used[ ucUp ] & HB_CDP_UPPER ) == 0 )
            {
               lower[ ucUp ] = ucLo;
               used[ ucUp ] |= HB_CDP_UPPER;
            }
            if( sort )
            {
               if( sort[ ucUp ] == 0 )
               {
                  if( iAcc && nACSort != HB_CDP_ACSORT_NONE )
                     ++iAccUp;
                  sort[ ucUp ] = ++iSortUp - iAccUp;
                  if( acc )
                     acc[ ucUp ] = iSortUp;
                  if( ucUp2 > ucUp )
                     ucUp2 = ucUp;
               }
            }
         }
         if( ucLo != ' ' )
         {
            flags[ ucLo ] |= HB_CDP_ALPHA;
            flags[ ucLo ] |= HB_CDP_LOWER;
            if( ucUp != ' ' && ( used[ ucLo ] & HB_CDP_LOWER ) == 0 )
            {
               upper[ ucLo ] = ucUp;
               used[ ucLo ] |= HB_CDP_LOWER;
            }
            if( sort )
            {
               if( sort[ ucLo ] == 0 )
               {
                  if( iAcc && nACSort != HB_CDP_ACSORT_NONE )
                     ++iAccLo;
                  sort[ ucLo ] = ++iSortLo - iAccLo;
                  if( acc )
                     acc[ ucLo ] = iSortLo;
                  if( ucLo2 > ucLo )
                     ucLo2 = ucLo;
               }
            }
         }
      }
   }

   if( sort )
   {
      int iUp, iLo, iSort1, iSort2, iSort3, iAdd;

      if( iMulti > 0 )
      {
         if( iMulti > ucUp2 || iMulti > ucLo2 )
            hb_errInternal( 9994, "Harbour CP (%s) initialization failure", id, NULL );

         if( iMulti <= 32 )
            iMulti = 33;
         else
            iMulti = 65;
      }
      else
         iMulti = 1;

      for( iUp = iLo = 0, i = iMulti; i < 256; ++i )
      {
         if( sort[ i ] == 0 )
         {
            if( i < ( int ) ucUp2 )
               ++iUp;
            else if( i < ( int ) ucLo2 )
               ++iLo;
         }
      }
      for( iSort1 = iSort2 = iSort3 = 0, i = iMulti; i < 256; ++i )
      {
         if( sort[ i ] == 0 )
         {
            if( i < ( int ) ucUp2 )
               iAdd = ++iSort1;
            else if( i < ( int ) ucLo2 )
               iAdd = ++iSort2 + iSortUp + iUp;
            else
               iAdd = ++iSort3 + iUp + iSortLo + iLo;
         }
         else if( sort[ i ] <= iSortUp )
            iAdd = iUp;
         else
            iAdd = iUp + iLo;

         sort[ i ] += iAdd;
         if( acc )
            acc[ i ] += iAdd;
      }
   }

   return cdp;
}

static PHB_CODEPAGE * hb_cdpFindPos( const char * id )
{
   PHB_CODEPAGE * cdp_ptr;

   if( s_cdpList == NULL )
   {
      unsigned char * flags, * upper, * lower;
      int i;

      s_en_codepage.buffer = ( unsigned char * ) hb_xgrab( 0x300 );
      memset( s_en_codepage.buffer, '\0', 0x300 );
      s_en_codepage.flags = flags = ( unsigned char * ) s_en_codepage.buffer;
      s_en_codepage.upper = upper = ( unsigned char * ) s_en_codepage.buffer + 0x100;
      s_en_codepage.lower = lower = ( unsigned char * ) s_en_codepage.buffer + 0x200;
      for( i = 0; i < 0x100; ++i )
      {
         if( HB_ISDIGIT( i ) )
            flags[ i ] |= HB_CDP_DIGIT;
         if( HB_ISALPHA( i ) )
            flags[ i ] |= HB_CDP_ALPHA;
         if( HB_ISUPPER( i ) )
            flags[ i ] |= HB_CDP_UPPER;
         if( HB_ISLOWER( i ) )
            flags[ i ] |= HB_CDP_LOWER;
         upper[ i ] = ( unsigned char ) HB_TOUPPER( i );
         lower[ i ] = ( unsigned char ) HB_TOLOWER( i );
      }
      s_utf8_codepage.flags = s_en_codepage.flags;
      s_utf8_codepage.upper = s_en_codepage.upper;
      s_utf8_codepage.lower = s_en_codepage.lower;
      s_cdpList = &s_en_codepage;
   }

   cdp_ptr = &s_cdpList;
   while( *cdp_ptr )
   {
      if( strcmp( ( *cdp_ptr )->id, id ) == 0 )
         break;
      cdp_ptr = &( *cdp_ptr )->next;
   }
   return cdp_ptr;
}

BOOL hb_cdpRegisterRaw( PHB_CODEPAGE cdp )
{
   PHB_CODEPAGE * cdp_ptr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpRegisterRaw(%p)", cdp ) );

   cdp_ptr = hb_cdpFindPos( cdp->id );
   if( *cdp_ptr == NULL )
   {
      *cdp_ptr = cdp;
      return TRUE;
   }
   return FALSE;
}

BOOL hb_cdpRegisterNew( const char * id, const char * info,
                        PHB_UNITABLE uniTable,
                        const char * pszUpper, const char * pszLower,
                        unsigned int nACSort )
{
   PHB_CODEPAGE * cdp_ptr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpRegisterNew(%s,%s,%s,%s,%d)", id, info, pszUpper, pszLower, nACSort ) );

   cdp_ptr = hb_cdpFindPos( id );
   if( *cdp_ptr == NULL )
   {
      *cdp_ptr = hb_buildCodePage( id, info, uniTable, pszUpper, pszLower, nACSort );
      return *cdp_ptr != NULL;
   }
   return FALSE;
}

void hb_cdpReleaseAll( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpReleaseAll()" ) );

   while( s_cdpList )
   {
      void * buffer = s_cdpList->buffer;
      if( s_cdpList->uniTable->uniTrans )
      {
         hb_xfree( s_cdpList->uniTable->uniTrans );
         s_cdpList->uniTable->uniTrans = NULL;
      }
      s_cdpList = s_cdpList->next;
      if( buffer )
         hb_xfree( buffer );
   }
}

PHB_CODEPAGE hb_cdpFind( const char * id )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpFind(%s)", id ) );

   return id ? * hb_cdpFindPos( id ) : NULL;
}

PHB_CODEPAGE hb_cdpFindExt( const char * id )
{
   PHB_CODEPAGE cdp = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpFindExt(%s)", id ) );

   if( id )
   {
      cdp = * hb_cdpFindPos( id );
      if( cdp == NULL && strcmp( id, "UTF8" ) == 0 )
         return &s_utf8_codepage;
   }
   return cdp;
}

PHB_CODEPAGE hb_cdpSelect( PHB_CODEPAGE cdp )
{
   PHB_CODEPAGE cdpOld;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpSelect(%p)", cdp ) );

   cdpOld = hb_vmCDP();
   if( cdp )
      hb_vmSetCDP( cdp );

   return cdpOld;
}

const char * hb_cdpID( void )
{
   PHB_CODEPAGE cdp;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpID()" ) );

   cdp = hb_vmCDP();

   return cdp ? cdp->id : NULL;
}

const char * hb_cdpSelectID( const char * id )
{
   const char * idOld;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpSelectID(%s)", id ) );

   idOld = hb_cdpID();
   hb_cdpSelect( hb_cdpFind( id ) );

   return idOld;
}

/*
 * basic CP PRG functions
 */
HB_FUNC( HB_CDPLIST )
{
   PHB_CODEPAGE cdp;
   int iCount;

   cdp = s_cdpList;
   iCount = 0;
   while( cdp )
   {
      ++iCount;
      cdp = cdp->next;
   }

   hb_reta( iCount );
   cdp = s_cdpList;
   iCount = 0;
   while( cdp )
   {
      hb_storvc( cdp->id, -1, ++iCount );
      cdp = cdp->next;
   }
}

HB_FUNC( HB_CDPSELECT )
{
   hb_retc( hb_cdpID() );

   if( HB_ISCHAR( 1 ) )
      hb_cdpSelectID( hb_parc( 1 ) );
}

HB_FUNC( HB_CDPUNIID )
{
   const char * id = hb_parc( 1 );
   PHB_CODEPAGE cdp = id ? *hb_cdpFindPos( id ) : hb_vmCDP();

   hb_retc( cdp ? cdp->uniTable->uniID : NULL );
}

HB_FUNC( HB_CDPINFO )
{
   const char * id = hb_parc( 1 );
   PHB_CODEPAGE cdp = id ? *hb_cdpFindPos( id ) : hb_vmCDP();

   hb_retc( cdp ? cdp->info : NULL );
}

HB_FUNC( __NATSORTVER )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATSORT v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATSORT v1.3i x19 06/Mar/95" */

   HB_FUNC_EXEC( HB_CDPINFO );
}

HB_FUNC( HB_SETCODEPAGE )
{
   HB_FUNC_EXEC( HB_CDPSELECT );
}

/*
 * extended CP PRG functions
 */
HB_FUNC( HB_TRANSLATE )
{
   ULONG ulLen = hb_parclen( 1 );
   const char *szIdIn = hb_parc( 2 );
   const char *szIdOut = hb_parc( 3 );

   if( ulLen && ( szIdIn || szIdOut ) )
   {
      PHB_CODEPAGE cdpIn = szIdIn ? hb_cdpFindExt( szIdIn ) : hb_vmCDP();
      PHB_CODEPAGE cdpOut = szIdOut ? hb_cdpFindExt( szIdOut ) : hb_vmCDP();

      if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable )
      {
         char *szResult = hb_cdpnDup( hb_parc( 1 ), &ulLen, cdpIn, cdpOut );
         hb_retclen_buffer( szResult, ulLen );
      }
      else
         hb_itemReturn( hb_param( 1, HB_IT_STRING ) );
   }
   else
      hb_retc_null();
}

HB_FUNC( HB_UTF8CHR )
{
   if( HB_ISNUM( 1 ) )
   {
      char utf8Char[ HB_MAX_UTF8 ];
      int iLen;

      iLen = u16toutf8( utf8Char, ( HB_WCHAR ) hb_parni( 1 ) );
      hb_retclen( utf8Char, iLen );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8ASC )
{
   const char *pszString = hb_parc( 1 );

   if( pszString )
   {
      ULONG ulLen = hb_parclen( 1 );
      HB_WCHAR wc = 0;
      int n = 0;

      while( ulLen-- )
      {
         if( utf8tou16nextchar( ( unsigned char ) *pszString++, &n, &wc ) )
         {
            if( n == 0 )
               break;
         }
      }
      hb_retnint( wc );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_STRTOUTF8 )
{
   ULONG ulLen = hb_parclen( 1 ), ulDest = 0;
   const char *szString;
   char *szDest = NULL;

   if( ulLen )
   {
      PHB_CODEPAGE cdp = HB_ISCHAR( 2 ) ? hb_cdpFind( hb_parc( 2 ) ) : hb_vmCDP();

      if( cdp )
      {
         szString = hb_parc( 1 );
         ulDest = hb_cdpStrAsUTF8Len( cdp, FALSE, szString, ulLen, 0 );
         szDest = ( char * ) hb_xgrab( ulDest + 1 );
         hb_cdpStrToUTF8( cdp, FALSE, szString, ulLen, szDest, ulDest + 1 );
      }
   }
   if( szDest )
      hb_retclen_buffer( szDest, ulDest );
   else
      hb_retc_null();
}

HB_FUNC( HB_UTF8TOSTR )
{
   const char *szString = hb_parc( 1 );

   if( szString )
   {
      ULONG ulLen = hb_parclen( 1 ), ulDest = 0;
      char *szDest = NULL;

      if( ulLen )
      {
         PHB_CODEPAGE cdp = HB_ISCHAR( 2 ) ? hb_cdpFind( hb_parc( 2 ) ) : hb_vmCDP();

         if( cdp )
         {
            szString = hb_parc( 1 );
            ulDest = hb_cdpUTF8AsStrLen( cdp, FALSE, szString, ulLen, 0 );
            szDest = ( char * ) hb_xgrab( ulDest + 1 );
            hb_cdpUTF8ToStr( cdp, FALSE, szString, ulLen, szDest, ulDest + 1 );
         }
      }

      if( szDest )
         hb_retclen_buffer( szDest, ulDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8SUBSTR )
{
   const char *szString = hb_parc( 1 );
   int iPCount = hb_pcount();

   if( szString && ( iPCount < 2 || ( HB_ISNUM( 2 ) && ( iPCount < 3 || HB_ISNUM( 3 ) ) ) ) )
   {
      char *szDest = NULL;
      ULONG ulLen = hb_parclen( 1 ), ulDest = 0;
      LONG lFrom = hb_parnl( 2 );
      LONG lCount = iPCount < 3 ? ( LONG ) ulLen : hb_parnl( 3 );

      if( lFrom < 0 )
      {
         lFrom += hb_cdpUTF8StringLength( szString, ulLen );
         if( lFrom < 0 )
            lFrom = 0;
      }
      else if( lFrom )
         --lFrom;

      if( ulLen && lCount > 0 )
         szDest = hb_cdpUTF8StringSubstr( szString, ulLen,
                                          lFrom, lCount, &ulDest );
      if( szDest )
         hb_retclen_buffer( szDest, ulDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8LEFT )
{
   const char *szString = hb_parc( 1 );

   if( szString && HB_ISNUM( 2 ) )
   {
      LONG lLen = hb_parnl( 2 );
      ULONG ulDest = 0;
      char *szDest = NULL;

      if( lLen > 0 )
         szDest = hb_cdpUTF8StringSubstr( szString, hb_parclen( 1 ),
                                          0, lLen, &ulDest );

      if( szDest )
         hb_retclen_buffer( szDest, ulDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8RIGHT )
{
   const char *szString = hb_parc( 1 );

   if( szString && HB_ISNUM( 2 ) )
   {
      LONG lLen = hb_parnl( 2 ), lFrom;
      ULONG ulLen = hb_parclen( 1 ), ulDest = 0;
      char *szDest = NULL;

      if( ulLen && lLen > 0 )
      {
         lFrom = hb_cdpUTF8StringLength( szString, ulLen ) - lLen;
         if( lFrom < 0 )
            lFrom = 0;
         szDest = hb_cdpUTF8StringSubstr( szString, ulLen,
                                          lFrom, lLen, &ulDest );
      }

      if( szDest )
         hb_retclen_buffer( szDest, ulDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8PEEK )
{
   const char *szString = hb_parc( 1 );

   if( szString && HB_ISNUM( 2 ) )
   {
      ULONG ulPos = hb_parnl( 2 );
      ULONG ulLen = hb_parclen( 1 );

      if( ulPos > 0 && ulPos <= ulLen )
         hb_retnint( hb_cdpUTF8StringPeek( szString, ulLen, ulPos - 1 ) );
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8POKE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      const char *szString = hb_itemGetCPtr( pText );
      ULONG ulLen = hb_parclen( 1 ), ulPos;

      ulPos = utf8pos( szString, ulLen, hb_parnl( 2 ) );
      if( ulPos )
      {
         HB_WCHAR uc, uc2;
         int n, n2;

         --ulPos;
         uc = ( HB_WCHAR ) hb_parni( 3 );
         n = utf8Size( uc );
         n2 = 0;
         utf8tou16nextchar( szString[ulPos], &n2, &uc2 );
         ++n2;
         if( n == n2 )
         {
            char * szText;
            if( hb_itemGetWriteCL( pText, &szText, &ulLen ) &&
                ulPos + n <= ulLen )
            {
               u16toutf8( &szText[ulPos], uc );
            }
            hb_itemReturn( pText );
         }
         else
         {
            char *szResult = ( char * ) hb_xgrab( ulLen - n2 + n + 1 );

            memcpy( szResult, szString, ulPos );
            u16toutf8( &szResult[ulPos], uc );
            memcpy( szResult + ulPos + n, szString + ulPos + n2, ulLen - ulPos - n2 );
            if( HB_ISBYREF( 1 ) )
               hb_storclen( szResult, ulLen - n2 + n, 1 );
            hb_retclen_buffer( szResult, ulLen - n2 + n );
         }
      }
      else
         hb_itemReturn( pText );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8STUFF )
{
   const char *szString = hb_parc( 1 );

   if( szString && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISCHAR( 4 ) )
   {
      ULONG ulLen = hb_parclen( 1 );
      ULONG ulPos = hb_parnl( 2 );
      ULONG ulDel = hb_parnl( 3 );
      ULONG ulIns = hb_parclen( 4 );
      ULONG ulTot;

      if( ulPos )
      {
         ulPos = utf8pos( szString, ulLen, ulPos );
         if( ulPos == 0 )
            ulPos = ulLen;
         else
            ulPos--;
      }
      if( ulDel )
      {
         if( ulPos < ulLen )
         {
            ulDel = utf8pos( szString + ulPos, ulLen - ulPos, ulDel + 1 );
            if( ulDel == 0 )
               ulDel = ulLen - ulPos;
            else
               ulDel--;
         }
         else
            ulDel = 0;
      }

      if( ( ulTot = ulLen + ulIns - ulDel ) > 0 )
      {
         char *szResult = ( char * ) hb_xgrab( ulTot + 1 );

         hb_xmemcpy( szResult, szString, ulPos );
         hb_xmemcpy( szResult + ulPos, hb_parc( 4 ), ulIns );
         hb_xmemcpy( szResult + ulPos + ulIns, szString + ulPos + ulDel,
                     ulLen - ( ulPos + ulDel ) );
         hb_retclen_buffer( szResult, ulTot );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTF8LEN )
{
   const char *szString = hb_parc( 1 );

   if( szString )
      hb_retnint( hb_cdpUTF8StringLength( szString, hb_parclen( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* none of numeric parameters in STRTRAN() (4-th and 5-th) refers to
 * character position in string so we do not need to create new
 * HB_UTF8STRTRAN() but we can safely use normal STRTRAN() function
 */
HB_FUNC_EXTERN( STRTRAN );

HB_FUNC( HB_UTF8STRTRAN )
{
   HB_FUNC_EXEC( STRTRAN )
}

#ifdef HB_LEGACY_LEVEL2
void hb_cdpnTranslate( char *psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, ULONG nChars )
{
   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable )
   {
      ULONG ulDst = nChars;
      char * pDst = psz;

      if( cdpOut == &s_utf8_codepage || cdpOut->nMultiUC )
      {
         ulDst = hb_cdpTransLen( psz, nChars, 0, cdpIn, cdpOut );
         pDst = ( char * ) hb_xgrab( ulDst );
      }
      ulDst = hb_cdpTransTo( psz, nChars, pDst, ulDst, cdpIn, cdpOut );
      if( psz != pDst )
      {
         if( ulDst > nChars )
            ulDst = nChars;
         memcpy( psz, pDst, ulDst );
         hb_xfree( pDst );
      }
      if( ulDst < nChars )
         memset( psz + ulDst, '\0', nChars - ulDst );
   }
}

void hb_cdpTranslate( char *psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   hb_cdpnTranslate( psz, cdpIn, cdpOut, strlen( psz ) );
}
#endif
