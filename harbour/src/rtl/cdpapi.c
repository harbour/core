/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The CodePages API
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

#include "hbapi.h"
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


/* MT macros */
#define HB_CDP_LOCK           hb_threadEnterCriticalSection( &s_cdpMtx );
#define HB_CDP_UNLOCK         hb_threadLeaveCriticalSection( &s_cdpMtx );
static HB_CRITICAL_NEW( s_cdpMtx );


#define NUMBER_OF_CHARS    256

static const HB_WCHAR s_uniCtrls[ 32 ] =
{
   0x2007, 0x263A, 0x263B, 0x2665, 0x2666, 0x2663, 0x2660, 0x2022,
   0x25D8, 0x25CB, 0x25D9, 0x2642, 0x2640, 0x266A, 0x266B, 0x263C,
   0x25BA, 0x25C4, 0x2195, 0x203C, 0x00B6, 0x00A7, 0x25AC, 0x21A8,
   0x2191, 0x2193, 0x2192, 0x2190, 0x221F, 0x2194, 0x25B2, 0x25BC
};

static const HB_WCHAR s_uniCodes[ NUMBER_OF_CHARS ] =
{
   0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
   0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
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
HB_BOOL hb_cdpIsDigit( PHB_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_DIGIT ) != 0;
   else
      return HB_ISDIGIT( iChar );
}

HB_BOOL hb_cdpIsAlpha( PHB_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_ALPHA ) != 0;
   else
      return HB_ISALPHA( iChar );
}

HB_BOOL hb_cdpIsLower( PHB_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_LOWER ) != 0;
   else
      return HB_ISLOWER( iChar );
}

HB_BOOL hb_cdpIsUpper( PHB_CODEPAGE cdp, int iChar )
{
   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_UPPER ) != 0;
   else
      return HB_ISUPPER( iChar );
}

HB_BOOL hb_charIsDigit( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_DIGIT ) != 0;
   else
      return HB_ISDIGIT( iChar );
}

HB_BOOL hb_charIsAlpha( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_ALPHA ) != 0;
   else
      return HB_ISALPHA( iChar );
}

HB_BOOL hb_charIsLower( int iChar )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp )
      return ( cdp->flags[ iChar & 0x0ff ] & HB_CDP_LOWER ) != 0;
   else
      return HB_ISLOWER( iChar );
}

HB_BOOL hb_charIsUpper( int iChar )
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

char * hb_strLower( char * szText, HB_SIZE nLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strLower(%s, %" HB_PFS "u)", szText, nLen));

   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE u;

      if( cdp )
         for( u = 0; u < nLen; u++ )
            szText[ u ] = ( char ) cdp->lower[ ( HB_UCHAR ) szText[ u ] ];
      else
         for( u = 0; u < nLen; u++ )
            szText[ u ] = HB_TOLOWER( szText[ u ] );
   }

   return szText;
}

char * hb_strUpper( char * szText, HB_SIZE nLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strUpper(%s, %" HB_PFS "u)", szText, nLen));

   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE u;

      if( cdp )
         for( u = 0; u < nLen; u++ )
            szText[ u ] = ( char ) cdp->upper[ ( HB_UCHAR ) szText[ u ] ];
      else
         for( u = 0; u < nLen; u++ )
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
      int n1 = cdp->sort[ ( HB_UCHAR ) cFirst ],
          n2 = cdp->sort[ ( HB_UCHAR ) cSecond ];

      if( cdp->nMulti == 0 || ( n1 != 0 && n2 != 0 ) )
      {
         if( n1 == n2 )
         {
            if( cdp->acc )
            {
               n1 = cdp->acc[ ( HB_UCHAR ) cFirst ];
               n2 = cdp->acc[ ( HB_UCHAR ) cSecond ];
            }
            else
               return 0;
         }
         return ( n1 < n2 ) ? -1 : 1;
      }
   }

   return ( ( HB_UCHAR ) cFirst < ( HB_UCHAR ) cSecond ) ? -1 : 1;
}

static int hb_cdpMultiWeight( PHB_CODEPAGE cdp, const char * szChar )
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
         return ( szChar[ 0 ] == pmulti->cFirst[ 0 ] ) ?
                pmulti->sortUp : pmulti->sortLo;
      }
   }

   return 0;
}

int hb_cdpcmp( const char * szFirst, HB_SIZE nLenFirst,
               const char * szSecond, HB_SIZE nLenSecond,
               PHB_CODEPAGE cdp, HB_BOOL fExact )
{
   int iRet = 0, iAcc = 0, n, n1, n2;
   HB_SIZE ul, nLen;

   nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
   if( cdp->sort == NULL )
   {
      iRet = memcmp( szFirst, szSecond, nLen );
   }
   else if( cdp->nMulti )
   {
      for( ul = 0; ul < nLen; ++szFirst, ++szSecond, ++ul )
      {
         HB_UCHAR u1 = ( HB_UCHAR ) * szFirst;
         HB_UCHAR u2 = ( HB_UCHAR ) * szSecond;

         n1 = cdp->sort[ u1 ];
         if( ( cdp->flags[ u1 ] & HB_CDP_MULTI1 ) != 0 &&
             ( ul < nLenFirst - 1 ) &&
             ( cdp->flags[ ( HB_UCHAR ) szFirst[ 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            n = hb_cdpMultiWeight( cdp, szFirst );
            if( n != 0 )
            {
               n1 = n;
               ++szFirst;
               if( --nLenFirst < nLen )
                  nLen = nLenFirst;
            }
         }
         n2 = cdp->sort[ u2 ];
         if( ( cdp->flags[ u2 ] & HB_CDP_MULTI1 ) != 0 &&
             ( ul < nLenSecond - 1 ) &&
             ( cdp->flags[ ( HB_UCHAR ) szSecond[ 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            n = hb_cdpMultiWeight( cdp, szSecond );
            if( n != 0 )
            {
               n2 = n;
               ++szSecond;
               if( --nLenSecond < nLen )
                  nLen = nLenSecond;
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
            if( iAcc == 0 && ( fExact || ( nLenFirst == nLenSecond && cdp->acc ) ) )
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
      for( ul = 0; ul < nLen; ++szFirst, ++szSecond, ++ul )
      {
         if( *szFirst != *szSecond )
         {
            n1 = ( HB_UCHAR ) cdp->sort[ ( HB_UCHAR ) * szFirst ];
            n2 = ( HB_UCHAR ) cdp->sort[ ( HB_UCHAR ) * szSecond ];
            if( n1 != n2 )
            {
               iRet = ( n1 < n2 ) ? -1 : 1;
               break;
            }
            if( iAcc == 0 && ( fExact || ( nLenFirst == nLenSecond && cdp->acc ) ) )
            {
               if( cdp->acc )
                  iAcc = ( cdp->acc[ ( HB_UCHAR ) * szFirst ] <
                           cdp->acc[ ( HB_UCHAR ) * szSecond ] ) ? -1 : 1;
               else
                  iAcc = ( ( HB_UCHAR ) * szFirst < ( HB_UCHAR ) * szSecond ) ? -1 : 1;
            }
         }
      }
   }

   if( !iRet )
   {
      if( iAcc )
         iRet = iAcc;
      else if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }

   return iRet;
}

static int hb_cdpMultiWeightI( PHB_CODEPAGE cdp, const char * szChar )
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

int hb_cdpicmp( const char * szFirst, HB_SIZE nLenFirst,
                const char * szSecond, HB_SIZE nLenSecond,
                PHB_CODEPAGE cdp, HB_BOOL fExact )
{
   int iRet = 0, iAcc = 0, n, n1, n2, u1, u2;
   HB_SIZE ul, nLen;

   nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
   if( cdp->sort == NULL )
   {
      for( ul = 0; ul < nLen; ++szFirst, ++szSecond, ++ul )
      {
         u1 = cdp->upper[ ( HB_UCHAR ) * szFirst ];
         u2 = cdp->upper[ ( HB_UCHAR ) * szSecond ];
         if( u1 != u2 )
         {
            iRet = ( u1 < u2 ) ? -1 : 1;
            break;
         }
      }
   }
   else if( cdp->nMulti )
   {
      for( ul = 0; ul < nLen; ++szFirst, ++szSecond, ++ul )
      {
         u1 = cdp->upper[ ( HB_UCHAR ) * szFirst ];
         u2 = cdp->upper[ ( HB_UCHAR ) * szSecond ];

         if( ( cdp->flags[ u1 ] & HB_CDP_MULTI1 ) != 0 &&
             ( ul < nLenFirst - 1 ) &&
             ( cdp->flags[ ( HB_UCHAR ) szFirst[ 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            n = hb_cdpMultiWeightI( cdp, szFirst );
            if( n != 0 )
            {
               n1 = n;
               ++szFirst;
               if( --nLenFirst < nLen )
                  nLen = nLenFirst;
            }
            else
               n1 = cdp->sort[ u1 ];
         }
         else
            n1 = cdp->sort[ u1 ];

         if( ( cdp->flags[ u2 ] & HB_CDP_MULTI1 ) != 0 &&
             ( ul < nLenSecond - 1 ) &&
             ( cdp->flags[ ( HB_UCHAR ) szSecond[ 1 ] ] & HB_CDP_MULTI2 ) != 0 )
         {
            n = hb_cdpMultiWeightI( cdp, szSecond );
            if( n != 0 )
            {
               n2 = n;
               ++szSecond;
               if( --nLenSecond < nLen )
                  nLen = nLenSecond;
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
            if( iAcc == 0 && ( fExact || ( nLenFirst == nLenSecond && cdp->acc ) ) )
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
      for( ul = 0; ul < nLen; ++szFirst, ++szSecond, ++ul )
      {
         u1 = cdp->upper[ ( HB_UCHAR ) * szFirst ];
         u2 = cdp->upper[ ( HB_UCHAR ) * szSecond ];

         if( u1 != u2 )
         {
            n1 = ( HB_UCHAR ) cdp->sort[ u1 ];
            n2 = ( HB_UCHAR ) cdp->sort[ u2 ];
            if( n1 != n2 )
            {
               iRet = ( n1 < n2 ) ? -1 : 1;
               break;
            }
            if( iAcc == 0 && ( fExact || ( nLenFirst == nLenSecond && cdp->acc ) ) )
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
      else if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
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
      HB_UCHAR * uniTrans;

      for( i = 0; i < 256; ++i )
      {
         HB_WCHAR wc = uniTable->uniCodes[ i ];
         if( wc > wcMax )
            wcMax = wc;
      }
      uniTrans = ( HB_UCHAR * )
                           hb_xgrab( ( wcMax + 1 ) * sizeof( HB_UCHAR ) );
      memset( uniTrans, '\0', ( wcMax + 1 ) * sizeof( HB_UCHAR ) );
      for( i = 0; i < 256; ++i )
         uniTrans[ uniTable->uniCodes[ i ] ] = ( HB_UCHAR ) i;

      uniTable->wcMax = wcMax;
      uniTable->uniTrans = uniTrans;
   }
   HB_CDP_UNLOCK
}

/*
 * UTF8 conversions
 */
int hb_cdpUTF8CharSize( HB_WCHAR wc )
{
   if( wc < 0x0080 )
      return 1;
   else if( wc < 0x0800 )
      return 2;
   else                         /* if( wc <= 0xffff ) */
      return 3;
}

int hb_cdpU16CharToUTF8( char * szUTF8, HB_WCHAR wc )
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

HB_BOOL hb_cdpUTF8ToU16NextChar( HB_UCHAR ucChar, int * n, HB_WCHAR * pwc )
{
   if( *n > 0 )
   {
      if( ( ucChar & 0xc0 ) != 0x80 )
         return HB_FALSE;
      *pwc = ( *pwc << 6 ) | ( ucChar & 0x3f );
      ( *n )--;
      return HB_TRUE;
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
   return HB_TRUE;
}

HB_SIZE hb_cdpUTF8StringLength( const char * pSrc, HB_SIZE nLen )
{
   HB_SIZE ul, nDst;
   HB_WCHAR wc;
   int n = 0;

   for( ul = nDst = 0; ul < nLen; ++ul )
   {
      if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ ul ], &n, &wc ) )
      {
         if( n == 0 )
            ++nDst;
      }
   }

   return nDst;
}

HB_SIZE hb_cdpUTF8StringAt( const char * szNeedle, HB_SIZE nLenN,
                            const char * szHaystack, HB_SIZE nLenH,
                            HB_SIZE nStart, HB_SIZE nEnd, HB_BOOL fReverse )
{
   HB_SIZE nPosN = 0;
   HB_SIZE nPosH = 0;
   HB_SIZE nPosX = 0;
   HB_SIZE nPos = 0;
   HB_SIZE nRAt = 0;
   HB_SIZE nAt = 0;

   HB_WCHAR wcN = 0;
   HB_WCHAR wcH = 0;
   int nN = 0;
   int nH = 0;

   while( nPosH < nLenH && nPosN < nLenN && nPos < nEnd )
   {
      do
      {
         if( !hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) szHaystack[ nPosH++ ], &nH, &wcH ) )
         {
            --nPosH;
            nH = 0;
         }
      } while( nH );

      if( ++nPos < nStart )
         continue;

      do
      {
         if( !hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) szNeedle[ nPosN++ ], &nN, &wcN ) )
         {
            --nPosN;
            nN = 0;
         }
      } while( nN );

      if( wcH == wcN )
      {
         if( nAt == 0 )
         {
            nAt = nPos;
            nPosX = nPosH;
         }

         if( nPosN == nLenN )
         {
            if( fReverse )
            {
               nRAt = nAt;
               nPos = nAt;
               nAt = 0;
               nPosH = nPosX;
               nPosX = 0;
               nPosN = 0;
            }
            else
               return nAt;
         }
      }
      else
      {
         if( nAt )
         {
            nPos = nAt;
            nAt = 0;
            nPosH = nPosX;
            nPosX = 0;
         }
         nPosN = 0;
      }
   }

   return nRAt;
}

HB_SIZE hb_cdpUTF8StringPeek( const char * pSrc, HB_SIZE nLen, HB_SIZE nPos )
{
   if( nLen )
   {
      HB_SIZE ul;
      HB_WCHAR wc = 0;
      int n = 0;

      for( ul = 0; ul < nLen && nPos; ++ul )
      {
         if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ ul ], &n, &wc ) )
         {
            if( n == 0 )
               --nPos;
         }
      }

      if( ul < nLen )
      {
         n = 0;
         do
         {
            if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ ul ], &n, &wc ) )
            {
               if( n == 0 )
                  return wc;
            }
         }
         while( ++ul < nLen );
      }
   }

   return 0;
}

/* caller must free the returned buffer if not NULL */
char * hb_cdpUTF8StringSubstr( const char * pSrc, HB_SIZE nLen,
                               HB_SIZE nFrom, HB_SIZE nCount, HB_SIZE * pulDest )
{
   HB_SIZE ul, nCnt, nDst = 0;
   HB_WCHAR wc;
   int n;
   char * pDst = NULL;

   if( nCount && nLen )
   {
      n = 0;
      for( ul = 0; ul < nLen && nFrom; ++ul )
      {
         if( hb_cdpUTF8ToU16NextChar( pSrc[ ul ], &n, &wc ) )
         {
            if( n == 0 )
               --nFrom;
         }
      }

      if( ul < nLen )
      {
         nFrom = ul;
         nCnt = nCount;
         n = 0;
         do
         {
            if( hb_cdpUTF8ToU16NextChar( pSrc[ ul ], &n, &wc ) )
            {
               if( n == 0 )
                  --nCnt;
            }
         }
         while( ++ul < nLen && nCnt );

         nDst = ul - nFrom;
         pDst = ( char * ) hb_xgrab( nDst + 1 );
         memcpy( pDst, &pSrc[ nFrom ], nDst );
         pDst[ nDst ] = '\0';
      }
   }

   if( pulDest )
      *pulDest = nDst;

   return pDst;
}

HB_BOOL hb_cdpGetFromUTF8( PHB_CODEPAGE cdp, HB_UCHAR ch,
                           int * n, HB_WCHAR * pwc )
{
   if( hb_cdpUTF8ToU16NextChar( ch, n, pwc ) )
   {
      if( *n == 0 && cdp )
      {
         if( cdp->uniTable->uniTrans == NULL )
            hb_cdpBuildTransTable( cdp->uniTable );

         if( *pwc <= cdp->uniTable->wcMax )
         {
            HB_UCHAR uc = cdp->uniTable->uniTrans[ *pwc ];
            if( uc )
               *pwc = uc;
         }
      }
      return HB_TRUE;
   }
   return HB_FALSE;
}

HB_SIZE hb_cdpStrAsUTF8Len( PHB_CODEPAGE cdp,
                            const char * pSrc, HB_SIZE nSrc,
                            HB_SIZE nMax )
{
   const HB_WCHAR * uniCodes;
   HB_SIZE ulS, ulD, u;
   int i;

   if( cdp == &s_utf8_codepage )
      return ( nMax && nSrc > nMax ) ? nMax : nSrc;

   uniCodes = cdp->uniTable->uniCodes;
   for( ulS = ulD = 0; ulS < nSrc; ++ulS )
   {
      HB_UCHAR uc = ( HB_UCHAR ) pSrc[ ulS ];
      HB_WCHAR wc = uniCodes[ uc ];

      if( cdp->nMultiUC &&
          ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
          ulS + 1 < nSrc )
      {
         HB_UCHAR uc2 = ( HB_UCHAR ) pSrc[ ulS + 1 ];
         if( ( cdp->flags[ uc2 ] & HB_CDP_MULTI2 ) != 0 )
         {
            for( i = 0; i < cdp->nMulti; ++i )
            {
               if( uc2 == cdp->multi[ i ].cLast[ 0 ] ||
                   uc2 == cdp->multi[ i ].cLast[ 1 ] )
               {
                  if( uc == cdp->multi[ i ].cFirst[ 0 ] )
                  {
                     if( cdp->multi[ i ].wcUp )
                     {
                        wc = cdp->multi[ i ].wcUp;
                        ++ulS;
                     }
                     break;
                  }
                  else if( uc == cdp->multi[ i ].cFirst[ 1 ] )
                  {
                     if( cdp->multi[ i ].wcLo )
                     {
                        wc = cdp->multi[ i ].wcLo;
                        ++ulS;
                     }
                     break;
                  }
               }
            }
         }
      }
      if( wc == 0 )
         wc = uc;
      u = hb_cdpUTF8CharSize( wc );
      if( nMax && ulD + u > nMax )
         break;
      ulD += u;
   }

   return ulD;
}

HB_SIZE hb_cdpStrToUTF8( PHB_CODEPAGE cdp,
                         const char * pSrc, HB_SIZE nSrc,
                         char * pDst, HB_SIZE nDst )
{
   const HB_WCHAR * uniCodes;
   HB_SIZE ulS, ulD, u;
   int i;

   if( cdp == &s_utf8_codepage )
   {
      if( nSrc > nDst )
         nSrc = nDst;
      else if( nSrc < nDst )
         pDst[ nSrc ] = '\0';
      memcpy( pDst, pSrc, nSrc );
      return nSrc;
   }

   uniCodes = cdp->uniTable->uniCodes;
   for( ulS = ulD = 0; ulS < nSrc && ulD < nDst; ++ulS )
   {
      HB_UCHAR uc = ( HB_UCHAR ) pSrc[ ulS ];
      HB_WCHAR wc = uniCodes[ uc ];

      if( cdp->nMultiUC &&
          ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
          ulS + 1 < nSrc )
      {
         HB_UCHAR uc2 = ( HB_UCHAR ) pSrc[ ulS + 1 ];
         if( ( cdp->flags[ uc2 ] & HB_CDP_MULTI2 ) != 0 )
         {
            for( i = 0; i < cdp->nMulti; ++i )
            {
               if( uc2 == cdp->multi[ i ].cLast[ 0 ] ||
                   uc2 == cdp->multi[ i ].cLast[ 1 ] )
               {
                  if( uc == cdp->multi[ i ].cFirst[ 0 ] )
                  {
                     if( cdp->multi[ i ].wcUp )
                     {
                        wc = cdp->multi[ i ].wcUp;
                        ++ulS;
                     }
                     break;
                  }
                  else if( uc == cdp->multi[ i ].cFirst[ 1 ] )
                  {
                     if( cdp->multi[ i ].wcLo )
                     {
                        wc = cdp->multi[ i ].wcLo;
                        ++ulS;
                     }
                     break;
                  }
               }
            }
         }
      }
      if( wc == 0 )
         wc = uc;
      u = hb_cdpUTF8CharSize( wc );
      if( ulD + u <= nDst )
      {
         hb_cdpU16CharToUTF8( &pDst[ ulD ], wc );
         ulD += u;
      }
      else
         break;
   }
   if( ulD < nDst )
      pDst[ ulD ] = '\0';

   return ulD;
}

HB_SIZE hb_cdpStrToUTF8Disp( PHB_CODEPAGE cdp,
                             const char * pSrc, HB_SIZE nSrc,
                             char * pDst, HB_SIZE nDst )
{
   const HB_WCHAR * uniCodes;
   HB_SIZE ulS, ulD, u;
   int i;

   if( cdp == &s_utf8_codepage )
   {
      if( nSrc > nDst )
         nSrc = nDst;
      else if( nSrc < nDst )
         pDst[ nSrc ] = '\0';
      memcpy( pDst, pSrc, nSrc );
      return nSrc;
   }

   uniCodes = cdp->uniTable->uniCodes;
   for( ulS = ulD = 0; ulS < nSrc && ulD < nDst; ++ulS )
   {
      HB_UCHAR uc = ( HB_UCHAR ) pSrc[ ulS ];
      HB_WCHAR wc = uniCodes[ uc ];

      if( cdp->nMultiUC &&
          ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
          ulS + 1 < nSrc )
      {
         HB_UCHAR uc2 = ( HB_UCHAR ) pSrc[ ulS + 1 ];
         if( ( cdp->flags[ uc2 ] & HB_CDP_MULTI2 ) != 0 )
         {
            for( i = 0; i < cdp->nMulti; ++i )
            {
               if( uc2 == cdp->multi[ i ].cLast[ 0 ] ||
                   uc2 == cdp->multi[ i ].cLast[ 1 ] )
               {
                  if( uc == cdp->multi[ i ].cFirst[ 0 ] )
                  {
                     if( cdp->multi[ i ].wcUp )
                     {
                        wc = cdp->multi[ i ].wcUp;
                        ++ulS;
                     }
                     break;
                  }
                  else if( uc == cdp->multi[ i ].cFirst[ 1 ] )
                  {
                     if( cdp->multi[ i ].wcLo )
                     {
                        wc = cdp->multi[ i ].wcLo;
                        ++ulS;
                     }
                     break;
                  }
               }
            }
         }
      }
      if( wc == 0 )
         wc = uc < 32 ? s_uniCtrls[ uc ] : s_uniCodes[ uc ];

      u = hb_cdpUTF8CharSize( wc );
      if( ulD + u <= nDst )
      {
         hb_cdpU16CharToUTF8( &pDst[ ulD ], wc );
         ulD += u;
      }
      else
         break;
   }
   if( ulD < nDst )
      pDst[ ulD ] = '\0';

   return ulD;
}

HB_SIZE hb_cdpUTF8AsStrLen( PHB_CODEPAGE cdp, const char * pSrc, HB_SIZE nSrc,
                            HB_SIZE nMax )
{
   HB_UCHAR * uniTrans;
   HB_WCHAR wcMax, wc = 0;
   HB_SIZE ulS, ulD;
   int n = 0, i;

   if( cdp == &s_utf8_codepage )
      return ( nMax && nSrc > nMax ) ? nMax : nSrc;

   if( cdp->uniTable->uniTrans == NULL )
      hb_cdpBuildTransTable( cdp->uniTable );
   uniTrans = cdp->uniTable->uniTrans;
   wcMax = cdp->uniTable->wcMax;

   for( ulS = ulD = 0; ulS < nSrc; ++ulS )
   {
      if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ ulS ], &n, &wc ) )
      {
         if( n == 0 )
         {
            ++ulD;
            if( nMax && ulD >= nMax )
               break;
            if( wc && cdp->nMultiUC && ( wc > wcMax || uniTrans[ wc ] == 0 ) )
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
               if( nMax && ulD >= nMax )
                  break;
            }
         }
      }
   }

   return ulD;
}

HB_SIZE hb_cdpUTF8ToStr( PHB_CODEPAGE cdp, 
                         const char * pSrc, HB_SIZE nSrc,
                         char * pDst, HB_SIZE nDst )
{
   HB_UCHAR * uniTrans;
   HB_WCHAR wcMax, wc = 0;
   HB_SIZE ulS, ulD;
   int n = 0, i;

   if( cdp == &s_utf8_codepage )
   {
      if( nSrc > nDst )
         nSrc = nDst;
      else if( nSrc < nDst )
         pDst[ nSrc ] = '\0';
      memcpy( pDst, pSrc, nSrc );
      return nSrc;
   }

   if( cdp->uniTable->uniTrans == NULL )
      hb_cdpBuildTransTable( cdp->uniTable );
   uniTrans = cdp->uniTable->uniTrans;
   wcMax = cdp->uniTable->wcMax;

   for( ulS = ulD = 0; ulS < nSrc && ulD < nDst; ++ulS )
   {
      if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ ulS ], &n, &wc ) )
      {
         if( n == 0 )
         {
            if( wc <= wcMax && uniTrans[ wc ] )
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
                        if( ulD < nDst )
                           pDst[ ulD++ ] = cdp->multi[ i ].cLast[ 0 ];
                        break;
                     }
                     if( wc == cdp->multi[ i ].wcLo )
                     {
                        pDst[ ulD++ ] = cdp->multi[ i ].cFirst[ 1 ];
                        if( ulD < nDst )
                           pDst[ ulD++ ] = cdp->multi[ i ].cLast[ 1 ];
                        break;
                     }
                  }
                  if( i < cdp->nMulti )
                     continue;
               }
               pDst[ ulD++ ] = wc >= 0x100 ? '?' : ( HB_UCHAR ) wc;
            }
         }
      }
   }

   if( ulD < nDst )
      pDst[ ulD ] = '\0';

   return ulD;
}

/*
 * U16 (hb wide char) conversions
 */
HB_WCHAR hb_cdpGetU16( PHB_CODEPAGE cdp, HB_UCHAR ch )
{
   if( cdp )
   {
      HB_WCHAR wc = cdp->uniTable->uniCodes[ ch ];
      return wc == 0 ? ch : wc;
   }
   else
      return ch;
}

HB_WCHAR hb_cdpGetU16Disp( PHB_CODEPAGE cdp, HB_UCHAR ch )
{
   if( cdp )
   {
      HB_WCHAR wc = cdp->uniTable->uniCodes[ ch ];
      if( wc == 0 )
         wc = ch < 32 ? s_uniCtrls[ ch ] : s_uniCodes[ ch ];
      return wc;
   }
   else
      return ch;
}

HB_UCHAR hb_cdpGetChar( PHB_CODEPAGE cdp, HB_WCHAR wc )
{
   if( cdp )
   {
      if( cdp->uniTable->uniTrans == NULL )
         hb_cdpBuildTransTable( cdp->uniTable );

      if( wc <= cdp->uniTable->wcMax )
      {
         HB_UCHAR uc = cdp->uniTable->uniTrans[ wc ];
         if( uc )
            wc = uc;
      }
   }
   return wc >= 0x100 ? '?' : ( HB_UCHAR ) wc;
}

HB_SIZE hb_cdpStrAsU16Len( PHB_CODEPAGE cdp,
                           const char * pSrc, HB_SIZE nSrc,
                           HB_SIZE nMax )
{
   if( cdp == &s_utf8_codepage )
   {
      nSrc = hb_cdpUTF8StringLength( pSrc, nSrc );
   }
   else if( cdp->nMultiUC )
   {
      HB_SIZE ulS, ulD;
      int i;

      for( ulS = ulD = 0; ulS < nSrc; ++ulS )
      {
         HB_UCHAR uc = ( HB_UCHAR ) pSrc[ ulS ];

         if( ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
             ulS + 1 < nSrc )
         {
            HB_UCHAR uc2 = ( HB_UCHAR ) pSrc[ ulS + 1 ];
            if( ( cdp->flags[ uc2 ] & HB_CDP_MULTI2 ) != 0 )
            {
               for( i = 0; i < cdp->nMulti; ++i )
               {
                  if( uc2 == cdp->multi[ i ].cLast[ 0 ] ||
                      uc2 == cdp->multi[ i ].cLast[ 1 ] )
                  {
                     if( uc == cdp->multi[ i ].cFirst[ 0 ] )
                     {
                        if( cdp->multi[ i ].wcUp )
                           ++ulS;
                        break;
                     }
                     else if( uc == cdp->multi[ i ].cFirst[ 1 ] )
                     {
                        if( cdp->multi[ i ].wcLo )
                           ++ulS;
                        break;
                     }
                  }
               }
            }
         }
         ++ulD;
         if( nMax && ulD >= nMax )
            break;
      }
      return ulD;
   }

   return ( nMax && nSrc > nMax ) ? nMax : nSrc;
}

#undef HB_CDP_ENDIAN_SWAP
#if defined( HB_BIG_ENDIAN )
#  define HB_CDP_ENDIAN_SWAP  HB_CDP_ENDIAN_LITTLE
#elif defined( HB_LITTLE_ENDIAN )
#  define HB_CDP_ENDIAN_SWAP  HB_CDP_ENDIAN_BIG
#endif

HB_SIZE hb_cdpStrToU16( PHB_CODEPAGE cdp, int iEndian,
                        const char * pSrc, HB_SIZE nSrc,
                        HB_WCHAR * pDst, HB_SIZE nDst )
{
   const HB_WCHAR * uniCodes;
   HB_SIZE ulS, ulD;
   int i;

   if( cdp == &s_utf8_codepage )
   {
      HB_WCHAR wc = 0;
      int n = 0;

      for( ulS = ulD = 0; ulS < nSrc && ulD < nDst; ++ulS )
      {
         if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ ulS ], &n, &wc ) )
         {
            if( n == 0 )
            {
#if defined( HB_CDP_ENDIAN_SWAP )
               if( iEndian == HB_CDP_ENDIAN_SWAP )
                  wc = HB_SWAP_UINT16( wc );
               pDst[ ulD++ ] = wc;
#else
               if( iEndian == HB_CDP_ENDIAN_LITTLE )
                  HB_PUT_LE_UINT16( &pDst[ ulD ], wc );
               else if( iEndian == HB_CDP_ENDIAN_BIG )
                  HB_PUT_BE_UINT16( &pDst[ ulD ], wc );
               else
                  pDst[ ulD ] = wc;
               ++ulD;
#endif
            }
         }
      }
   }
   else
   {
      uniCodes = cdp->uniTable->uniCodes;
      for( ulS = ulD = 0; ulS < nSrc && ulD < nDst; ++ulS )
      {
         HB_UCHAR uc = ( HB_UCHAR ) pSrc[ ulS ];
         HB_WCHAR wc = uniCodes[ uc ];

         if( cdp->nMultiUC &&
             ( cdp->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
             ulS + 1 < nSrc )
         {
            HB_UCHAR uc2 = ( HB_UCHAR ) pSrc[ ulS + 1 ];
            if( ( cdp->flags[ uc2 ] & HB_CDP_MULTI2 ) != 0 )
            {
               for( i = 0; i < cdp->nMulti; ++i )
               {
                  if( uc2 == cdp->multi[ i ].cLast[ 0 ] ||
                      uc2 == cdp->multi[ i ].cLast[ 1 ] )
                  {
                     if( uc == cdp->multi[ i ].cFirst[ 0 ] )
                     {
                        if( cdp->multi[ i ].wcUp )
                        {
                           wc = cdp->multi[ i ].wcUp;
                           ++ulS;
                        }
                        break;
                     }
                     else if( uc == cdp->multi[ i ].cFirst[ 1 ] )
                     {
                        if( cdp->multi[ i ].wcLo )
                        {
                           wc = cdp->multi[ i ].wcLo;
                           ++ulS;
                        }
                        break;
                     }
                  }
               }
            }
         }
         if( wc == 0 )
            wc = uc;
#if defined( HB_CDP_ENDIAN_SWAP )
         if( iEndian == HB_CDP_ENDIAN_SWAP )
            wc = HB_SWAP_UINT16( wc );
         pDst[ ulD++ ] = wc;
#else
         if( iEndian == HB_CDP_ENDIAN_LITTLE )
            HB_PUT_LE_UINT16( &pDst[ ulD ], wc );
         else if( iEndian == HB_CDP_ENDIAN_BIG )
            HB_PUT_BE_UINT16( &pDst[ ulD ], wc );
         else
            pDst[ ulD ] = wc;
         ++ulD;
#endif
      }
   }

   if( ulD < nDst )
      pDst[ ulD ] = '\0';

   return ulD;
}

HB_SIZE hb_cdpU16AsStrLen( PHB_CODEPAGE cdp,
                           const HB_WCHAR * pSrc, HB_SIZE nSrc,
                           HB_SIZE nMax )
{
   HB_UCHAR * uniTrans;
   HB_WCHAR wcMax, wc;
   HB_SIZE ulS, ulD;
   int i;

   if( cdp == &s_utf8_codepage )
   {
      for( ulS = ulD = 0; ulS < nSrc; ++ulS )
      {
         i = hb_cdpUTF8CharSize( pSrc[ ulS ] );
         if( nMax && ulD + i > nMax )
            break;
         ulD += i;
      }
      return ulD;
   }
   else if( cdp->nMultiUC )
   {
      if( cdp->uniTable->uniTrans == NULL )
         hb_cdpBuildTransTable( cdp->uniTable );
      uniTrans = cdp->uniTable->uniTrans;
      wcMax = cdp->uniTable->wcMax;

      for( ulS = ulD = 0; ulS < nSrc; ++ulS )
      {
         wc = pSrc[ ulS ];
         ++ulD;
         if( nMax && ulD >= nMax )
            break;
         if( wc && ( wc > wcMax || uniTrans[ wc ] == 0 ) )
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
            if( nMax && ulD >= nMax )
               break;
         }
      }
   }
   else
      ulD = ( nMax && nSrc > nMax ) ? nMax : nSrc;

   return ulD;
}

HB_SIZE hb_cdpU16ToStr( PHB_CODEPAGE cdp, int iEndian,
                        const HB_WCHAR * pSrc, HB_SIZE nSrc,
                        char * pDst, HB_SIZE nDst )
{
   HB_UCHAR * uniTrans;
   HB_WCHAR wcMax, wc;
   HB_SIZE ulS, ulD;
   int i;

   if( cdp == &s_utf8_codepage )
   {
      for( ulS = ulD = 0; ulS < nSrc; ++ulS )
      {
#if defined( HB_CDP_ENDIAN_SWAP )
         wc = pSrc[ ulS ];
         if( iEndian == HB_CDP_ENDIAN_SWAP )
            wc = HB_SWAP_UINT16( wc );
#else
         if( iEndian == HB_CDP_ENDIAN_LITTLE )
            wc = HB_GET_LE_UINT16( &pSrc[ ulS ] );
         else if( iEndian == HB_CDP_ENDIAN_BIG )
            wc = HB_GET_BE_UINT16( &pSrc[ ulS ] );
         else
            wc = pSrc[ ulS ];
#endif
         i = hb_cdpUTF8CharSize( wc );
         if( ulD + i <= nDst )
         {
            hb_cdpU16CharToUTF8( &pDst[ ulD ], wc );
            ulD += i;
         }
         else
            break;
      }
   }
   else
   {
      if( cdp->uniTable->uniTrans == NULL )
         hb_cdpBuildTransTable( cdp->uniTable );
      uniTrans = cdp->uniTable->uniTrans;
      wcMax = cdp->uniTable->wcMax;

      for( ulS = ulD = 0; ulS < nSrc && ulD < nDst; ++ulS )
      {
#if defined( HB_CDP_ENDIAN_SWAP )
         wc = pSrc[ ulS ];
         if( iEndian == HB_CDP_ENDIAN_SWAP )
            wc = HB_SWAP_UINT16( wc );
#else
         if( iEndian == HB_CDP_ENDIAN_LITTLE )
            wc = HB_GET_LE_UINT16( &pSrc[ ulS ] );
         else if( iEndian == HB_CDP_ENDIAN_BIG )
            wc = HB_GET_BE_UINT16( &pSrc[ ulS ] );
         else
            wc = pSrc[ ulS ];
#endif
         if( wc <= wcMax && uniTrans[ wc ] )
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
                     if( ulD < nDst )
                        pDst[ ulD++ ] = cdp->multi[ i ].cLast[ 0 ];
                     break;
                  }
                  if( wc == cdp->multi[ i ].wcLo )
                  {
                     pDst[ ulD++ ] = cdp->multi[ i ].cFirst[ 1 ];
                     if( ulD < nDst )
                        pDst[ ulD++ ] = cdp->multi[ i ].cLast[ 1 ];
                     break;
                  }
               }
               if( i < cdp->nMulti )
                  continue;
            }
            pDst[ ulD++ ] = wc >= 0x100 ? '?' : ( HB_UCHAR ) wc;
         }
      }
   }

   if( ulD < nDst )
      pDst[ ulD ] = '\0';

   return ulD;
}


/*
 * CP translations
 */
HB_SIZE hb_cdpTransLen( const char * pSrc, HB_SIZE nSrc, HB_SIZE nMax,
                        PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   HB_SIZE nSize;

   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable )
   {
      if( cdpIn == &s_utf8_codepage )
         return hb_cdpUTF8AsStrLen( cdpOut, pSrc, nSrc, nMax );
      else if( cdpOut == &s_utf8_codepage )
         return hb_cdpStrAsUTF8Len( cdpIn, pSrc, nSrc, nMax );
      else
      {
         if( cdpIn->nMultiUC || cdpOut->nMultiUC )
         {
            HB_SIZE ul;
            int i;

            HB_UCHAR * uniTrans;
            HB_WCHAR wcMax;

            if( cdpOut->uniTable->uniTrans == NULL )
               hb_cdpBuildTransTable( cdpOut->uniTable );
            uniTrans = cdpOut->uniTable->uniTrans;
            wcMax = cdpOut->uniTable->wcMax;

            for( ul = nSize = 0; ul < nSrc && ( nMax == 0 || nSize < nMax ); ++ul, ++nSize )
            {
               HB_UCHAR uc = ( HB_UCHAR ) pSrc[ ul ];
               HB_WCHAR wc = cdpIn->uniTable->uniCodes[ uc ];

               if( cdpIn->nMultiUC &&
                   ( cdpIn->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
                   ul + 1 < nSrc )
               {
                  HB_UCHAR uc2 = ( HB_UCHAR ) pSrc[ ul + 1 ];
                  if( ( cdpIn->flags[ uc2 ] & HB_CDP_MULTI2 ) != 0 )
                  {
                     for( i = 0; i < cdpIn->nMulti; ++i )
                     {
                        if( uc2 == cdpIn->multi[ i ].cLast[ 0 ] ||
                            uc2 == cdpIn->multi[ i ].cLast[ 1 ] )
                        {
                           if( uc == cdpIn->multi[ i ].cFirst[ 0 ] )
                           {
                              if( cdpIn->multi[ i ].wcUp )
                              {
                                 wc = cdpIn->multi[ i ].wcUp;
                                 ++ul;
                              }
                              break;
                           }
                           else if( uc == cdpIn->multi[ i ].cFirst[ 1 ] )
                           {
                              if( cdpIn->multi[ i ].wcLo )
                              {
                                 wc = cdpIn->multi[ i ].wcLo;
                                 ++ul;
                              }
                              break;
                           }
                        }
                     }
                  }
               }

               if( wc && ( wc > wcMax || uniTrans[ wc ] == 0 ) && cdpOut->nMultiUC &&
                   ( nMax == 0 || nSize + 1 < nMax ) )
               {
                  for( i = 0; i < cdpOut->nMulti; ++i )
                  {
                     if( wc == cdpOut->multi[ i ].wcUp ||
                         wc == cdpOut->multi[ i ].wcLo )
                     {
                           ++nSize;
                           break;
                     }
                  }
               }
            }
         }
         else
            nSize = ( nMax && nSrc > nMax ) ? nMax : nSrc;
      }
   }
   else
      nSize = ( nMax && nSrc > nMax ) ? nMax : nSrc;

   return nSize;
}

HB_SIZE hb_cdpTransTo( const char * pSrc, HB_SIZE nSrc,
                       char * pDst, HB_SIZE nDst,
                       PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   HB_SIZE nSize;

   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable )
   {
      if( cdpIn == &s_utf8_codepage )
         return hb_cdpUTF8ToStr( cdpOut, pSrc, nSrc, pDst, nDst );
      else if( cdpOut == &s_utf8_codepage )
         return hb_cdpStrToUTF8( cdpIn, pSrc, nSrc, pDst, nDst );
      else
      {
         HB_UCHAR * uniTrans;
         HB_WCHAR wcMax;

         if( cdpOut->uniTable->uniTrans == NULL )
            hb_cdpBuildTransTable( cdpOut->uniTable );
         uniTrans = cdpOut->uniTable->uniTrans;
         wcMax = cdpOut->uniTable->wcMax;

         if( cdpIn->nMultiUC || cdpOut->nMultiUC )
         {
            HB_SIZE ul;
            int i;

            for( ul = nSize = 0; ul < nSrc && nSize < nDst; ++ul, ++nSize )
            {
               HB_UCHAR uc = ( HB_UCHAR ) pSrc[ ul ];
               HB_WCHAR wc = cdpIn->uniTable->uniCodes[ uc ];

               if( cdpIn->nMultiUC &&
                   ( cdpIn->flags[ uc ] & HB_CDP_MULTI1 ) != 0 &&
                   ul + 1 < nSrc )
               {
                  HB_UCHAR uc2 = ( HB_UCHAR ) pSrc[ ul + 1 ];
                  if( ( cdpIn->flags[ uc2 ] & HB_CDP_MULTI2 ) != 0 )
                  {
                     for( i = 0; i < cdpIn->nMulti; ++i )
                     {
                        if( uc2 == cdpIn->multi[ i ].cLast[ 0 ] ||
                            uc2 == cdpIn->multi[ i ].cLast[ 1 ] )
                        {
                           if( uc == cdpIn->multi[ i ].cFirst[ 0 ] )
                           {
                              if( cdpIn->multi[ i ].wcUp )
                              {
                                 wc = cdpIn->multi[ i ].wcUp;
                                 ++ul;
                              }
                              break;
                           }
                           else if( uc == cdpIn->multi[ i ].cFirst[ 1 ] )
                           {
                              if( cdpIn->multi[ i ].wcLo )
                              {
                                 wc = cdpIn->multi[ i ].wcLo;
                                 ++ul;
                              }
                              break;
                           }
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
                           if( nSize + 1 < nDst )
                           {
                              pDst[ nSize++ ] = cdpOut->multi[ i ].cFirst[ 0 ];
                              uc = cdpOut->multi[ i ].cLast[ 0 ];
                           }
                           else
                              uc = cdpOut->multi[ i ].cFirst[ 0 ];
                           break;
                        }
                        if( wc == cdpOut->multi[ i ].wcLo )
                        {
                           if( nSize + 1 < nDst )
                           {
                              pDst[ nSize++ ] = cdpOut->multi[ i ].cFirst[ 1 ];
                              uc = cdpOut->multi[ i ].cLast[ 1 ];
                           }
                           else
                              uc = cdpOut->multi[ i ].cFirst[ 1 ];
                           break;
                        }
                     }
                  }
               }

               pDst[ nSize ] = uc;
            }
         }
         else
         {
            if( nSrc > nDst )
               nSrc = nDst;
            for( nSize = 0; nSize < nSrc; ++nSize )
            {
               HB_UCHAR uc = ( HB_UCHAR ) pSrc[ nSize ];
               HB_WCHAR wc = cdpIn->uniTable->uniCodes[ uc ];
               if( wc && wc <= wcMax && uniTrans[ wc ] )
                  uc = uniTrans[ wc ];
               pDst[ nSize ] = uc;
            }
         }
      }
   }
   else
   {
      nSize = ( nSrc > nDst ) ? nDst : nSrc;
      memcpy( pDst, pSrc, nSize );
   }

   if( nSize < nDst )
      pDst[ nSize ] = '\0';

   return nSize;
}

int hb_cdpTranslateChar( int iChar, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable &&
       iChar >= 0 && iChar < 256 )
   {
      HB_WCHAR wc = cdpIn->uniTable->uniCodes[ iChar ];

      if( wc )
      {
         if( cdpOut->uniTable->uniTrans == NULL )
            hb_cdpBuildTransTable( cdpOut->uniTable );

         if( wc <= cdpOut->uniTable->wcMax )
         {
            wc = cdpOut->uniTable->uniTrans[ wc ];
            if( wc )
               iChar = wc;
         }
      }
   }

   return iChar;
}

int hb_cdpTranslateDispChar( int iChar, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable &&
       iChar >= 0 && iChar < 256 )
   {
      HB_WCHAR wc = cdpIn->uniTable->uniCodes[ iChar ];

      if( wc == 0 )
         wc = iChar < 32 ? s_uniCtrls[ iChar ] : s_uniCodes[ iChar ];

      if( wc )
      {
         if( cdpOut->uniTable->uniTrans == NULL )
            hb_cdpBuildTransTable( cdpOut->uniTable );

         if( wc <= cdpOut->uniTable->wcMax )
         {
            wc = cdpOut->uniTable->uniTrans[ wc ];
            if( wc )
               iChar = wc;
         }
      }
   }

   return iChar;
}

HB_SIZE hb_cdpnDupLen( const char * pSrc, HB_SIZE nSrc,
                       PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   return hb_cdpTransLen( pSrc, nSrc, 0, cdpIn, cdpOut );
}

HB_SIZE hb_cdpnDup2Len( const char * pSrc, HB_SIZE nSrc, HB_SIZE nMax,
                        PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   return hb_cdpTransLen( pSrc, nSrc, nMax, cdpIn, cdpOut );
}

char * hb_cdpnDup( const char * pSrc, HB_SIZE * pnLen,
                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   char * pDst;
   HB_SIZE nDst;

   nDst = hb_cdpTransLen( pSrc, *pnLen, 0, cdpIn, cdpOut );
   pDst = ( char * ) hb_xgrab( nDst + 1 );
   hb_cdpTransTo( pSrc, *pnLen, pDst, nDst + 1, cdpIn, cdpOut );
   *pnLen = nDst;

   return pDst;
}

const char * hb_cdpnDup2( const char * pSrc, HB_SIZE nSrc,
                          char * pDst, HB_SIZE * pnDst,
                          PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   * pnDst = hb_cdpTransTo( pSrc, nSrc, pDst, *pnDst, cdpIn, cdpOut );
   return pDst;
}

const char * hb_cdpnDup3( const char * pSrc, HB_SIZE nSrc,
                          char * pDst, HB_SIZE * pnDst,
                          char ** pFree, HB_SIZE * pnSize,
                          PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn->uniTable != cdpOut->uniTable && nSrc )
   {
      char * pPrev = NULL;
      HB_SIZE nDst = hb_cdpTransLen( pSrc, nSrc, 0, cdpIn, cdpOut );

      if( pDst == NULL )
      {
         pDst = *pFree;
         if( pDst == NULL && *pnSize > 0 )
            pDst = ( char * ) pSrc;
      }

      if( nDst >= *pnSize || ( pDst == pSrc &&
          ( cdpOut == &s_utf8_codepage || cdpOut->nMultiUC ) ) )
      {
         pPrev = *pFree;
         pDst = *pFree = ( char * ) hb_xgrab( nDst + 1 );
         *pnSize = nDst + 1;
      }

      nDst = hb_cdpTransTo( pSrc, nSrc, pDst, *pnSize, cdpIn, cdpOut );

      if( pPrev )
         hb_xfree( pPrev );
      if( pnDst )
         *pnDst = nDst;
      return pDst;
   }

   if( pnDst )
      *pnDst = nSrc;

   return pSrc;
}

char * hb_cdpDup( const char * pszSrc, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   HB_SIZE nLen = strlen( pszSrc );
   return hb_cdpnDup( pszSrc, &nLen, cdpIn, cdpOut );
}


/*
 * CP management
 */
static PHB_CODEPAGE hb_buildCodePage( const char * id, const char * info,
                                      PHB_UNITABLE uniTable,
                                      const char * pszUpper,
                                      const char * pszLower,
                                      unsigned int nACSort,
                                      unsigned int nCaseSort )
{
   HB_BOOL lSort, fError;
   int iMulti, iAcc, iAccUp, iAccLo, iSortUp, iSortLo, i;
   const char * pup, * plo;
   HB_UCHAR ucUp, ucLo, ucUp2, ucLo2;
   HB_SIZE nSize, ul;
   HB_UCHAR * buffer, * flags, * upper, * lower, * sort, * acc;
   HB_UCHAR used[ 256 ];
   PHB_CODEPAGE cdp;
   PHB_MULTICHAR multi;

   memset( used, '\0', sizeof( used ) );

   iMulti = iAcc = iSortUp = iSortLo = 0;
   fError = lSort = HB_FALSE;

   ucUp2 = ucLo2 = 0;
   pup = pszUpper;
   plo = pszLower;
   for( ;; )
   {
      ucUp = ( HB_UCHAR ) *pup++;
      ucLo = ( HB_UCHAR ) *plo++;
      if( ucUp == 0 || ucLo == 0 )
      {
         if( ucUp || ucLo )
            fError = HB_TRUE;
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
            ucUp = ( HB_UCHAR ) *pup;
            ucLo = ( HB_UCHAR ) *plo;

            if( ( ucUp != ' ' || ucLo != ' ' ) &&
                ( ucUp == ( HB_UCHAR ) *pup || ( ucUp != ' ' && *pup != ' ' ) ) &&
                ( ucLo == ( HB_UCHAR ) *plo || ( ucLo != ' ' && *plo != ' ' ) ) )
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
                  lSort = HB_TRUE;
                  iMulti++;
                  pup++;
                  plo++;
                  continue;
               }
            }
         }
         fError = HB_TRUE;
         break;
      }
      if( ucUp == '~' )
      {
         if( ucLo != '~' || *pup == '\0' || *plo == '\0' )
         {
            fError = HB_TRUE;
            break;
         }
         ucUp = ( HB_UCHAR ) *pup++;
         ucLo = ( HB_UCHAR ) *plo++;
         ++iAcc;
      }
      if( used[ ucUp ] != 0 )
         ucUp = ' ';
      if( used[ ucLo ] != 0 )
         ucLo = ' ';
      if( ucUp == ' ' && ucLo == ' ' )
      {
         fError = HB_TRUE;
         break;
      }
      if( ucUp != ' ' )
      {
         used[ ucUp ] = 1;
         if( ucUp < ucUp2 )
            lSort = HB_TRUE;
         ucUp2 = ucUp;
         ++iSortLo;
      }
      if( ucLo != ' ' )
      {
         used[ ucLo ] = 1;
         if( ucLo < ucLo2 )
            lSort = HB_TRUE;
         ucLo2 = ucLo;
      }
   }

   if( iMulti > 64 )
      fError = HB_TRUE;

   if( fError || nACSort > HB_CDP_ACSORT_INTERLEAVED ||
       nCaseSort > HB_CDP_CSSORT_IGNORE )
   {
#ifdef __HB_IGNORE_CP_ERRORS
      return NULL;
#else
      hb_errInternal( 9994, "Harbour CP (%s) initialization failure", id, NULL );
#endif
   }

   if( iAcc == 0 )
      nACSort = HB_CDP_ACSORT_NONE;
   else if( nACSort != HB_CDP_ACSORT_NONE )
      lSort = HB_TRUE;

   if( nCaseSort != HB_CDP_CSSORT_UPLO )
      lSort = HB_TRUE;

   nSize = 0x300;
   if( lSort )
   {
      nSize += 0x100;
      if( nACSort == HB_CDP_ACSORT_INTERLEAVED )
         nSize += 0x100;
   }
   ul = nSize;
   nSize += sizeof( HB_CODEPAGE );
   if( iMulti )
      nSize += iMulti * sizeof( HB_MULTICHAR );

   buffer = ( HB_UCHAR * ) hb_xgrab( nSize );
   memset( buffer, '\0', nSize );
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
      upper[ i ] = ( HB_UCHAR ) HB_TOUPPER( i );
      lower[ i ] = ( HB_UCHAR ) HB_TOLOWER( i );
   }

   iAccUp = iAccLo = 0;
   multi = cdp->multi;
   pup = pszUpper;
   plo = pszLower;
   ucUp2 = ucLo2 = 255;
   memset( used, '\0', sizeof( used ) );
   while( *pup )
   {
      ucUp = ( HB_UCHAR ) *pup++;
      ucLo = ( HB_UCHAR ) *plo++;
      if( ucUp == '.' )
      {
         multi->cFirst[ 0 ] = *pup++;
         multi->cLast [ 0 ] = *pup++;
         multi->cFirst[ 1 ] = *plo++;
         multi->cLast [ 1 ] = *plo++;
         if( multi->cFirst[ 0 ] != ' ' )
         {
            flags[ ( HB_UCHAR ) multi->cFirst[ 0 ] ] |= HB_CDP_MULTI1;
            flags[ ( HB_UCHAR ) multi->cLast [ 0 ] ] |= HB_CDP_MULTI2;
            multi->sortUp = ++iSortUp - iAccUp;
         }
         if( multi->cFirst[ 1 ] != ' ' )
         {
            flags[ ( HB_UCHAR ) multi->cFirst[ 1 ] ] |= HB_CDP_MULTI1;
            flags[ ( HB_UCHAR ) multi->cLast [ 1 ] ] |= HB_CDP_MULTI2;

            if( nCaseSort == HB_CDP_CSSORT_UPLO )
               ++iSortLo;
            else if( nCaseSort == HB_CDP_CSSORT_MIXED )
               iSortLo = ++iSortUp;
            else
               iSortLo = iSortUp;
            multi->sortLo = iSortLo - iAccLo;
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
            ucUp = ( HB_UCHAR ) *pup++;
            ucLo = ( HB_UCHAR ) *plo++;
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
                  sort[ ucUp ] = ( HB_UCHAR ) ( ++iSortUp - iAccUp );
                  if( acc )
                     acc[ ucUp ] = ( HB_UCHAR ) iSortUp;
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
                  if( nCaseSort == HB_CDP_CSSORT_UPLO )
                  {
                     if( iAcc && nACSort != HB_CDP_ACSORT_NONE )
                        ++iAccLo;
                     ++iSortLo;
                  }
                  else
                  {
                     if( nCaseSort == HB_CDP_CSSORT_MIXED )
                     {
                        if( iAcc && nACSort != HB_CDP_ACSORT_NONE )
                           ++iAccUp;
                        ++iSortUp;
                     }
                     iAccLo = iAccUp;
                     iSortLo = iSortUp;
                  }
                  sort[ ucLo ] = ( HB_UCHAR ) ( iSortLo - iAccLo );
                  if( acc )
                     acc[ ucLo ] = ( HB_UCHAR ) iSortLo;
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
         {
#ifdef __HB_IGNORE_CP_ERRORS
            hb_xfree( buffer );
            return NULL;
#else
            hb_errInternal( 9994, "Harbour CP (%s) initialization failure", id, NULL );
#endif
         }

         if( iMulti <= 32 )
            iMulti = 33;
         else
            iMulti = 65;
      }
      else
         iMulti = 1;

      if( nCaseSort != HB_CDP_CSSORT_UPLO )
         ucLo2 = 0;

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

         sort[ i ] += ( HB_UCHAR ) iAdd;
         if( acc )
            acc[ i ] += ( HB_UCHAR ) iAdd;
      }
      for( i = 0; i < cdp->nMulti; ++i )
      {
         cdp->multi[ i ].sortUp += iUp;
         cdp->multi[ i ].sortLo += iUp + iLo;
      }
   }

   return cdp;
}

static PHB_CODEPAGE * hb_cdpFindPos( const char * id )
{
   PHB_CODEPAGE * cdp_ptr;

   if( s_cdpList == NULL )
   {
      HB_UCHAR * flags, * upper, * lower;
      int i;

      s_en_codepage.buffer = ( HB_UCHAR * ) hb_xgrab( 0x300 );
      memset( s_en_codepage.buffer, '\0', 0x300 );
      s_en_codepage.flags = flags = ( HB_UCHAR * ) s_en_codepage.buffer;
      s_en_codepage.upper = upper = ( HB_UCHAR * ) s_en_codepage.buffer + 0x100;
      s_en_codepage.lower = lower = ( HB_UCHAR * ) s_en_codepage.buffer + 0x200;
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
         upper[ i ] = ( HB_UCHAR ) HB_TOUPPER( i );
         lower[ i ] = ( HB_UCHAR ) HB_TOLOWER( i );
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

HB_BOOL hb_cdpRegisterRaw( PHB_CODEPAGE cdp )
{
   PHB_CODEPAGE * cdp_ptr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpRegisterRaw(%p)", cdp ) );

   cdp_ptr = hb_cdpFindPos( cdp->id );
   if( *cdp_ptr == NULL )
   {
      *cdp_ptr = cdp;
      return HB_TRUE;
   }
   return HB_FALSE;
}

HB_BOOL hb_cdpRegisterNew( const char * id, const char * info,
                           PHB_UNITABLE uniTable,
                           const char * pszUpper, const char * pszLower,
                           unsigned int nACSort,
                           unsigned int nCaseSort )
{
   PHB_CODEPAGE * cdp_ptr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpRegisterNew(%s,%s,%s,%s,%u,%u)", id, info, pszUpper, pszLower, nACSort, nCaseSort ) );

   cdp_ptr = hb_cdpFindPos( id );
   if( *cdp_ptr == NULL )
   {
      *cdp_ptr = hb_buildCodePage( id, info, uniTable, pszUpper, pszLower,
                                   nACSort, nCaseSort );
      return *cdp_ptr != NULL;
   }
   return HB_FALSE;
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
   PHB_CODEPAGE cdp;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpSelectID(%s)", id ) );

   cdp = hb_cdpSelect( hb_cdpFind( id ) );

   return cdp ? cdp->id : NULL;
}

/* Caller must release the pointer */
const char ** hb_cdpList( void )
{
   PHB_CODEPAGE cdp;
   int iCount, iPos;
   const char ** list;

   cdp = s_cdpList;
   iCount = 0;
   while( cdp )
   {
      ++iCount;
      cdp = cdp->next;
   }

   list = ( const char ** ) hb_xgrab( ( iCount + 1 ) * sizeof( char * ) );

   cdp = s_cdpList;
   iPos = 0;
   while( cdp && iPos < iCount )
   {
      list[ iPos++ ] = cdp->id;
      cdp = cdp->next;
   }
   list[ iPos ] = NULL;

   return list;
}
