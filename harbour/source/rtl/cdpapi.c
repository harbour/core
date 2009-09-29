/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The CodePages API
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

#include "hbapi.h"

#ifndef HB_CDP_SUPPORT_OFF

#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapierr.h"

#define HB_CDP_MAX_  128

/* Now we are using only 16bit Unicode values so the maximum size
 * of single character encoded in UTF8 is 3 though ISO 10646 Universal
 * Character Set (UCS) occupies even a 31-bit code space and to encode
 * all UCS values we will need 6 bytes. Now in practice no one uses
 * Unicode character over 0xFFFF but it may change in the future so
 * it's safer to use macro for maximum UTF8 character size. [druzus]
 */
#define HB_MAX_UTF8        3

#define NUMBER_OF_CHARS    256

static USHORT s_uniCodes[ NUMBER_OF_CHARS ] =
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

HB_UNITABLE hb_uniTbl_437 = { HB_CPID_437, NUMBER_OF_CHARS, FALSE, s_uniCodes };

static HB_CODEPAGE s_en_codepage =
   { "EN", HB_CPID_437, HB_UNITB_437, 0, NULL, NULL, 0, 0, 0, 0, 0, NULL, NULL, NULL, NULL, 0, NULL };

HB_CODEPAGE_ANNOUNCE( EN )

static PHB_CODEPAGE s_cdpList[HB_CDP_MAX_] = { &s_en_codepage };

/* pseudo codepage for translations only */
static HB_CODEPAGE s_utf8_codepage =
   { "UTF8", HB_CPID_437, HB_UNITB_437, 0, NULL, NULL, 0, 0, 0, 0, 0, NULL, NULL, NULL, NULL, 0, NULL };

static int utf8Size( USHORT uc )
{
   if( uc < 0x0080 )
      return 1;
   else if( uc < 0x0800 )
      return 2;
   else                         /* if( uc <= 0xffff ) */
      return 3;
}

static int u16toutf8( char * szUTF8, USHORT uc )
{
   int n;

   if( uc < 0x0080 )
   {
      szUTF8[0] = uc & 0xff;
      n = 1;
   }
   else if( uc < 0x0800 )
   {
      szUTF8[0] = 0xc0 | ( ( uc >> 6 ) & 0x1f );
      szUTF8[1] = 0x80 | ( uc & 0x3f );
      n = 2;
   }
   else                         /* if( uc <= 0xffff ) */
   {
      szUTF8[0] = 0xe0 | ( ( uc >> 12 ) & 0x0f );
      szUTF8[1] = 0x80 | ( ( uc >> 6 ) & 0x3f );
      szUTF8[2] = 0x80 | ( uc & 0x3f );
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

static BOOL utf8tou16nextchar( UCHAR ucChar, int *n, USHORT * uc )
{
   if( *n > 0 )
   {
      if( ( ucChar & 0xc0 ) != 0x80 )
         return FALSE;
      *uc = ( *uc << 6 ) | ( ucChar & 0x3f );
      ( *n )--;
      return TRUE;
   }

   *n = 0;
   *uc = ucChar;
   if( ucChar >= 0xc0 )
   {
      if( ucChar < 0xe0 )
      {
         *uc &= 0x1f;
         *n = 1;
      }
      else if( ucChar < 0xf0 )
      {
         *uc &= 0x0f;
         *n = 2;
      }
      else if( ucChar < 0xf8 )
      {
         *uc &= 0x07;
         *n = 3;
      }
      else if( ucChar < 0xfc )
      {
         *uc &= 0x03;
         *n = 4;
      }
      else if( ucChar < 0xfe )
      {
         *uc &= 0x01;
         *n = 5;
      }
   }
   return TRUE;
}

#   if 0                        /* currently unused, it will in the future */
static int utf8tou16( const char * szUTF8, USHORT * uc )
{
   int n = 1, m = 1;
   UINT32 u32;

   u32 = ( UCHAR ) *szUTF8;
   if( u32 >= 0xc0 )
   {
      if( u32 < 0xe0 )
      {
         u32 &= 0x1f;
         m = 2;
      }
      else if( u32 < 0xf0 )
      {
         u32 &= 0x0f;
         m = 3;
      }
      else if( u32 < 0xf8 )
      {
         u32 &= 0x07;
         m = 4;
      }
      else if( u32 < 0xfc )
      {
         u32 &= 0x03;
         m = 5;
      }
      else if( u32 < 0xfe )
      {
         u32 &= 0x01;
         m = 6;
      }
      while( n < m && ( ( UCHAR ) szUTF8[ n ] & 0xc0 ) == 0x80 )
      {
         u32 = ( u32 << 6 ) | ( ( UCHAR ) szUTF8[n++] & 0x3f );
      }
      if( n < m )
      {
         u32 <<= 6 * ( m - n );
      }
   }

   *uc = ( USHORT ) u32;
   return n;
}
#   endif

static ULONG utf8pos( const char * szUTF8, ULONG ulLen, ULONG ulUTF8Pos )
{
   if( ulUTF8Pos )
   {
      ULONG ul, ul2;
      USHORT uc;
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

static int hb_cdpFindPos( const char *pszID )
{
   int iPos;

   if( pszID != NULL )
   {
      for( iPos = 0; iPos < HB_CDP_MAX_ && s_cdpList[iPos]; iPos++ )
      {
         if( strcmp( s_cdpList[iPos]->id, pszID ) == 0 )
            return iPos;
      }
   }

   return -1;
}

BOOL hb_cdpRegister( PHB_CODEPAGE cdpage )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpRegister(%p)", cdpage ) );

   if( cdpage )
   {
      int iPos = hb_cdpFindPos( cdpage->id );

      if( iPos == -1 )
      {
         for( iPos = 0; iPos < HB_CDP_MAX_; iPos++ )
         {
            if( !s_cdpList[iPos] )
            {
               int i, ia, iu, il, iumax = 0, ilmax = 0;
               char *ptrUpper = ( char * ) cdpage->CharsUpper;
               char *ptrLower = ( char * ) cdpage->CharsLower;
               char *ptr;
               HB_MULTICHAR multi[12];
               int nMulti = 0;

               s_cdpList[iPos] = cdpage;

               cdpage->lSort = cdpage->lAccInterleave || cdpage->lAccEqual;
               cdpage->lChClone = FALSE;
               if( cdpage->nChars )
               {
                  int nAddLower = cdpage->nChars + ( ( cdpage->lLatin ) ? 6 : 0 );

                  cdpage->s_chars = ( BYTE * ) hb_xgrab( 256 );
                  memset( cdpage->s_chars, '\0', 256 );
                  cdpage->s_upper = ( BYTE * ) hb_xgrab( 256 );
                  cdpage->s_lower = ( BYTE * ) hb_xgrab( 256 );
                  if( cdpage->lAccInterleave )
                  {
                     cdpage->s_accent = ( BYTE * ) hb_xgrab( 256 );
                     memset( cdpage->s_accent, '\0', 256 );
                  }
                  else
                     cdpage->s_accent = NULL;

                  for( i = 0; i < 256; i++ )
                  {
                     cdpage->s_upper[ i ] = ( char ) HB_TOUPPER( ( UCHAR ) i );
                     cdpage->s_lower[ i ] = ( char ) HB_TOLOWER( ( UCHAR ) i );
                  }
                  if( strpbrk( cdpage->CharsUpper, "~." ) != NULL )
                  {
                     cdpage->CharsUpper = ptrUpper = hb_strdup( cdpage->CharsUpper );
                     cdpage->CharsLower = ptrLower = hb_strdup( cdpage->CharsLower );
                     cdpage->lChClone = TRUE;
                  }
                  for( i = ia = 1; *ptrUpper; i++, ia++, ptrUpper++, ptrLower++ )
                  {
                     if( ( cdpage->lAccEqual || cdpage->lAccInterleave ) &&
                         *ptrUpper == '~' && *ptrLower == '~' )
                     {
                        for( ptr = ptrUpper + 1; *ptr; ptr++ )
                           *( ptr - 1 ) = *ptr;
                        *( ptr - 1 ) = '\0';
                        for( ptr = ptrLower + 1; *ptr; ptr++ )
                           *( ptr - 1 ) = *ptr;
                        *( ptr - 1 ) = '\0';
                        if( cdpage->lAccEqual )
                           i--;
                     }
                     else if( *ptrUpper == '.' && *ptrLower == '.' &&
                              ptrUpper[1] && ptrUpper[2] && ptrUpper[3] == '.' &&
                              ptrLower[1] && ptrLower[2] && ptrLower[3] == '.' )
                     {
                        multi[nMulti].cFirst[0] = *( ptrUpper + 1 );
                        multi[nMulti].cFirst[1] = *( ptrLower + 1 );
                        multi[nMulti].cLast[0] = *( ptrUpper + 2 );
                        multi[nMulti].cLast[1] = *( ptrLower + 2 );
                        multi[nMulti].nCode = i;

                        for( ptr = ptrUpper + 4; *ptr; ptr++ )
                           *( ptr - 4 ) = *ptr;
                        *( ptr - 4 ) = '\0';
                        for( ptr = ptrLower + 4; *ptr; ptr++ )
                           *( ptr - 4 ) = *ptr;
                        *( ptr - 4 ) = '\0';

                        nMulti++;
                        ptrUpper--;
                        ptrLower--;
                        cdpage->lSort = TRUE;
                        continue;
                     }
                     iu = ( UCHAR ) * ptrUpper;
                     il = ( UCHAR ) * ptrLower;
                     if( iu < iumax || il < ilmax )
                        cdpage->lSort = TRUE;
                     iumax = iu;
                     ilmax = il;

                     cdpage->s_chars[iu] = ( char ) i;
                     cdpage->s_chars[il] = ( char ) ( i + nAddLower );
                     if( cdpage->lAccInterleave )
                     {
                        cdpage->s_accent[iu] = ( char ) ia;
                        cdpage->s_accent[il] = ( char ) ( ia + nAddLower );
                     }
                     cdpage->s_upper[il] = *ptrUpper;
                     cdpage->s_lower[iu] = *ptrLower;
                  }
                  if( cdpage->lLatin )
                  {
                     for( i = 91; i <= 96; i++ )
                     {
                        if( !cdpage->s_chars[ i ] )
                           cdpage->s_chars[ i ] = ( char ) ( cdpage->nChars + ( i - 90 ) );
                     }
                     for( i = 123; i < 256; i++ )
                     {
                        if( !cdpage->s_chars[ i ] )
                           cdpage->s_chars[ i ] = ( char ) ( cdpage->nChars + nAddLower + ( i - 122 ) );
                     }
                  }
                  /*
                     for( i=0; i<32; i++ )
                     printf( "\n %3d %3d %3d %3d %3d %3d %3d %3d",cdpage->s_chars[i*8],
                     cdpage->s_chars[i*8+1],cdpage->s_chars[i*8+2],
                     cdpage->s_chars[i*8+3],cdpage->s_chars[i*8+4],
                     cdpage->s_chars[i*8+5],cdpage->s_chars[i*8+6],
                     cdpage->s_chars[i*8+7] );
                     if( nMulti )
                     {
                     printf( "\n" );
                     for( i=0; i<nMulti; i++ )
                     printf( "\n %c%c %c%c %d",multi[ i ].cFirst[0],
                     multi[ i ].cFirst[1],multi[ i ].cLast[0],multi[ i ].cLast[1],multi[ i ].nCode );
                     }
                   */
                  if( nMulti )
                  {
                     cdpage->multi = ( PHB_MULTICHAR ) hb_xgrab( sizeof( HB_MULTICHAR ) * nMulti );
                     memcpy( cdpage->multi, multi, sizeof( HB_MULTICHAR ) * nMulti );
                     cdpage->nMulti = nMulti;
                  }
                  else
                     cdpage->multi = NULL;
               }
               return TRUE;
            }
         }
      }
   }

   return FALSE;
}

PHB_CODEPAGE hb_cdpFind( const char *pszID )
{
   int iPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpFind(%s)", pszID ) );

   iPos = hb_cdpFindPos( pszID );

   return ( iPos != -1 ) ? s_cdpList[iPos] : NULL;
}

PHB_CODEPAGE hb_cdpFindExt( const char *pszID )
{
   int iPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpFindExt(%s)", pszID ) );

   if( pszID && strcmp( pszID, "UTF8" ) == 0 )
      return &s_utf8_codepage;

   iPos = hb_cdpFindPos( pszID );

   return ( iPos != -1 ) ? s_cdpList[iPos] : NULL;
}

PHB_CODEPAGE hb_cdpSelect( PHB_CODEPAGE cdpage )
{
   PHB_CODEPAGE cdpOld;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpSelect(%p)", cdpage ) );

   cdpOld = hb_vmCDP();
   if( cdpage )
      hb_vmSetCDP( cdpage );

   return cdpOld;
}

const char * hb_cdpID( void )
{
   PHB_CODEPAGE cdp;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpID()" ) );

   cdp = hb_vmCDP();

   return cdp ? cdp->id : NULL;
}

const char * hb_cdpSelectID( const char *pszID )
{
   const char * pszIDOld;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cdpSelectID(%s)", pszID ) );

   pszIDOld = hb_cdpID();
   hb_cdpSelect( hb_cdpFind( pszID ) );

   return pszIDOld;
}

void hb_cdpTranslate( char *psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn != cdpOut && cdpIn->nChars == cdpOut->nChars )
   {
      if( cdpIn->lAccEqual )
      {
         for( ; *psz; psz++ )
         {
            char * ptr;

            /* ( char * ) casting for MSVC */
            ptr = strchr( ( char * ) cdpIn->CharsUpper, *psz );
            if( ptr )
               *psz = cdpOut->CharsUpper[ ptr - cdpIn->CharsUpper ];
            else
            {
               /* ( char * ) casting for MSVC */
               ptr = strchr( ( char * ) cdpIn->CharsLower, *psz );
               if( ptr )
                  *psz = cdpOut->CharsLower[ ptr - cdpIn->CharsLower ];
            }
         }
      }
      else
      {
         int nAddLower = ( cdpIn->lLatin ) ? 6 : 0;

         for( ; *psz; psz++ )
         {
            int n = ( int ) cdpIn->s_chars[( UCHAR ) * psz];

            if( n != 0 &&
                ( n <= cdpOut->nChars || ( n > ( cdpOut->nChars + nAddLower ) &&
                                           n <= ( cdpOut->nChars * 2 + nAddLower ) ) ) )
            {
               n--;
               *psz = ( n >= ( cdpOut->nChars + nAddLower ) ) ?
                  cdpOut->CharsLower[ n - cdpOut->nChars - nAddLower ] : cdpOut->CharsUpper[ n ];
            }
         }
      }
   }
}

void hb_cdpnTranslate( char *psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, ULONG nChars )
{
   if( cdpIn != cdpOut && cdpIn->nChars == cdpOut->nChars )
   {
      if( cdpIn->lAccEqual )
      {
         for( ; nChars--; psz++ )
         {
            char * ptr;

            /* ( char * ) casting for MSVC */
            ptr = strchr( ( char * ) cdpIn->CharsUpper, *psz );
            if( ptr )
               *psz = cdpOut->CharsUpper[ ptr - cdpIn->CharsUpper ];
            else
            {
               /* ( char * ) casting for MSVC */
               ptr = strchr( ( char * ) cdpIn->CharsLower, *psz );
               if( ptr )
                  *psz = cdpOut->CharsLower[ ptr - cdpIn->CharsLower ];
            }
         }
      }
      else
      {
         int nAddLower = ( cdpIn->lLatin ) ? 6 : 0;

         for( ; nChars--; psz++ )
         {
            int n = ( int ) cdpIn->s_chars[( UCHAR ) * psz];

            if( n != 0 &&
                ( n <= cdpOut->nChars || ( n > ( cdpOut->nChars + nAddLower ) &&
                                           n <= ( cdpOut->nChars * 2 + nAddLower ) ) ) )
            {
               n--;
               *psz = ( n >= ( cdpOut->nChars + nAddLower ) ) ?
                  cdpOut->CharsLower[n - cdpOut->nChars - nAddLower] : cdpOut->CharsUpper[ n ];
            }
         }
      }
   }
}

USHORT hb_cdpGetU16( PHB_CODEPAGE cdp, BOOL fCtrl, UCHAR ch )
{
   if( ( fCtrl || ch >= 32 ) && cdp && cdp->uniTable &&
       cdp->uniTable->uniCodes && ch < cdp->uniTable->nChars )
      return cdp->uniTable->uniCodes[ ch ];
   else
      return ch;
}

UCHAR hb_cdpGetChar( PHB_CODEPAGE cdp, BOOL fCtrl, USHORT uc )
{
   if( ( fCtrl || uc >= 32 ) && cdp && cdp->uniTable && cdp->uniTable->uniCodes )
   {
      int i;

      for( i = fCtrl ? 0 : 32; i < cdp->uniTable->nChars; i++ )
      {
         if( cdp->uniTable->uniCodes[ i ] == uc )
         {
            uc = ( USHORT ) i;
            break;
         }
      }
   }
   return uc >= 0x100 ? '?' : ( UCHAR ) uc;
}

/* caller must free the returned buffer if not NULL */
char *hb_cdpUTF8StringSubstr( const char * pSrc, ULONG ulLen,
                              ULONG ulFrom, ULONG ulCount, ULONG * pulDest )
{
   ULONG ul, ulCnt, ulDst = 0;
   USHORT uc;
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

ULONG hb_cdpUTF8StringPeek( const char * pSrc, ULONG ulLen, ULONG ulPos )
{
   if( ulLen )
   {
      ULONG ul;
      USHORT uc = 0;
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

ULONG hb_cdpUTF8StringLength( const char * pSrc, ULONG ulLen )
{
   ULONG ul, ulDst;
   USHORT uc;
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

ULONG hb_cdpStringInUTF8Length( PHB_CODEPAGE cdp, BOOL fCtrl,
                                const char * pSrc, ULONG ulLen )
{
   ULONG ul, ulDst;

   for( ul = ulDst = 0; ul < ulLen; ++ul )
   {
      ulDst += utf8Size( hb_cdpGetU16( cdp, fCtrl, ( UCHAR ) pSrc[ ul ] ) );
   }

   return ulDst;
}

ULONG hb_cdpStringInUTF8Length2( PHB_CODEPAGE cdp, BOOL fCtrl,
                                 const char * pSrc, ULONG ulLen, ULONG ulMax )
{
   ULONG ul, ulDst;
   int n;

   for( ul = ulDst = 0; ul < ulLen; ++ul )
   {
      n = utf8Size( hb_cdpGetU16( cdp, fCtrl, ( UCHAR ) pSrc[ ul ] ) );
      if( ulDst + n > ulMax )
         break;
      ulDst += n;
   }

   return ulDst;
}

ULONG hb_cdpUTF8ToStrn( PHB_CODEPAGE cdp, BOOL fCtrl,
                        const char * pSrc, ULONG ulSrc,
                        char * pDst, ULONG ulDst )
{
   ULONG ulS, ulD;
   USHORT uc = 0;
   int n = 0;

   for( ulS = ulD = 0; ulS < ulSrc; ++ulS )
   {
      if( utf8tou16nextchar( ( UCHAR ) pSrc[ulS], &n, &uc ) )
      {
         if( n == 0 )
         {
            if( ulD < ulDst )
            {
               if( ( fCtrl || uc >= 32 ) && cdp->uniTable && cdp->uniTable->uniCodes )
               {
                  int i;

                  for( i = fCtrl ? 0 : 32; i < cdp->uniTable->nChars; i++ )
                  {
                     if( cdp->uniTable->uniCodes[ i ] == uc )
                     {
                        uc = ( USHORT ) i;
                        break;
                     }
                  }
               }
               pDst[ulD] = uc >= 0x100 ? '?' : ( UCHAR ) uc;
            }
            ++ulD;
         }
      }
   }

   if( ulD < ulDst )
      pDst[ulD] = '\0';

   return ulD;
}

BOOL hb_cdpGetFromUTF8( PHB_CODEPAGE cdp, BOOL fCtrl, UCHAR ch,
                        int *n, USHORT * uc )
{
   if( utf8tou16nextchar( ch, n, uc ) )
   {
      if( *n == 0 && cdp && ( fCtrl || *uc >= 32 ) && cdp->uniTable && cdp->uniTable->uniCodes )
      {
         int i;

         for( i = fCtrl ? 0 : 32; i < cdp->uniTable->nChars; i++ )
         {
            if( cdp->uniTable->uniCodes[ i ] == *uc )
            {
               *uc = ( USHORT ) i;
               break;
            }
         }
      }
      return TRUE;
   }
   return FALSE;
}

ULONG hb_cdpStrnToUTF8( PHB_CODEPAGE cdp, BOOL fCtrl,
                        const char * pSrc, ULONG ulLen, char * pDst )
{
   USHORT u, *uniCodes, nChars;
   ULONG i, n;

   if( cdp && cdp->uniTable )
   {
      if( cdp->nMulti || cdp->uniTable->lMulti )
      {
         /*
          * TODO: this translation is bad, please fix me!!!
          */
         for( i = 0, n = 0; i < ulLen; i++ )
         {
            u = hb_cdpGetU16( cdp, fCtrl, ( UCHAR ) pSrc[ i ] );
            n += u16toutf8( &pDst[ n ], u );
         }
         pDst[ n ] = '\0';
         return n;
      }
      else
      {
         uniCodes = cdp->uniTable->uniCodes;
         nChars = ( USHORT ) cdp->uniTable->nChars;
      }
   }
   else
   {
      nChars = 0;
      uniCodes = NULL;
   }

   for( i = 0, n = 0; i < ulLen; i++ )
   {
      u = ( UCHAR ) pSrc[ i ];
      if( uniCodes && u < nChars && ( fCtrl || u >= 32 ) )
         u = uniCodes[ u ];
      n += u16toutf8( &pDst[ n ], u );
   }
   pDst[ n ] = '\0';

   return n;
}

ULONG hb_cdpStrnToUTF8n( PHB_CODEPAGE cdp, BOOL fCtrl,
                         const char * pSrc, ULONG ulLen,
                         char * pDst, ULONG ulDst )
{
   USHORT u, *uniCodes, nChars;
   ULONG i, n, l;

   if( cdp && cdp->uniTable )
   {
      if( cdp->nMulti || cdp->uniTable->lMulti )
      {
         /*
          * TODO: this translation is bad, please fix me!!!
          */
         for( i = 0, n = 0; i < ulLen; i++ )
         {
            u = hb_cdpGetU16( cdp, fCtrl, ( UCHAR ) pSrc[ i ] );
            l = utf8Size( u );
            if( n + l <= ulDst )
            {
               u16toutf8( &pDst[ n ], u );
               n += l;
            }
            else
               break;
         }
         if( n < ulDst )
            pDst[ n ] = '\0';
         return n;
      }
      else
      {
         uniCodes = cdp->uniTable->uniCodes;
         nChars = ( USHORT ) cdp->uniTable->nChars;
      }
   }
   else
   {
      nChars = 0;
      uniCodes = NULL;
   }

   for( i = 0, n = 0; i < ulLen; i++ )
   {
      u = ( UCHAR ) pSrc[ i ];
      if( uniCodes && u < nChars && ( fCtrl || u >= 32 ) )
         u = uniCodes[ u ];
      l = utf8Size( u );
      if( n + l <= ulDst )
      {
         u16toutf8( &pDst[ n ], u );
         n += l;
      }
      else
         break;
   }
   if( n < ulDst )
      pDst[ n ] = '\0';

   return n;
}

ULONG hb_cdpStrnToU16( PHB_CODEPAGE cdp, BOOL fCtrl,
                       const char * pSrc, ULONG ulLen, char * pDst )
{
   USHORT u, *uniCodes, nChars;
   ULONG i;

   if( cdp && cdp->uniTable )
   {
      if( cdp->nMulti || cdp->uniTable->lMulti )
      {
         /*
          * TODO: this translation is bad, please fix me!!!
          */
         for( i = 0; i < ulLen; i++, pDst += 2 )
         {
            u = hb_cdpGetU16( cdp, fCtrl, ( UCHAR ) pSrc[ i ] );
            HB_PUT_BE_UINT16( pDst, u );
         }
         return i << 1;
      }
      else
      {
         uniCodes = cdp->uniTable->uniCodes;
         nChars = ( USHORT ) cdp->uniTable->nChars;
      }
   }
   else
   {
      nChars = 0;
      uniCodes = NULL;
   }

   for( i = 0; i < ulLen; i++, pDst += 2 )
   {
      u = ( UCHAR ) pSrc[ i ];
      if( uniCodes && u < nChars && ( fCtrl || u >= 32 ) )
         u = uniCodes[ u ];
      HB_PUT_BE_UINT16( pDst, u );
   }
   return i << 1;
}

ULONG hb_cdpnDupLen( const char * pszSrc, ULONG ulLen,
                     PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn != cdpOut && ulLen )
   {
      if( cdpIn == &s_utf8_codepage )
         ulLen = hb_cdpUTF8StringLength( pszSrc, ulLen );
      else if( cdpOut == &s_utf8_codepage )
         ulLen = hb_cdpStringInUTF8Length( cdpIn, FALSE, pszSrc, ulLen );
      else
      {
         /* TODO: add unicode base translation which can respect
          * multibyte characters
          */
      }
   }

   return ulLen;
}

ULONG hb_cdpnDup2Len( const char * pszSrc, ULONG ulLen, ULONG ulMax,
                      PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn != cdpOut && ulLen )
   {
      if( cdpIn == &s_utf8_codepage )
         ulLen = hb_cdpUTF8StringLength( pszSrc, ulLen );
      else if( cdpOut == &s_utf8_codepage )
         ulLen = hb_cdpStringInUTF8Length2( cdpIn, FALSE, pszSrc, ulLen, ulMax );
      else
      {
         /* TODO: add unicode base translation which can respect
          * multibyte characters
          */
      }
   }
   if( ulLen > ulMax )
      ulLen = ulMax;

   return ulLen;
}

char * hb_cdpDup( const char * pszSrc, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   ULONG ulLen = strlen( pszSrc );
   return hb_cdpnDup( pszSrc, &ulLen, cdpIn, cdpOut );
}

char * hb_cdpnDup( const char * pszSrc, ULONG * pulLen,
                   PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   char * pszDst;
   ULONG ulDst;

   if( cdpIn && cdpOut && cdpIn != cdpOut )
   {
      if( cdpIn == &s_utf8_codepage )
      {
         ulDst = hb_cdpUTF8StringLength( pszSrc, *pulLen );
         pszDst = ( char * ) hb_xgrab( ulDst + 1 );
         hb_cdpUTF8ToStrn( cdpOut, FALSE, pszSrc, *pulLen, pszDst, ulDst + 1 );
      }
      else if( cdpOut == &s_utf8_codepage )
      {
         ulDst = hb_cdpStringInUTF8Length( cdpIn, FALSE, pszSrc, *pulLen );
         pszDst = ( char * ) hb_xgrab( ulDst + 1 );
         hb_cdpStrnToUTF8( cdpIn, FALSE, pszSrc, *pulLen, pszDst );
      }
      else
      {
         /* TODO: add unicode base translation which can respect
          * multibyte characters
          */
         ulDst = *pulLen;
         pszDst = ( char * ) hb_xgrab( ulDst + 1 );
         memcpy( pszDst, pszSrc, ulDst );
         pszDst[ ulDst ] = '\0';
         hb_cdpnTranslate( pszDst, cdpIn, cdpOut, ulDst );
      }
   }
   else
   {
      ulDst = *pulLen;
      pszDst = ( char * ) hb_xgrab( ulDst + 1 );
      memcpy( pszDst, pszSrc, ulDst );
      pszDst[ ulDst ] = '\0';
   }

   *pulLen = ulDst;

   return pszDst;
}

const char * hb_cdpnDup2( const char * pszSrc, ULONG ulSrc,
                          char * pszDst, ULONG * pulDst,
                          PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn != cdpOut )
   {
      if( cdpIn == &s_utf8_codepage )
         *pulDst = hb_cdpUTF8ToStrn( cdpOut, FALSE, pszSrc, ulSrc, pszDst, *pulDst );
      else if( cdpOut == &s_utf8_codepage )
         *pulDst = hb_cdpStrnToUTF8n( cdpIn, FALSE, pszSrc, ulSrc, pszDst, *pulDst );
      else
      {
         /* TODO: add unicode base translation which can respect
          * multibyte characters
          */
         if( *pulDst > ulSrc )
         {
            *pulDst = ulSrc;
            pszDst[ ulSrc ] = '\0';
         }
         else
            ulSrc = *pulDst;
         memcpy( pszDst, pszSrc, ulSrc );
         hb_cdpnTranslate( pszDst, cdpIn, cdpOut, ulSrc );
      }
   }
   else
   {
      if( *pulDst > ulSrc )
      {
         *pulDst = ulSrc;
         pszDst[ ulSrc ] = '\0';
      }
      else
         ulSrc = *pulDst;
      memcpy( pszDst, pszSrc, ulSrc );
   }

   return pszDst;
}

const char * hb_cdpnDup3( const char * pszSrc, ULONG ulSrc,
                          char * pszDst, ULONG * pulDst,
                          char ** pszFree, ULONG * pulSize,
                          PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   if( cdpIn && cdpOut && cdpIn != cdpOut && ulSrc )
   {
      char * pszPrev = NULL;
      ULONG ulDst = cdpOut != &s_utf8_codepage ? ulSrc :
                     hb_cdpStringInUTF8Length( cdpIn, FALSE, pszSrc, ulSrc );

      if( pszDst == NULL )
      {
         pszDst = *pszFree;
         if( pszDst == NULL && *pulSize > 0 )
            pszDst = ( char * ) pszSrc;
      }

      if( ulDst >= *pulSize ||
          ( pszDst == pszSrc && cdpOut == &s_utf8_codepage ) )
      {
         pszPrev = *pszFree;
         pszDst = *pszFree = ( char * ) hb_xgrab( ulDst + 1 );
         *pulSize = ulDst + 1;
      }

      if( cdpIn == &s_utf8_codepage )
         ulDst = hb_cdpUTF8ToStrn( cdpOut, FALSE, pszSrc, ulSrc, pszDst, *pulSize );
      else if( cdpOut == &s_utf8_codepage )
         ulDst = hb_cdpStrnToUTF8n( cdpIn, FALSE, pszSrc, ulSrc, pszDst, *pulSize );
      else
      {
         /* TODO: add unicode base translation which can respect
          * multibyte characters
          */
         if( pszDst != pszSrc )
            memcpy( pszDst, pszSrc, ulDst );
         pszDst[ ulDst ] = '\0';
         hb_cdpnTranslate( pszDst, cdpIn, cdpOut, ulSrc );
      }
      if( pszPrev )
         hb_xfree( pszPrev );
      if( *pulDst )
         *pulDst = ulDst;
      return pszDst;
   }

   if( *pulDst )
      *pulDst = ulSrc;
   return pszSrc;
}

int hb_cdpchrcmp( char cFirst, char cSecond, PHB_CODEPAGE cdpage )
{
   int n1, n2;

   if( cFirst == cSecond )
      return 0;

   if( ( n1 = ( int ) cdpage->s_chars[ ( UCHAR ) cFirst ] ) != 0 &&
       ( n2 = ( int ) cdpage->s_chars[ ( UCHAR ) cSecond ] ) != 0 )
      return ( n1 < n2 ) ? -1 : 1;

   return ( ( UCHAR ) cFirst < ( UCHAR ) cSecond ) ? -1 : 1;
}

static int hb_cdpMultiWeight( PHB_CODEPAGE cdpage, const char *szChar )
{
   PHB_MULTICHAR pmulti = cdpage->multi;
   int j;

   for( j = 0; j < cdpage->nMulti; ++j, ++pmulti )
   {
      if( ( *szChar == pmulti->cFirst[0] ||
            *szChar == pmulti->cFirst[1] ) &&
          ( *( szChar + 1 ) == pmulti->cLast[0] || *( szChar + 1 ) == pmulti->cLast[1] ) )
      {
         return pmulti->nCode +
            ( ( *szChar == pmulti->cFirst[0] ) ? 0 :
              ( cdpage->nChars + ( cdpage->lLatin ? 6 : 0 ) ) );
      }
   }

   return 0;
}

int hb_cdpcmp( const char *szFirst, ULONG ulLenFirst,
               const char *szSecond, ULONG ulLenSecond,
               PHB_CODEPAGE cdpage, BOOL fExact )
{
   int iRet = 0, iAcc = 0, n1 = 0, n2 = 0;
   ULONG ul, ulLen;

   ulLen = ulLenFirst < ulLenSecond ? ulLenFirst : ulLenSecond;
   for( ul = 0; ul < ulLen; ++szFirst, ++szSecond, ++ul )
   {
      if( *szFirst != *szSecond )
      {
         if( cdpage->nMulti )
         {
            int nd1, nd2;

            if( ul > 0 )
            {
               nd1 = hb_cdpMultiWeight( cdpage, szFirst - 1 );
               nd2 = hb_cdpMultiWeight( cdpage, szSecond - 1 );
               if( nd1 )
               {
                  if( nd2 )
                  {
                     if( nd1 == nd2 )
                     {
                        nd1 = ( UCHAR ) cdpage->s_chars[ ( UCHAR ) * szFirst ];
                        nd2 = ( UCHAR ) cdpage->s_chars[ ( UCHAR ) * szSecond ];
                        if( nd1 == nd2 || !nd1 || !nd2 )
                        {
                           nd1 = ( UCHAR ) * szFirst;
                           nd2 = ( UCHAR ) * szSecond;
                        }
                     }
                  }
                  else
                     nd2 = n2;
                  iRet = ( nd1 < nd2 ) ? -1 : 1;
                  break;
               }
               else if( nd2 )
               {
                  iRet = ( n1 < nd2 ) ? -1 : 1;
                  break;
               }
            }
            nd1 = ( ul < ulLenFirst - 1 ) ? hb_cdpMultiWeight( cdpage, szFirst ) : 0;
            nd2 = ( ul < ulLenSecond - 1 ) ? hb_cdpMultiWeight( cdpage, szSecond ) : 0;
            if( nd1 )
            {
               if( nd2 )
               {
                  if( nd1 == nd2 )
                  {
                     nd1 = ( UCHAR ) cdpage->s_chars[ ( UCHAR ) * szFirst ];
                     nd2 = ( UCHAR ) cdpage->s_chars[ ( UCHAR ) * szSecond ];
                     if( nd1 == nd2 || !nd1 || !nd2 )
                     {
                        nd1 = ( UCHAR ) * szFirst;
                        nd2 = ( UCHAR ) * szSecond;
                     }
                  }
               }
               else
                  nd2 = ( UCHAR ) cdpage->s_chars[( UCHAR ) * szSecond];
               iRet = ( nd1 < nd2 ) ? -1 : 1;
               break;
            }
            else if( nd2 )
            {
               nd1 = ( UCHAR ) cdpage->s_chars[( UCHAR ) * szFirst];
               iRet = ( nd1 < nd2 ) ? -1 : 1;
               break;
            }
         }

         if( ( n1 = ( UCHAR ) cdpage->s_chars[ ( UCHAR ) * szFirst ] ) == 0 ||
             ( n2 = ( UCHAR ) cdpage->s_chars[ ( UCHAR ) * szSecond ] ) == 0 )
         {
            /* One of characters doesn't belong to the national characters */
            iRet = ( ( UCHAR ) * szFirst < ( UCHAR ) * szSecond ) ? -1 : 1;
            break;
         }
         else if( n1 == n2 )
         {
            if( iAcc == 0 && ( fExact || ( ulLenFirst == ulLenSecond && cdpage->lAccInterleave ) ) )
            {
               if( cdpage->lAccInterleave )
                  iAcc = ( cdpage->s_accent[ ( UCHAR ) * szFirst ] <
                           cdpage->s_accent[ ( UCHAR ) * szSecond ] ) ? -1 : 1;
               else
                  iAcc = ( ( UCHAR ) * szFirst < ( UCHAR ) * szSecond ) ? -1 : 1;
            }
         }
         else
         {
            iRet = ( n1 < n2 ) ? -1 : 1;
            break;
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

static int hb_cdpMultiWeightI( PHB_CODEPAGE cdpage, const char *szChar )
{
   PHB_MULTICHAR pmulti = cdpage->multi;
   int j;

   for( j = 0; j < cdpage->nMulti; ++j, ++pmulti )
   {
      if( ( *szChar == pmulti->cFirst[0] ||
            *szChar == pmulti->cFirst[1] ) &&
          ( *( szChar + 1 ) == pmulti->cLast[0] || *( szChar + 1 ) == pmulti->cLast[1] ) )
      {
         return pmulti->nCode;
      }
   }

   return 0;
}

int hb_cdpicmp( const char *szFirst, ULONG ulLenFirst,
                const char *szSecond, ULONG ulLenSecond,
                PHB_CODEPAGE cdpage, BOOL fExact )
{
   int iRet = 0, iAcc = 0, n1 = 0, n2 = 0, u1, u2;
   ULONG ul, ulLen;

   ulLen = ulLenFirst < ulLenSecond ? ulLenFirst : ulLenSecond;
   for( ul = 0; ul < ulLen; ++szFirst, ++szSecond, ++ul )
   {
      u1 = ( UCHAR ) cdpage->s_upper[( UCHAR ) * szFirst];
      u2 = ( UCHAR ) cdpage->s_upper[( UCHAR ) * szSecond];
      if( u1 != u2 )
      {
         if( cdpage->nMulti )
         {
            int nd1, nd2;

            if( ul > 0 )
            {
               nd1 = hb_cdpMultiWeightI( cdpage, szFirst - 1 );
               nd2 = hb_cdpMultiWeightI( cdpage, szSecond - 1 );
               if( nd1 )
               {
                  if( nd2 )
                  {
                     if( nd1 == nd2 )
                     {
                        nd1 = ( UCHAR ) cdpage->s_chars[u1];
                        nd2 = ( UCHAR ) cdpage->s_chars[u2];
                        if( nd1 == nd2 || !nd1 || !nd2 )
                        {
                           nd1 = u1;
                           nd2 = u2;
                        }
                     }
                  }
                  else
                     nd2 = n2;
                  iRet = ( nd1 < nd2 ) ? -1 : 1;
                  break;
               }
               else if( nd2 )
               {
                  iRet = ( n1 < nd2 ) ? -1 : 1;
                  break;
               }
            }
            nd1 = ( ul < ulLenFirst - 1 ) ? hb_cdpMultiWeightI( cdpage, szFirst ) : 0;
            nd2 = ( ul < ulLenSecond - 1 ) ? hb_cdpMultiWeightI( cdpage, szSecond ) : 0;
            if( nd1 )
            {
               if( nd2 )
               {
                  if( nd1 == nd2 )
                  {
                     nd1 = ( UCHAR ) cdpage->s_chars[u1];
                     nd2 = ( UCHAR ) cdpage->s_chars[u2];
                     if( nd1 == nd2 || !nd1 || !nd2 )
                     {
                        nd1 = u1;
                        nd2 = u2;
                     }
                  }
               }
               else
                  nd2 = ( UCHAR ) cdpage->s_chars[u2];
               iRet = ( nd1 < nd2 ) ? -1 : 1;
               break;
            }
            else if( nd2 )
            {
               nd1 = ( UCHAR ) cdpage->s_chars[u1];
               iRet = ( nd1 < nd2 ) ? -1 : 1;
               break;
            }
         }

         if( ( n1 = ( UCHAR ) cdpage->s_chars[u1] ) == 0 ||
             ( n2 = ( UCHAR ) cdpage->s_chars[u2] ) == 0 )
         {
            /* One of characters doesn't belong to the national characters */
            iRet = ( u1 < u2 ) ? -1 : 1;
            break;
         }
         else if( n1 == n2 )
         {
            if( iAcc == 0 && ( fExact || ( ulLenFirst == ulLenSecond && cdpage->lAccInterleave ) ) )
            {
               if( cdpage->lAccInterleave )
                  iAcc = ( cdpage->s_accent[u1] < cdpage->s_accent[u2] ) ? -1 : 1;
               else
                  iAcc = ( u1 < u2 ) ? -1 : 1;
            }
         }
         else
         {
            iRet = ( n1 < n2 ) ? -1 : 1;
            break;
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

void hb_cdpReleaseAll( void )
{
   int iPos = 0;

   while( iPos < HB_CDP_MAX_ && s_cdpList[iPos] )
   {
      if( s_cdpList[iPos]->s_chars )
         hb_xfree( s_cdpList[iPos]->s_chars );
      if( s_cdpList[iPos]->s_upper )
         hb_xfree( s_cdpList[iPos]->s_upper );
      if( s_cdpList[iPos]->s_lower )
         hb_xfree( s_cdpList[iPos]->s_lower );
      if( s_cdpList[iPos]->s_accent )
         hb_xfree( s_cdpList[iPos]->s_accent );
      if( s_cdpList[iPos]->multi )
         hb_xfree( s_cdpList[iPos]->multi );
      if( s_cdpList[iPos]->lChClone )
      {
         hb_xfree( ( void * ) s_cdpList[iPos]->CharsUpper );
         hb_xfree( ( void * ) s_cdpList[iPos]->CharsLower );
      }
      iPos++;
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
   PHB_CODEPAGE cdp = hb_cdpFind( hb_parcx( 1 ) );

   hb_retc( cdp ? cdp->uniID : NULL );
}

HB_FUNC( HB_SETCODEPAGE )
{
   HB_FUNC_EXEC( HB_CDPSELECT );
}

HB_FUNC( HB_TRANSLATE )
{
   ULONG ulLen = hb_parclen( 1 );
   const char *szIdIn = hb_parc( 2 );
   const char *szIdOut = hb_parc( 3 );

   if( ulLen && ( szIdIn || szIdOut ) )
   {
      PHB_CODEPAGE cdpIn = szIdIn ? hb_cdpFindExt( szIdIn ) : hb_vmCDP();
      PHB_CODEPAGE cdpOut = szIdOut ? hb_cdpFindExt( szIdOut ) : hb_vmCDP();

      if( cdpIn && cdpOut && cdpIn != cdpOut )
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

HB_FUNC( HB_CDPLIST )
{
   int iCount, iPos;

   for( iCount = 0; iCount < HB_CDP_MAX_; ++iCount )
   {
      if( !s_cdpList[iCount] )
         break;
   }

   hb_reta( iCount );
   for( iPos = 0; iPos < iCount; ++iPos )
   {
      hb_storvc( s_cdpList[iPos]->id, -1, iPos + 1 );
   }
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
         ulDest = hb_cdpStringInUTF8Length( cdp, FALSE, szString, ulLen );
         szDest = ( char * ) hb_xgrab( ulDest + 1 );
         hb_cdpStrnToUTF8( cdp, FALSE, szString, ulLen, szDest );
      }
   }
   if( szDest )
      hb_retclen_buffer( szDest, ulDest );
   else
      hb_retc_null();
}

HB_FUNC( HB_UTF8CHR )
{
   if( HB_ISNUM( 1 ) )
   {
      char utf8Char[ HB_MAX_UTF8 ];
      int iLen;

      iLen = u16toutf8( utf8Char, ( USHORT ) hb_parni( 1 ) );
      hb_retclen( utf8Char, iLen );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
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
            ulDest = hb_cdpUTF8StringLength( szString, ulLen );
            szDest = ( char * ) hb_xgrab( ulDest + 1 );
            hb_cdpUTF8ToStrn( cdp, FALSE, szString, ulLen, szDest, ulDest );
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
         USHORT uc, uc2;
         int n, n2;

         --ulPos;
         uc = ( USHORT ) hb_parni( 3 );
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

/* non of numeric parameters in STRTRAN() (4-th and 5-th) refers to
 * character position in string so we do not need to create new
 * HB_UTF8STRTRAN() but we can safely use normal STRTRAN() function
 */
HB_FUNC_EXTERN( STRTRAN );

HB_FUNC( HB_UTF8STRTRAN )
{
   HB_FUNC_EXEC( STRTRAN )
}

#endif /* HB_CDP_SUPPORT_OFF */
