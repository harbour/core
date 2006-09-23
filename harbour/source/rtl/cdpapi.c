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

#include "hbapicdp.h"

#define NUMBER_OF_CHARS    256

static USHORT uniCodes[NUMBER_OF_CHARS] = {
   0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007,
   0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F,
   0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017,
   0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F,
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
   0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0,
};

HB_UNITABLE hb_uniTbl_437 = { CPID_437, NUMBER_OF_CHARS, FALSE, uniCodes };

static HB_CODEPAGE  s_en_codepage = { "EN",CPID_437,UNITB_437,0,NULL,NULL,0,0,0,0,0,NULL,NULL,NULL,NULL,0,NULL };

#define HB_CDP_MAX_ 64

static PHB_CODEPAGE s_cdpList[ HB_CDP_MAX_ ] = { &s_en_codepage };
PHB_CODEPAGE hb_cdp_page = &s_en_codepage;


static int utf8Size( USHORT uc )
{
   if ( uc <  0x0080 )
      return 1;

   else if ( uc <  0x0800 )
      return 2;

   else /* if ( uc <= 0xffff ) */
      return 3;
}

static int u16toutf8( BYTE *szUTF8, USHORT uc )
{
   int n;

   if ( uc <  0x0080 )
   {
      szUTF8[0] = uc & 0xff;
      n = 1;
   }
   else if ( uc <  0x0800 )
   {
      szUTF8[0] = 0xc0 | ( ( uc >> 6 ) & 0x1f );
      szUTF8[1] = 0x80 | ( uc & 0x3f );
      n = 2;
   }
   else /* if ( uc <= 0xffff ) */
   {
      szUTF8[0] = 0xe0 | ( ( uc >> 12 ) & 0x0f);
      szUTF8[1] = 0x80 | ( ( uc >> 6 ) & 0x3f);
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

static BOOL utf8tou16nextchar( BYTE byChar, int * n, USHORT * uc )
{
   if ( *n > 0 )
   {
      if ( ( byChar & 0xc0 ) != 0x80 )
         return FALSE;
      *uc = ( *uc << 6 ) | ( byChar & 0x3f );
      (*n)--;
      return TRUE;
   }

   *n = 0;
   *uc = byChar;
   if ( byChar >= 0xc0 )
   {
      if ( byChar < 0xe0 )
      {
         *uc &= 0x1f;
         *n = 1;
      }
      else if ( byChar < 0xf0 )
      {
         *uc &= 0x0f;
         *n = 2;
      }
      else if ( byChar < 0xf8 )
      {
         *uc &= 0x07;
         *n = 3;
      }
      else if ( byChar < 0xfc )
      {
         *uc &= 0x03;
         *n = 4;
      }
      else if ( byChar < 0xfe )
      {
         *uc &= 0x01;
         *n = 5;
      }
   }
   return TRUE;
}

#if 0  /* currently unused, it will in the future */
static int utf8tou16( BYTE *szUTF8, USHORT *uc )
{
   int n = 1, m = 1;
   UINT32 u32;

   u32 = *szUTF8;
   if ( u32 >= 0xc0 )
   {
      if ( u32 < 0xe0 )
      {
         u32 &= 0x1f;
         m = 2;
      }
      else if ( u32 < 0xf0 )
      {
         u32 &= 0x0f;
         m = 3;
      }
      else if ( u32 < 0xf8 )
      {
         u32 &= 0x07;
         m = 4;
      }
      else if ( u32 < 0xfc )
      {
         u32 &= 0x03;
         m = 5;
      }
      else if ( u32 < 0xfe )
      {
         u32 &= 0x01;
         m = 6;
      }
      while ( n < m && ( szUTF8[n] & 0xc0 ) == 0x80 )
      {
         u32 = ( u32 << 6 ) | ( szUTF8[n++] & 0x3f );
      }
      if ( n < m )
      {
         u32 <<= 6 * (m - n);
      }
   }

   *uc = (USHORT) u32;
   return n;
}
#endif

static int hb_cdpFindPos( char * pszID )
{
   int iPos;

   if( pszID != NULL )
   {
      for( iPos = 0; iPos < HB_CDP_MAX_ && s_cdpList[iPos]; iPos++ )
      {
         if( strcmp( s_cdpList[ iPos ]->id, pszID ) == 0 )
            return iPos;
      }
   }

   return -1;
}

HB_EXPORT BOOL hb_cdpRegister( PHB_CODEPAGE cdpage )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpRegister(%p)", cdpage));

   if( cdpage )
   {
      int iPos = hb_cdpFindPos( cdpage->id );

      if( iPos == -1 )
      {
         for( iPos = 0; iPos < HB_CDP_MAX_; iPos++ )
         {
            if( !s_cdpList[ iPos ] )
            {
               int i, iu, il, iumax = 0, ilmax = 0;
               char *ptrUpper = cdpage->CharsUpper;
               char *ptrLower = cdpage->CharsLower;
               char *ptr;
               HB_MULTICHAR multi[12];
               int nMulti = 0;

               s_cdpList[ iPos ] = cdpage;

               cdpage->lSort = cdpage->lAccInterleave || cdpage->lAccEqual;
               cdpage->lChClone = FALSE;
               if( cdpage->nChars )
               {
                  int nAddLower = cdpage->nChars + ( (cdpage->lLatin)? 6:0 );
                  cdpage->s_chars = (BYTE*) hb_xgrab(256);
                  memset( cdpage->s_chars,'\0',256 );
                  cdpage->s_upper = (BYTE*) hb_xgrab(256);
                  cdpage->s_lower = (BYTE*) hb_xgrab(256);
                  if( cdpage->lAccInterleave )
                  {
                     cdpage->s_accent = (BYTE*) hb_xgrab(256);
                     memset( cdpage->s_accent,'\0',256 );
                  }
                  else
                     cdpage->s_accent = NULL;

                  for( i=0; i<256; i++ )
                  {
                     cdpage->s_upper[i] = toupper( (BYTE) i&255 );
                     cdpage->s_lower[i] = tolower( (BYTE) i&255 );
                  }
                  if( strpbrk(cdpage->CharsUpper, "~.") != NULL )
                  {
                     ptrUpper = cdpage->CharsUpper = hb_strdup(cdpage->CharsUpper);
                     ptrLower = cdpage->CharsLower = hb_strdup(cdpage->CharsLower);
                     cdpage->lChClone = TRUE;
                  }
                  for( i=1; *ptrUpper; i++,ptrUpper++,ptrLower++ )
                  {
                     if( *ptrUpper == '~' )
                     {
                        for( ptr=ptrUpper+1; *ptr; ptr++ )
                           *(ptr-1) = *ptr;
                        *(ptr-1) = '\0';
                        for( ptr=ptrLower+1; *ptr; ptr++ )
                           *(ptr-1) = *ptr;
                        *(ptr-1) = '\0';
                        if( cdpage->lAccEqual )
                           i--;
                        if( cdpage->lAccInterleave )
                        {
                           char *ptrtmp;

                           ptrtmp = ptrUpper-1;
                           while( cdpage->s_accent[ (((int)*ptrtmp)&255) ] )
                              ptrtmp --;
                           iu = (((int)*ptrUpper)&255);
                           cdpage->s_accent[ iu ] = *ptrtmp;

                           ptrtmp = ptrLower-1;
                           while( cdpage->s_accent[ (((int)*ptrtmp)&255) ] )
                              ptrtmp --;
                           il = (((int)*ptrLower)&255);
                           cdpage->s_accent[ il ] = *ptrtmp;
                        }
                     }
                     else if( *ptrUpper == '.' )
                     {
                        multi[nMulti].cFirst[0] = *(ptrUpper+1);
                        multi[nMulti].cFirst[1] = *(ptrLower+1);
                        multi[nMulti].cLast[0] = *(ptrUpper+2);
                        multi[nMulti].cLast[1] = *(ptrLower+2);
                        multi[nMulti].nCode = i;

                        for( ptr=ptrUpper+4; *ptr; ptr++ )
                           *(ptr-4) = *ptr;
                        *(ptr-4) = '\0';
                        for( ptr=ptrLower+4; *ptr; ptr++ )
                           *(ptr-4) = *ptr;
                        *(ptr-4) = '\0';

                        nMulti ++;
                        ptrUpper --; ptrLower --;
                        cdpage->lSort = TRUE;
                        continue;
                     }
                     iu = (((int)*ptrUpper)&255);
                     cdpage->s_chars[ iu ] = i;
                     il = (((int)*ptrLower)&255);
                     cdpage->s_chars[ il ] = i+nAddLower;
                     if( iu < iumax || il < ilmax )
                        cdpage->lSort = TRUE;
                     iumax = iu; ilmax = il;

                     iu = ((int)(*ptrLower))&255;
                     cdpage->s_upper[iu] = *ptrUpper;
                     il = ((int)(*ptrUpper))&255;
                     cdpage->s_lower[il] = *ptrLower;
                  }
                  if( cdpage->lLatin )
                  {
                     for( i=91; i<=96; i++ )
                     {
                        if( !cdpage->s_chars[i] )
                           cdpage->s_chars[i] = cdpage->nChars + (i-90);
                     }
                     for( i=123; i<=255; i++ )
                     {
                        if( !cdpage->s_chars[i] )
                           cdpage->s_chars[i] = cdpage->nChars + nAddLower + (i-122);
                     }
                  }
                  /*
                  for( i=0; i<32; i++ )
                     printf( "\n %3d %3d %3d %3d %3d %3d %3d %3d",cdpage->s_accent[i*8],
                       cdpage->s_accent[i*8+1],cdpage->s_accent[i*8+2],
                       cdpage->s_accent[i*8+3],cdpage->s_accent[i*8+4],
                       cdpage->s_accent[i*8+5],cdpage->s_accent[i*8+6],
                       cdpage->s_accent[i*8+7] );
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
                        printf( "\n %c%c %c%c %d",multi[i].cFirst[0],
                             multi[i].cFirst[1],multi[i].cLast[0],multi[i].cLast[1],multi[i].nCode );
                  }
                  */
                  if( nMulti )
                  {
                     cdpage->multi = (PHB_MULTICHAR) hb_xgrab( sizeof(HB_MULTICHAR)*nMulti );
                     memcpy( (BYTE*)cdpage->multi,(BYTE*)multi,sizeof(HB_MULTICHAR)*nMulti );
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

HB_EXPORT PHB_CODEPAGE hb_cdpFind( char * pszID )
{
   int iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdpFind(%s)", pszID));

   iPos = hb_cdpFindPos( pszID );

   return ( iPos != -1 ) ? s_cdpList[ iPos ] : NULL;
}

HB_EXPORT PHB_CODEPAGE hb_cdpSelect( PHB_CODEPAGE cdpage )
{
   PHB_CODEPAGE cdpOld = hb_cdp_page;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdpSelect(%p)", cdpage));

   if( cdpage )
   {
      hb_cdp_page = cdpage;
   }

   return cdpOld;
}

HB_EXPORT char * hb_cdpSelectID( char * pszID )
{
   char * pszIDOld = hb_cdp_page->id;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdpSelectID(%s)", pszID));

   hb_cdpSelect( hb_cdpFind( pszID ) );

   return pszIDOld;
}

HB_EXPORT void hb_cdpTranslate( char* psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   int n;

   if( cdpIn != cdpOut && cdpIn->nChars == cdpOut->nChars )
   {
      int nAddLower = (cdpIn->lLatin)? 6:0;
      for( ; *psz; psz++ )
      {
         n = (int)cdpIn->s_chars[ ((int)*psz)&255 ];
         if( n != 0 &&
             ( n <= cdpOut->nChars || ( n > (cdpOut->nChars+nAddLower) &&
               n <= (cdpOut->nChars*2+nAddLower) ) ) )
         {
            n--;
            *psz = ( n >= (cdpOut->nChars+nAddLower) )?
                        cdpOut->CharsLower[n-cdpOut->nChars-nAddLower]:cdpOut->CharsUpper[n];
         }
      }
   }
}

HB_EXPORT void hb_cdpnTranslate( char* psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, ULONG nChars )
{
   int n;
   ULONG i;

   if( cdpIn != cdpOut && cdpIn->nChars == cdpOut->nChars )
   {
      int nAddLower = (cdpIn->lLatin)? 6:0;
      for( i=0; i<nChars; i++,psz++ )
      {
         n = (int)cdpIn->s_chars[ ((int)*psz)&255 ];
         if( n != 0 &&
             ( n <= cdpOut->nChars || ( n > (cdpOut->nChars+nAddLower) &&
               n <= (cdpOut->nChars*2+nAddLower) ) ) )
         {
            n--;
            *psz = ( n >= (cdpOut->nChars+nAddLower) )?
                        cdpOut->CharsLower[n-cdpOut->nChars-nAddLower]:cdpOut->CharsUpper[n];
         }
      }
   }
}

HB_EXPORT USHORT hb_cdpGetU16( PHB_CODEPAGE cdp, BOOL fCtrl, BYTE ch )
{
   USHORT u;

   if ( ( fCtrl || ch >= 32 ) && cdp && cdp->uniTable &&
        cdp->uniTable->uniCodes && ch < cdp->uniTable->nChars )
   {
      u = cdp->uniTable->uniCodes[ ch ];
   }
   else
   {
      u = ch;
   }
   return u;
}

HB_EXPORT ULONG hb_cdpUTF8StringLength( BYTE * pSrc, ULONG ulLen )
{
   ULONG ul, ulDst;
   USHORT uc;
   int n = 0;

   for( ul = ulDst = 0; ul < ulLen; ++ul )
   {
      if( utf8tou16nextchar( pSrc[ ul ], &n, &uc ) )
      {
         if( n == 0 )
            ++ulDst;
      }
   }

   return ulDst;
}

HB_EXPORT ULONG hb_cdpStringInUTF8Length( PHB_CODEPAGE cdp, BOOL fCtrl, BYTE * pSrc, ULONG ulLen )
{
   ULONG ul, ulDst;

   for( ul = ulDst = 0; ul < ulLen; ++ul )
   {
      ulDst += utf8Size( hb_cdpGetU16( cdp, fCtrl, pSrc[ ul ] ) );
   }

   return ulDst;
}

HB_EXPORT ULONG hb_cdpUTF8ToStrn( PHB_CODEPAGE cdp,
                                  BYTE * pSrc, ULONG ulSrc,
                                  BYTE * pDst, ULONG ulDst )
{
   ULONG ulS, ulD;
   USHORT uc = 0;
   int n = 0;

   for( ulS = ulD = 0; ulS < ulSrc; ++ulS )
   {
      if( utf8tou16nextchar( pSrc[ ulS ], &n, &uc ) )
      {
         if( n == 0 )
         {
            if( ulD < ulDst )
            {
               if( cdp->uniTable && cdp->uniTable->uniCodes )
               {
                  int i;
                  for ( i = 0; i < cdp->uniTable->nChars; i++ )
                  {
                     if ( cdp->uniTable->uniCodes[ i ] == uc )
                     {
                        uc = ( USHORT ) i;
                        break;
                     }
                  }
               }
               pDst[ ulD ] = uc >= 0x100 ? '?' : uc;
            }
            ++ulD;
         }
      }
   }

   if( ulD < ulDst )
      pDst[ ulD ] = '\0';

   return ulD;
}

HB_EXPORT BOOL hb_cdpGetFromUTF8( PHB_CODEPAGE cdp, BYTE ch, int * n, USHORT * uc )
{
   if ( utf8tou16nextchar( ch, n, uc ) )
   {
      if ( *n == 0 && cdp && cdp->uniTable && cdp->uniTable->uniCodes )
      {
         int i;
         for ( i = 0; i < cdp->uniTable->nChars; i++ )
         {
            if ( cdp->uniTable->uniCodes[ i ] == *uc )
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

HB_EXPORT ULONG hb_cdpStrnToUTF8( PHB_CODEPAGE cdp, BOOL fCtrl, BYTE* pSrc, ULONG ulLen, BYTE* pDst )
{
   USHORT u, *uniCodes, nChars;
   ULONG i, n;

   if ( cdp && cdp->uniTable )
   {
      if ( cdp->nMulti || cdp->uniTable->lMulti )
      {
         /*
          * TODO: this translation is bad, please fix me!!!
          */
         for ( i = 0, n = 0; i < ulLen; i++ )
         {
            u = hb_cdpGetU16( cdp, fCtrl, pSrc[ i ] );
            n += u16toutf8( &pDst[n], u );
         }
         return n;
      }
      else
      {
         uniCodes = cdp->uniTable->uniCodes;
         nChars = cdp->uniTable->nChars;
      }
   }
   else
   {
      nChars = 0;
      uniCodes = NULL;
   }

   for ( i = 0, n = 0; i < ulLen; i++ )
   {
      u = pSrc[ i ];
      if ( uniCodes && u < nChars && ( fCtrl || u >= 32 ) )
         u = uniCodes[ u ];
      n += u16toutf8( &pDst[n], u );
   }
   pDst[ n ] = '\0';

   return n;
}

HB_EXPORT ULONG hb_cdpStrnToU16( PHB_CODEPAGE cdp, BOOL fCtrl, BYTE* pSrc, ULONG ulLen, BYTE* pDst )
{
   USHORT u, *uniCodes, nChars;
   ULONG i;

   if ( cdp && cdp->uniTable )
   {
      if ( cdp->nMulti || cdp->uniTable->lMulti )
      {
         /*
          * TODO: this translation is bad, please fix me!!!
          */
         for ( i = 0; i < ulLen; i++, pDst += 2 )
         {
            u = hb_cdpGetU16( cdp, fCtrl, pSrc[ i ] );
            HB_PUT_BE_UINT16( pDst, u );
         }
         return i<<1;
      }
      else
      {
         uniCodes = cdp->uniTable->uniCodes;
         nChars = cdp->uniTable->nChars;
      }
   }
   else
   {
      nChars = 0;
      uniCodes = NULL;
   }

   for ( i = 0; i < ulLen; i++, pDst += 2 )
   {
      u = pSrc[ i ];
      if ( uniCodes && u < nChars && ( fCtrl || u >= 32 ) )
         u = uniCodes[ u ];
      HB_PUT_BE_UINT16( pDst, u );
   }
   return i<<1;
}

HB_EXPORT int hb_cdpchrcmp( char cFirst, char cSecond, PHB_CODEPAGE cdpage )
{
   int n1, n2;

   if( cFirst == cSecond )
      return 0;

   if( ( n1 = (int)cdpage->s_chars[ ((int)cFirst)&255 ] ) != 0 &&
       ( n2 = (int)cdpage->s_chars[ ((int)cSecond)&255 ] ) != 0 )
      return ( n1 < n2 )? -1 : 1;

   return ( (((int)cFirst)&255) < (((int)cSecond)&255) ) ? -1 : 1;

}

HB_EXPORT int hb_cdpcmp( char* szFirst, ULONG ulLenFirst, char* szSecond, ULONG ulLenSecond, PHB_CODEPAGE cdpage, BOOL bExact )
{
   ULONG ul, ulLen;
   int iRet = 0, n1, n2;
   int nAcc1 = 0, nAcc2 = 0;
   /* printf( "\nhb_cdpcmp-0 %s %s",szFirst,szSecond ); */

   ulLen = ulLenFirst < ulLenSecond ? ulLenFirst : ulLenSecond;

   for( ul=0; ul<ulLen; ul++,szFirst++,szSecond++ )
   {
      if( cdpage->nMulti && ( *szFirst != *szSecond  || *(szFirst+1) != *(szSecond+1) ) )
      {
         int j, nd1 = 0, nd2 = 0;
         PHB_MULTICHAR pmulti = cdpage->multi;
         // printf( "\nhb_cdpcmp-1 %c %c",*szFirst,*szSecond );
         ul ++;
         for( j=0; j<cdpage->nMulti; j++,pmulti++ )
         {
            if( ( ul < ulLenFirst ) && 
                ( *(szFirst+1) == pmulti->cLast[0] ||
                  *(szFirst+1) == pmulti->cLast[1] ) &&
                ( *szFirst == pmulti->cFirst[0] ||
                  *szFirst == pmulti->cFirst[1] ) )
            {
                nd1 = pmulti->nCode + 
                      ( (*szFirst == pmulti->cFirst[0])? 0 :
                           ( cdpage->nChars + ( (cdpage->lLatin)? 6:0 ) ) );
            }
            if( ( ul < ulLenSecond ) && 
                ( *(szSecond+1) == pmulti->cLast[0] ||
                  *(szSecond+1) == pmulti->cLast[1] ) &&
                ( *szSecond == pmulti->cFirst[0] ||
                  *szSecond == pmulti->cFirst[1] ) )
            {
                nd2 = pmulti->nCode +
                      ( (*szSecond == pmulti->cFirst[0])? 0 :
                           ( cdpage->nChars + ( (cdpage->lLatin)? 6:0 ) ) );
            }
         }
         ul --;
         if( nd1 && !nd2 )
         {
            n2 = (int)cdpage->s_chars[ ((int)(*szSecond))&255 ];
            iRet = ( nd1 < n2 )? -1 : 1;
            // printf( "\nhb_cdpcmp-2 %d %d %d",iRet,nd1,n2 );
            break;
         }
         else if( !nd1 && nd2 )
         {
            n1 = (int)cdpage->s_chars[ ((int)(*szFirst))&255 ];
            iRet = ( n1 < nd2 )? -1 : 1;
            // printf( "\nhb_cdpcmp-3 %d %d %d",iRet,n1,nd2 );
            break;
         }
         else if( nd1 && nd2 )
         {
            iRet = ( nd1 == nd2 )? ( ( *(szFirst+1) < *(szSecond+1) )? -1 : 1 ) :
                   ( ( nd1 < nd2 )? -1 : 1 );
            break;
         }
      }
      if( *szFirst != *szSecond )
      {
         if( ( n1 = (int)cdpage->s_chars[ ((int)*szFirst)&255 ] ) == 0 ||
             ( n2 = (int)cdpage->s_chars[ ((int)*szSecond)&255 ] ) == 0 )
         {
            /* One of characters doesn't belong to the national characters */
            iRet = ( (((int)*szFirst)&255) < (((int)*szSecond)&255) )? -1 : 1;
            /* printf( "\n|%c|%c|%d %d %d",*szFirst,*szSecond,((int)*szFirst)&255,((int)*szSecond)&255,iRet ); */
            break;
         }
         if( ( n1 == n2 ) && !bExact )
         {
            continue;
         }
         if( cdpage->lAccInterleave && !nAcc1 && !nAcc2 )
         {
            BYTE a1, a2;
            a1 = cdpage->s_accent[ ((int)*(szFirst))&255 ];
            a2 = cdpage->s_accent[ ((int)*(szSecond))&255 ];
            if( ( a1 || a2 ) &&
                ( a1 == a2 || a1 == *((BYTE*)szSecond) || a2 == *((BYTE*)szFirst) )
                )
            {
               /* printf( "\nhb_cdpcmp-3A %d %d",a1,a2 ); */
               if ( a1 == *((BYTE*)szSecond) || a1 == a2 )
                  nAcc1 = n1;
               if( a2 == *((BYTE*)szFirst) || a1 == a2 )
                  nAcc2 = n2;
            }
            else
            {
               iRet = ( n1 == n2 )? ( ( *szFirst < *szSecond )? -1 : 1 ) :
                      ( ( n1 < n2 )? -1 : 1 );
               break;
            }
         }
         else
         {
            iRet = ( n1 == n2 )? ( ( *szFirst < *szSecond )? -1 : 1 ) :
                   ( ( n1 < n2 )? -1 : 1 );
            break;
         }
      }
   }
   /* printf( "\r\n : %d %d %d",iRet,nAcc1,nAcc2 ); */

   if( !iRet )
   {
      if( ( bExact || ( ulLenSecond > ulLenFirst ) || ( nAcc1 != nAcc2 && ulLenSecond < ulLenFirst ) )
         && ( ulLenSecond != ulLenFirst ) )
      {
         iRet = ( ulLenFirst < ulLenSecond ) ? -1 : 1;
      }
      else if( nAcc1 > nAcc2 )
      {
         iRet = 1;
      }
      else if( nAcc2 > nAcc1 )
      {
         iRet = -1;
      }
   }

   return iRet;
}

HB_EXPORT void hb_cdpReleaseAll( void )
{
   int iPos = 0;

   while( iPos < HB_CDP_MAX_ && s_cdpList[iPos] )
   {
      if( s_cdpList[ iPos ]->s_chars )
         hb_xfree( s_cdpList[ iPos ]->s_chars );
      if( s_cdpList[ iPos ]->s_upper )
         hb_xfree( s_cdpList[ iPos ]->s_upper );
      if( s_cdpList[ iPos ]->s_lower )
         hb_xfree( s_cdpList[ iPos ]->s_lower );
      if( s_cdpList[ iPos ]->s_accent )
         hb_xfree( s_cdpList[ iPos ]->s_accent );
      if( s_cdpList[ iPos ]->multi )
         hb_xfree( s_cdpList[ iPos ]->multi );
      if( s_cdpList[ iPos ]->lChClone )
      {
         hb_xfree( s_cdpList[ iPos ]->CharsUpper );
         hb_xfree( s_cdpList[ iPos ]->CharsLower );
      }
      iPos ++;
   }
}

HB_FUNC( HB_SETCODEPAGE )
{
   hb_retc( hb_cdp_page->id );

   if( ISCHAR(1) )
      hb_cdpSelectID( hb_parc( 1 ) );
}

HB_FUNC( HB_TRANSLATE )
{
   char *szResult;
   char *szIn = hb_parc(1);
   char *szIdIn = hb_parc(2);
   char *szIdOut = hb_parc(3);
   int  ilen;
   PHB_CODEPAGE cdpIn;
   PHB_CODEPAGE cdpOut;

   if( szIn )
   {
      ilen = hb_parclen(1);
      cdpIn  = ( szIdIn )? hb_cdpFind( szIdIn ):hb_cdp_page;
      cdpOut = ( szIdOut )? hb_cdpFind( szIdOut ):hb_cdp_page;
      szResult = (char*) hb_xgrab( ilen + 1 );
      memcpy( szResult, szIn, ilen );
      szResult[ ilen ] = '\0';
      hb_cdpTranslate( szResult, cdpIn, cdpOut );

      hb_retc_buffer( szResult );
   }
   else
      hb_retc( "" );
}

#endif /* HB_CDP_SUPPORT_OFF */
