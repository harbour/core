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

#define HB_CDP_MAX_ 64

static HB_CODEPAGE  s_en_codepage = { "EN",0,NULL,NULL,0,0,0,0,NULL,NULL,NULL,NULL,0,NULL };
static PHB_CODEPAGE s_cdpList[ HB_CDP_MAX_ ];

PHB_CODEPAGE hb_cdp_page = &s_en_codepage;

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

BOOL  hb_cdpRegister( PHB_CODEPAGE cdpage )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_codepageRegister(%p)", cdpage));

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

               cdpage->lSort = FALSE;
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
                        multi[nMulti].cFirst[0] = *(ptrLower+1);
                        multi[nMulti].cFirst[1] = *(ptrUpper+1);
                        multi[nMulti].cLast[0] = *(ptrLower+2);
                        multi[nMulti].cLast[1] = *(ptrUpper+2);
                        multi[nMulti].nCode = i;

                        for( ptr=ptrUpper+4; *ptr; ptr++ )
                           *(ptr-4) = *ptr;
                        *(ptr-4) = '\0';
                        for( ptr=ptrLower+4; *ptr; ptr++ )
                           *(ptr-4) = *ptr;
                        *(ptr-4) = '\0';

                        nMulti ++;
                        ptrUpper --; ptrLower --;
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
                     printf( "\n %3d %3d %3d %3d %3d %3d %3d %3d",cdpage->s_chars[i*8],
                       cdpage->s_chars[i*8+1],cdpage->s_chars[i*8+2],
                       cdpage->s_chars[i*8+3],cdpage->s_chars[i*8+4],
                       cdpage->s_chars[i*8+5],cdpage->s_chars[i*8+6],
                       cdpage->s_chars[i*8+7] );
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

PHB_CODEPAGE  hb_cdpFind( char * pszID )
{
   int iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdpFind(%s)", pszID));

   iPos = hb_cdpFindPos( pszID );

   return ( iPos != -1 ) ? s_cdpList[ iPos ] : NULL;
}

PHB_CODEPAGE  hb_cdpSelect( PHB_CODEPAGE cdpage )
{
   PHB_CODEPAGE cdpOld = hb_cdp_page;

   HB_TRACE(HB_TR_DEBUG, ("hb_langSelect(%p)", cdpage));

   if( cdpage )
   {
      hb_cdp_page = cdpage;
   }

   return cdpOld;
}

char  * hb_cdpSelectID( char * pszID )
{
   char * pszIDOld = hb_cdp_page->id;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdpSelectID(%s)", pszID));

   hb_cdpSelect( hb_cdpFind( pszID ) );

   return pszIDOld;
}

void  hb_cdpTranslate( char* psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   int n;

   if( cdpIn != cdpOut && cdpIn->nChars == cdpOut->nChars )
   {
      int nAddLower = (cdpIn->lLatin)? 6:0;
      for( ; *psz; psz++ )
      {
         n = (int)cdpIn->s_chars[ ((int)*psz)&255 ];
         if( ( n != 0 ) &&
             ( ( n <= cdpIn->nChars ) || ( ( n > (cdpOut->nChars+nAddLower) ) &&
             ( n <= (cdpOut->nChars*2+nAddLower) ) ) ) )
         {
            n--;
            *psz = ( n >= (cdpOut->nChars+nAddLower) )?
                        cdpOut->CharsLower[n-cdpOut->nChars-nAddLower]:cdpOut->CharsUpper[n];
         }
      }
   }
}

void  hb_cdpnTranslate( char* psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, unsigned int nChars )
{
   int n;
   unsigned int i;

   if( cdpIn != cdpOut && cdpIn->nChars == cdpOut->nChars )
   {
      int nAddLower = (cdpIn->lLatin)? 6:0;
      for( i=0; i<nChars; i++,psz++ )
      {
         n = (int)cdpIn->s_chars[ ((int)*psz)&255 ];
         if( ( n != 0 ) &&
             ( ( n <= cdpIn->nChars ) || ( ( n > (cdpOut->nChars+nAddLower) ) &&
             ( n <= (cdpOut->nChars*2+nAddLower) ) ) ) )
         {
            n--;
            *psz = ( n >= (cdpOut->nChars+nAddLower) )?
                        cdpOut->CharsLower[n-cdpOut->nChars-nAddLower]:cdpOut->CharsUpper[n];
         }
      }
   }
}

int  hb_cdpchrcmp( char cFirst, char cSecond, PHB_CODEPAGE cdpage )
{
   int n1, n2;

   if( cFirst == cSecond )
      return 0;

   if( ( n1 = (int)cdpage->s_chars[ ((int)cFirst)&255 ] ) != 0 &&
       ( n2 = (int)cdpage->s_chars[ ((int)cSecond)&255 ] ) != 0 )
      return ( n1 < n2 )? -1 : 1;

   return ( (((int)cFirst)&255) < (((int)cSecond)&255) ) ? -1 : 1;

}

int  hb_cdpcmp( char* szFirst, char* szSecond, ULONG ulLen, PHB_CODEPAGE cdpage, ULONG* piCounter )
{
   ULONG ul;
   int iRet = 0, n1, n2;
   int lAcc1 = 0, lAcc2 = 0;
   /* printf( "\nhb_cdpcmp-0 %s %s",szFirst,szSecond ); */
   for( ul=0; ul<ulLen; ul++,szFirst++,szSecond++ )
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
         if( cdpage->nMulti && ul )
         {
            int j, nd1 = 0, nd2 = 0;
            PHB_MULTICHAR pmulti = cdpage->multi;
            /* printf( "\nhb_cdpcmp-1 %c %c",*szFirst,*szSecond ); */
            for( j=0; j<cdpage->nMulti; j++,pmulti++ )
            {
               if( ( *szFirst == pmulti->cLast[0] ||
                     *szFirst == pmulti->cLast[1] ) &&
                   ( *(szFirst-1) == pmulti->cFirst[0] ||
                     *(szFirst-1) == pmulti->cFirst[1] )  )
                   nd1 = pmulti->nCode;
               if( ( *szSecond == pmulti->cLast[0] ||
                     *szSecond == pmulti->cLast[1] ) &&
                   ( *(szSecond-1) == pmulti->cFirst[0] ||
                     *(szSecond-1) == pmulti->cFirst[1] )  )
                   nd2 = pmulti->nCode;
            }
            /* printf( "\nhb_cdpcmp-2 %d %d",nd1,nd2 ); */
            if( nd1 && !nd2 )
            {
               n2 = (int)cdpage->s_chars[ ((int)*(szSecond-1))&255 ];
               iRet = ( nd1 < n2 )? -1 : 1;
               break;
            }
            else if( !nd1 && nd2 )
            {
               n1 = (int)cdpage->s_chars[ ((int)*(szFirst-1))&255 ];
               iRet = ( n1 < nd2 )? -1 : 1;
               /* printf( "\nhb_cdpcmp-3 %d %d %d",iRet,n1,nd2 ); */
               break;
            }
            else if( nd1 && nd2 )
            {
               iRet = ( nd1 < nd2 )? -1 : 1;
               break;
            }
         }
         if( cdpage->lAccInterleave && !lAcc1 && !lAcc2 )
         {
            if( cdpage->s_accent[ ((int)*(szFirst))&255 ] == *((BYTE*)szSecond) )
               lAcc1 = TRUE;
            else if( cdpage->s_accent[ ((int)*(szSecond))&255 ] == *((BYTE*)szFirst) )
               lAcc2 = TRUE;
            else
            {
               iRet = ( n1 < n2 )? -1 : 1;
               break;
            }
         }
         else
         {
            iRet = ( n1 < n2 )? -1 : 1;
            break;
         }
      }
   /* printf( " : %d",iRet ); */

   if( piCounter )
      *piCounter = ul;
   if( !iRet && lAcc1 )
      return 1;
   else if( !iRet && lAcc2 )
      return -1;
   return iRet;
}

void  hb_cdpReleaseAll( void )
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

HB_CALL_ON_STARTUP_BEGIN( hb_codepage_Init_EN )
   hb_cdpRegister( &s_en_codepage );
HB_CALL_ON_STARTUP_END( hb_codepage_Init_EN )
#if defined(HB_STATIC_STARTUP) || ( (! defined(__GNUC__)) && (! defined(_MSC_VER)) )
   #pragma startup hb_codepage_Init_EN
#endif

#endif /* HB_CDP_SUPPORT_OFF */
