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
#include "hbapicdp.h"

#define HB_CDP_MAX_ 64

static HB_CODEPAGE  s_en_codepage = { "EN",0,NULL,NULL,0,NULL,NULL,NULL };
static PHB_CODEPAGE s_cdpList[ HB_CDP_MAX_ ];
PHB_CODEPAGE s_cdpage = &s_en_codepage;

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

BOOL hb_cdpRegister( PHB_CODEPAGE cdpage )
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

               s_cdpList[ iPos ] = cdpage;

               cdpage->lSort = FALSE;
               if( cdpage->nChars )
               {
                  cdpage->s_chars = (BYTE*) hb_xgrab(256);
                  cdpage->s_upper = (BYTE*) hb_xgrab(256);
                  cdpage->s_lower = (BYTE*) hb_xgrab(256);
                  for( i=0; i<256; i++ )
                  {
                     cdpage->s_chars[i] = 0;
                     cdpage->s_upper[i] = toupper( (BYTE) i&255 );
                     cdpage->s_lower[i] = tolower( (BYTE) i&255 );
                  }
                  for( i=1; *ptrUpper; i++,ptrUpper++,ptrLower++ )
                  {
                     if( *ptrUpper == '=' )
                     {
                        for( ptr=ptrUpper+1; *ptr; ptr++ )
                           *(ptr-1) = *ptr;
                        *(ptr-1) = '\0';
                        for( ptr=ptrLower+1; *ptr; ptr++ )
                           *(ptr-1) = *ptr;
                        *(ptr-1) = '\0';
                        i--;
                     }
                     else if( *ptrUpper == '.' )
                     {
                     }
                     iu = (((int)*ptrUpper)&255);
                     cdpage->s_chars[ iu ] = i;
                     il = (((int)*ptrLower)&255);
                     cdpage->s_chars[ il ] = i+cdpage->nChars;
                     if( iu < iumax || il < ilmax )
                        cdpage->lSort = TRUE;
                     iumax = iu; ilmax = il;
                     
                     iu = ((int)(*ptrLower))&255;
                     cdpage->s_upper[iu] = *ptrUpper;
                     il = ((int)(*ptrUpper))&255;
                     cdpage->s_lower[il] = *ptrLower;
                  }
               }
               // printf( "\n%s %d",s_cdpage->id,s_cdpage->lSort );
               return TRUE;
            }
         }
      }
   }

   return FALSE;
}

PHB_CODEPAGE hb_cdpFind( char * pszID )
{
   int iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdpFind(%s)", pszID));

   iPos = hb_cdpFindPos( pszID );

   return ( iPos != -1 ) ? s_cdpList[ iPos ] : NULL;
}

PHB_CODEPAGE hb_cdpSelect( PHB_CODEPAGE cdpage )
{
   PHB_CODEPAGE cdpOld = s_cdpage;

   HB_TRACE(HB_TR_DEBUG, ("hb_langSelect(%p)", cdpage));

   if( cdpage )
      s_cdpage = cdpage;

   return cdpOld;
}

char * hb_cdpSelectID( char * pszID )
{
   char * pszIDOld = s_cdpage->id;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdpSelectID(%s)", pszID));

   hb_cdpSelect( hb_cdpFind( pszID ) );

   return pszIDOld;
}

void hb_cdpTranslate( char* psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut )
{
   int n;

   if( cdpIn != cdpOut && cdpIn->nChars == cdpOut->nChars )
   {
      for( ; *psz; psz++ )
      {
         if( ( n = (int)cdpIn->s_chars[ ((int)*psz)&255 ] ) != 0 )
         {
            n--;
            *psz = ( n>=cdpOut->nChars )? 
                        cdpOut->CharsLower[n-cdpOut->nChars]:cdpOut->CharsUpper[n];
         }
      }
   }
}

void hb_cdpnTranslate( char* psz, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, unsigned int nChars )
{
   int n;
   unsigned int i;

   if( cdpIn != cdpOut && cdpIn->nChars == cdpOut->nChars )
      for( i=0; i<nChars; i++,psz++ )
      {
         if( ( n = (int)cdpIn->s_chars[ ((int)*psz)&255 ] ) != 0 )
         {
            n--;
            *psz = ( n>=cdpOut->nChars )? 
                        cdpOut->CharsLower[n-cdpOut->nChars]:cdpOut->CharsUpper[n];
         }
      }
}

int hb_cdpcmp( char* szFirst, char* szSecond, int iLen, PHB_CODEPAGE cdpage )
{
   int i, iRet = 0, n1, n2;
   // printf( "\nhb_cdpcmp-0 %s %s",szFirst,szSecond );
   for( i=0; i<iLen; i++,szFirst++,szSecond++ )
      if( *szFirst != *szSecond )
      {
         if( ( n1 = (int)cdpage->s_chars[ ((int)*szFirst)&255 ] ) == 0 ||
             ( n2 = (int)cdpage->s_chars[ ((int)*szSecond)&255 ] ) == 0 )
            iRet = ( *szFirst < *szSecond )? -1 : 1;
         else
            iRet = ( n1 < n2 )? -1 : 1;
         break;
      }
   // printf( " : %d",iRet );
   return iRet;
}

void hb_cdpReleaseAll( void )
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
      iPos ++;
   }
}

HB_FUNC( HB_SETCODEPAGE )
{
   hb_retc( s_cdpage->id );

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
      cdpIn  = ( szIdIn )? hb_cdpFind( szIdIn ):s_cdpage;
      cdpOut = ( szIdOut )? hb_cdpFind( szIdOut ):s_cdpage;

      szResult = (char*) hb_xgrab( ilen + 1 );
      memcpy( szResult, szIn, ilen );
      hb_cdpTranslate( szResult, cdpIn, cdpOut );

      hb_retc_buffer( szResult );
   }
   else
      hb_retc( "" );
}

HB_CALL_ON_STARTUP_BEGIN( hb_codepage_Init_EN )
   hb_cdpRegister( &s_en_codepage );
HB_CALL_ON_STARTUP_END( hb_codepage_Init_EN )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_codepage_Init_EN
#endif
