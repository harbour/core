/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __STRTOKEN() helper routine for TEDITOR.PRG
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 *                Matthew Hamilton <mhamilton@bunge.com.au>
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

static char * hb_strToken( char * szText, ULONG ulText,
                           ULONG ulIndex,
                           char cDelimiter,
                           ULONG * pulLen )
{
   ULONG ulStart;
   ULONG ulEnd = 0;
   ULONG ulCounter = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_strToken(%s, %lu, %lu, %d, %p)", szText, ulText, ulIndex, (int) cDelimiter, pulLen));

   do
   {
      ulStart = ulEnd;

      if( cDelimiter != ' ' )
      {
         if( szText[ ulStart ] == cDelimiter )
            ulStart++;
      }
      else
      {
         while( ulStart < ulText && szText[ ulStart ] == cDelimiter )
            ulStart++;
      }

      if( ulStart < ulText && szText[ ulStart ] != cDelimiter )
      {
         ulEnd = ulStart + 1;

         while( ulEnd < ulText && szText[ ulEnd ] != cDelimiter )
            ulEnd++;
      }
      else
         ulEnd = ulStart;

   }
   while( ulCounter++ < ulIndex - 1 && ulEnd < ulText );

   if( ulCounter < ulIndex )
   {
      *pulLen = 0;
      return "";
   }
   else
   {
      *pulLen = ulEnd - ulStart;
      return szText + ulStart;
   }
}

/* returns the nth occurence of a substring within a token-delimited string */
HB_FUNC( __STRTOKEN )
{
   char * pszText;
   ULONG ulLen;

   pszText = hb_strToken( hb_parc( 1 ), hb_parclen( 1 ),
                          hb_parnl( 2 ),
                          ISCHAR( 3 ) ? *hb_parc( 3 ) : ' ',
                          &ulLen );

   hb_retclen( pszText, ulLen );
}


/* like __STRTOKEN() but returns next token starting from passed position
   (0 based) inside string.
   __StrTkPtr( cString, @nTokPos, Chr( 9 ) )
*/
HB_FUNC( __STRTKPTR )
{
   char * pszString = hb_parc( 1 );
   ULONG ulStrLen = hb_parclen( 1 );
   ULONG ulLen;
   ULONG ulPos = hb_parnl( 2 );
   char * pszText;

   /* move start of string past last returned token */
   pszString += ulPos;

   /* decrease length of string consequently */
   ulStrLen -= ulPos + 1;

   pszText = hb_strToken( pszString, ulStrLen,
                          1,
                          ISCHAR( 3 ) ? *hb_parc( 3 ) : ' ',
                          &ulLen );

   /* return position to start next search from */
   hb_stornl( pszText - pszString + ulPos + ulLen, 2 );

   /* return token */
   hb_retclen( pszText, ulLen );
}

