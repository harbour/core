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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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
   hb_stornl( pszText - pszString + ulPos + ulLen + 1, 2 );

   /* return token */
   hb_retclen( pszText, ulLen );
}

