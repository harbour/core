/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __STRTOKEN() helper routine for TEDITOR.PRG
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

static char * hb_strToken( char * szText, long lText,
                           long lIndex,
                           char cDelimiter,
                           long * plLen )
{
   long lStart;
   long lEnd = 0;
   long lCounter = 0;
  
   HB_TRACE(HB_TR_DEBUG, ("hb_strToken(%s, %ld, %ld, %d, %p)", szText, lText, lIndex, (int) cDelimiter, plLen));
  
   do
   {
      lStart = lEnd;
  
      if( cDelimiter != ' ' )
      {
         if( szText[ lStart ] == cDelimiter )
            lStart++;
      }
      else
      {
         while( lStart < lText && szText[ lStart ] == cDelimiter )
            lStart++;
      }
  
      if( lStart < lText && szText[ lStart ] != cDelimiter )
      {
         lEnd = lStart + 1;
  
         while( lEnd < lText && szText[lEnd] != cDelimiter )
            lEnd++;
      }
      else
         lEnd = lStart;

   }
   while( lCounter++ < lIndex - 1 && lEnd < lText );
  
   if( lCounter < lIndex )
   {
      *plLen = 0;
      return "";
   }
   else
   {
      *plLen = lEnd - lStart;
      return szText + lStart;
   }
}

/* returns the nth occurence of a substring within a token-delimited string */
HB_FUNC( __STRTOKEN )
{
   char * pszText;
   long lLen;
  
   pszText = hb_strToken( hb_parc( 1 ), hb_parclen( 1 ), 
                          hb_parnl( 2 ), 
                          ISCHAR( 3 ) ? *hb_parc( 3 ) : ' ', 
                          &lLen );
  
   hb_retclen( pszText, lLen );
}

