/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour common string functions (accessed from standalone utilities and the RTL)
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_stricmp()
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include <ctype.h> /* Needed by hb_strupr() */

#include "hbapi.h"

ULONG hb_strAt( const char * szSub, ULONG ulSubLen, const char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strAt(%s, %lu, %s, %lu)", szSub, ulSubLen, szText, ulLen));

   if( ulSubLen > 0 && ulLen >= ulSubLen )
   {
      ULONG ulPos = 0;
      ULONG ulSubPos = 0;

      while( ulPos < ulLen && ulSubPos < ulSubLen )
      {
         if( szText[ ulPos ] == szSub[ ulSubPos ] )
         {
            ulSubPos++;
            ulPos++;
         }
         else if( ulSubPos )
            ulSubPos = 0;
         else
            ulPos++;
      }

      return ( ulSubPos < ulSubLen ) ? 0 : ( ulPos - ulSubLen + 1 );
   }
   else
      return 0;
}

char * hb_strupr( char * pszText )
{
   char * pszPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_strupr(%s)", pszText));

   for( pszPos = pszText; *pszPos; pszPos++ )
      *pszPos = toupper( *pszPos );

   return pszText;
}

char * hb_strdup( const char * pszText )
{
   char * pszDup;
   int iLen = strlen( pszText ) + 1;

   HB_TRACE(HB_TR_DEBUG, ("hb_strdup(%s, %i)", pszText, iLen));

   pszDup = ( char * ) hb_xgrab( iLen );
   memcpy( pszDup, pszText, iLen );

   return pszDup;
}

int hb_stricmp( const char * s1, const char * s2 )
{
   int rc = 0;
   ULONG l1;
   ULONG l2;
   ULONG count;

   HB_TRACE(HB_TR_DEBUG, ("hb_stricmp(%s, %s)", s1, s2));

   l1 = strlen( s1 );
   l2 = strlen( s2 );
   count = ( l1 < l2 ? l1 : l2 );

   while( rc == 0 && count > 0 )
   {
      char c1 = toupper( *s1++ );
      char c2 = toupper( *s2++ );

      if( c1 != c2 )
         rc = ( c1 < c2 ? -1 : 1 );

      count--;
   }

   if( rc == 0 && l1 != l2 )
      rc = ( l1 < l2 ? -1 : 1 );

   return rc;
}
