/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * String functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_strEmpty()
 *    hb_strnicmp()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

BOOL hb_strEmpty( const char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strEmpty(%s, %lu)", szText, ulLen));

   while( ulLen-- )
   {
      char c = szText[ ulLen ];

      if( !HB_ISSPACE( c ) )
         return FALSE;
   }

   return TRUE;
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

int hb_strnicmp( const char * s1, const char * s2, ULONG count )
{
   int rc = 0;
   ULONG l1;
   ULONG l2;

   HB_TRACE(HB_TR_DEBUG, ("hb_strnicmp(%s, %s, %lu)", s1, s2, count));

   l1 = strlen( s1 );
   l2 = strlen( s2 );
   if( l1 > count )
      l1 = count;
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

