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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_strEmpty()
 *    hb_strnicmp()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapi.h"

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
      char c1 = toupper( *s1 );
      char c2 = toupper( *s2 );

      s1++;
      s2++;

      if( c1 != c2 )
         rc = ( c1 < c2 ? -1 : 1 );

      count--;
   }

   if( rc == 0 && l1 != l2 )
      rc = ( l1 < l2 ? -1 : 1 );

   return rc;
}

