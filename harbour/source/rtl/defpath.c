/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DEFPATH(), __DEFPATH() functions
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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
#include "hbset.h"

HARBOUR HB_DEFPATH( void )
{
   char buffer[ _POSIX_PATH_MAX ];
   char delimiter[ 2 ] = ":";
   int size;

   if( hb_set.HB_SET_DEFAULT )
   {
      /* Leave enough space to append a path delimiter */
      strncpy( buffer, hb_set.HB_SET_DEFAULT, sizeof( buffer ) - 1 );
      size = sizeof( buffer ) - 2;
   }
   buffer[ size ] = '\0';
   size = strlen( buffer );

   HB_TRACE(HB_TR_INFO, ("HB_DEFPATH: buffer is |%s|, size is %d, last char is |%c|", buffer, size, buffer[ size - 1]));
   HB_TRACE(HB_TR_INFO, ("HB_DEFPATH: OS_PATH_DELIMITER is |%c| and OS_PATH_LIST_SEPARATOR is |%c|", OS_PATH_DELIMITER, OS_PATH_LIST_SEPARATOR));

   /* If the path is not empty and it doesn't end with a drive or path
      delimiter, then add the appropriate separator. Use ':' if the size
      of the path is 1 and the list separator is not ':', otherwise use
      the path delimiter. This allows the use of a drive letter delimiter
      for DOS compatible operating systems while preventing it from being
      with a Unix compatible OS. */
   if( size && buffer[ size - 1 ] != ':' && buffer[ size - 1 ] != OS_PATH_DELIMITER )
   {
      if( size > 1 || OS_PATH_LIST_SEPARATOR == ':' )
         delimiter[ 0 ] = OS_PATH_DELIMITER;
      strcat( buffer, delimiter );
   }

   hb_retc( buffer );
}

HARBOUR HB___DEFPATH( void )
{
   HB_DEFPATH();
}

