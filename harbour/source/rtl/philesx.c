/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CURDRIVE() function
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

#include <ctype.h>

#include "hbapi.h"
#include "hbapifs.h"

#ifdef HB_COMPAT_XPP

/* NOTE: XBase++ compatible */

HB_FUNC( CURDRIVE )
{
   USHORT uiErrorOld = hb_fsError();
   char szDrive[ 1 ];

   szDrive[ 0 ] = ( ( char ) hb_fsCurDrv() ) + 'A';
   hb_retclen( szDrive, 1 );

   if( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 )
   {
      if( hb_fsChDrv( ( BYTE )( toupper( *hb_parc( 1 ) ) - 'A' ) ) != 0 )
      {
         /* TODO: Throw some XBase++ like runtime error. [vszakats] */
      }
   }

   hb_fsSetError( uiErrorOld );
}

#endif

