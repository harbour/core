/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_FTEMPNAME(), HB_FTEMPCREATE() functions
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
 *                Victor Szakats <info@szelvesz.hu>
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

#include <errno.h>

#include "hbapi.h"
#include "hbapifs.h"

/* NOTE: The buffer must be at least _POSIX_PATH_MAX chars long */

void hb_fsTempName( BYTE * pszBuffer, const BYTE * pszDir, const BYTE * pszPrefix )
{
   /* TODO: Implement these: */
   HB_SYMBOL_UNUSED( pszDir );
   HB_SYMBOL_UNUSED( pszPrefix );

   /* TOFIX: The spec says to reserve L_tmpnam number of characters for the
             passed buffer. It will be needed to fix _POSIX_PATH_MAX to be
             at least this large. */

   pszBuffer[ 0 ] = '\0';
   tmpnam( ( char * ) pszBuffer );
}

FHANDLE hb_fsCreateTemp( const BYTE * pszDir, const BYTE * pszPrefix, USHORT uiAttribute )
{
   BYTE szName[ _POSIX_PATH_MAX + 1 ];

   hb_fsTempName( szName, pszDir, pszPrefix );
  
   errno = 0;
  
   if( szName[ 0 ] )
      return hb_fsCreate( szName, uiAttribute );

   hb_fsSetError( FS_ERROR );
   return FS_ERROR;
}

HARBOUR HB_HB_FTEMPNAME()
{
   BYTE szName[ _POSIX_PATH_MAX + 1 ];

   hb_fsTempName( szName, NULL, NULL );
  
   hb_retc( ( char * ) szName );
}

HARBOUR HB_HB_FTEMPCREATE( void )
{
   hb_retni( hb_fsCreateTemp( ( BYTE * ) hb_parc( 1 ),
                              ( BYTE * ) hb_parc( 2 ),
                              ISNUM( 2 ) ? hb_parni( 2 ) : FC_NORMAL ) );
}

