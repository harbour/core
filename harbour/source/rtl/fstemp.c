/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_FTEMPCREATE() function
 *
 * Copyright 2000-2001 Jose Lalin <dezac@corevia.com>
 *                     Viktor Szakats <viktor.szakats@syenar.hu>
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
#include "hbapifs.h"

#include <errno.h>

/* NOTE: The buffer must be at least _POSIX_PATH_MAX chars long */

static BOOL hb_fsTempName( BYTE * pszBuffer, const BYTE * pszDir, const BYTE * pszPrefix )
{
   /* TODO: Implement these: */
   HB_SYMBOL_UNUSED( pszDir );
   HB_SYMBOL_UNUSED( pszPrefix );

   /* TOFIX: The spec says to reserve L_tmpnam number of characters for the
             passed buffer. It will be needed to fix _POSIX_PATH_MAX to be
             at least this large. */

   pszBuffer[ 0 ] = '\0';

   tmpnam( ( char * ) pszBuffer );

   return pszBuffer[ 0 ] != '\0';
}

/* NOTE: The buffer must be at least _POSIX_PATH_MAX chars long */

FHANDLE hb_fsCreateTemp( const BYTE * pszDir, const BYTE * pszPrefix, USHORT uiAttr, BYTE * pszName )
{
   USHORT nAttemptLeft = 999;

   errno = 0;

   while( --nAttemptLeft )
   {
      if( hb_fsTempName( pszName, pszDir, pszPrefix ) )
      {
          FHANDLE fhnd = hb_fsCreateEx( pszName, uiAttr, FO_EXCLUSIVE );

          /* This function may fail, if the generated filename got 
             used between generation and the file creation. */

          if( fhnd != FS_ERROR )
          {
             return fhnd;
          }
      }
      else
      {
         /* Don't attempt to retry if the filename generator is 
            failing for some reason. */
         break;
      }
   }

   hb_fsSetError( FS_ERROR );
   return FS_ERROR;
}

#ifdef HB_EXTENSION

HB_FUNC( HB_FTEMPCREATE )
{
   BYTE szName[ _POSIX_PATH_MAX + 1 ];

   hb_retni( hb_fsCreateTemp( ( BYTE * ) hb_parc( 1 ),
                              ( BYTE * ) hb_parc( 2 ),
                              ISNUM( 3 ) ? hb_parni( 3 ) : FC_NORMAL,
                              szName ) );

   hb_storc( ( char *) szName, 4 );
}

#endif
