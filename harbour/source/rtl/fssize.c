/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_FSIZE() function
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

#include "hbapi.h"
#include "hbapifs.h"

#include <errno.h>
#if defined( OS_UNIX_COMPATIBLE )
   #include <sys/types.h>
   #include <sys/stat.h>
#else
   #include <sys\types.h>
   #include <sys\stat.h>
#endif

ULONG hb_fsFSize( BYTE * pszFileName, BOOL bUseDirEntry )
{
   if( bUseDirEntry )
   {
      struct stat statbuf;

      if( stat( ( char * ) pszFileName, &statbuf ) == 0 )
      {
         errno = 0;

         hb_fsSetError( 0 );
         return ( ULONG ) statbuf.st_size;
      }
   }
   else
   {
      FHANDLE hFileHandle = hb_fsOpen( pszFileName, 0 );
      
      if( hFileHandle != FS_ERROR )
      {
         ULONG ulPos;
      
         ulPos = hb_fsSeek( hFileHandle, 0, SEEK_END );
         hb_fsClose( hFileHandle );
      
         errno = 0;
      
         hb_fsSetError( 0 );
         return ulPos;
      }
   }
      
   hb_fsSetError( FS_ERROR );
   return 0;
}

#ifdef HB_EXTENSION

HB_FUNC( HB_FSIZE )
{
   hb_retnl( ISCHAR( 1 ) ? hb_fsFSize( ( BYTE * ) hb_parc( 1 ), 
                                       ISLOG( 2 ) ? hb_parl( 2 ) : TRUE ) : 0 );
}

#endif
