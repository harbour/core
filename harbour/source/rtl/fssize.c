/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_FSIZE() function
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
