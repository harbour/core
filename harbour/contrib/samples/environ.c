/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Sample file functions
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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

/* FilePath( <cFile> ) --> cFilePath
   Extract the full path name from a complete file name
   * FilePath( "c:\harbour\bin\harbour.exe" ) --> "c:\harbour\bin\"
*/
HB_FUNC( FILEPATH )
{
   if( ISCHAR( 1 ) )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_parc( 1 ) );
      hb_retc( pFileName->szPath );
      hb_xfree( pFileName );
   }
   else
      hb_retc( "" );
}

/* FileBase( <cFile> ) --> cFileBase
*/
HB_FUNC( FILEBASE )
{
   if( ISCHAR( 1 ) )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_parc( 1 ) );
      hb_retc( pFileName->szName );
      hb_xfree( pFileName );
   }
   else
      hb_retc( "" );
}

/* FileExt( <cFile> ) --> cFileExt
*/
HB_FUNC( FILEEXT )
{
   if( ISCHAR( 1 ) )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_parc( 1 ) );
      hb_retc( ( pFileName->szExtension ) + 1 ); /* Skip the dot */
      hb_xfree( pFileName );
   }
   else
      hb_retc( "" );
}

/* FileDrive( <cFile> ) --> cFileDrive
*/
HB_FUNC( FILEDRIVE )
{
   if( ISCHAR( 1 ) )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_parc( 1 ) );
      hb_retclen( pFileName->szDrive, 1 ); /* Only the drive letter */
      hb_xfree( pFileName );
   }
   else
      hb_retc( "" );
}
