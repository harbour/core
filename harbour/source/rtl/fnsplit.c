/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_FNAMESPLIT(), HB_FNAMEMERGE() functions
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

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( HB_FNAMESPLIT )
{
   if( ISCHAR( 1 ) )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      hb_storc( pFileName->szPath, 2 );
      hb_storc( pFileName->szName, 3 );
      hb_storc( pFileName->szExtension, 4 );
      hb_storc( pFileName->szDrive, 5 );

      hb_xfree( pFileName );
   }
}

HB_FUNC( HB_FNAMEMERGE )
{
   HB_FNAME pFileName;
   char szFileName[ _POSIX_PATH_MAX ];

   pFileName.szPath = ISCHAR( 1 ) ? hb_parc( 1 ) : NULL;
   pFileName.szName = ISCHAR( 2 ) ? hb_parc( 2 ) : NULL;
   pFileName.szExtension = ISCHAR( 3 ) ? hb_parc( 3 ) : NULL;
   pFileName.szDrive = ISCHAR( 4 ) ? hb_parc( 4 ) : NULL;

   hb_retc( hb_fsFNameMerge( szFileName, &pFileName ) );
}

