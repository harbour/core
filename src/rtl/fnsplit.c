/*
 * Harbour Project source code:
 * hb_FNameSplit(), hb_FNameMerge() functions
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

HB_FUNC( HB_FNAMESPLIT )
{
   PHB_FNAME pFileName = hb_fsFNameSplit( hb_parcx( 1 ) );

   hb_storc( pFileName->szPath, 2 );
   hb_storc( pFileName->szName, 3 );
   hb_storc( pFileName->szExtension, 4 );
   hb_storc( pFileName->szDrive, 5 );

   hb_xfree( pFileName );
}

HB_FUNC( HB_FNAMEMERGE )
{
   HB_FNAME pFileName;
   char szFileName[ HB_PATH_MAX ];

   pFileName.szPath = hb_parc( 1 );
   pFileName.szName = hb_parc( 2 );
   pFileName.szExtension = hb_parc( 3 );
   pFileName.szDrive = hb_parc( 4 );

   hb_retc( hb_fsFNameMerge( szFileName, &pFileName ) );
}
