/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * A search path shim for the FileSys API (C level)
 *
 * Copyright 2001 David G. Holm <dholm@jsd-llc.com>
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

#include "hbapifs.h"
#include "hbset.h"

static BOOL FindFile( BYTE * pFilename, BYTE * path )
{
   BOOL bIsFile = FALSE;
   PHB_FNAME pFilepath;

   HB_TRACE(HB_TR_DEBUG, ("FindFile(%s, %p)", (char*) pFilename, path));

   pFilepath = hb_fsFNameSplit( (char*) pFilename );
   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( (char*) path, pFilepath );
      bIsFile = hb_fsFile( path );
   }
   else
   {
      if( hb_set.HB_SET_DEFAULT )
      {
         pFilepath->szPath = hb_set.HB_SET_DEFAULT;
         hb_fsFNameMerge( (char*) path, pFilepath );
         bIsFile = hb_fsFile( path );
      }

      if( !bIsFile && hb_set.HB_SET_PATH )
      {
         HB_PATHNAMES * nextPath = hb_setGetFirstSetPath();
         while( !bIsFile && nextPath )
         {
            pFilepath->szPath = nextPath->szPath;
            hb_fsFNameMerge( (char*) path, pFilepath );
            bIsFile = hb_fsFile( path );
            nextPath = nextPath->pNext;
         }
      }
   }
   hb_xfree( pFilepath );

   if( !bIsFile )
      *path = '\0';

   return bIsFile;
}

BOOL hb_spFile( BYTE * pFilename )
{
   BYTE path[ _POSIX_PATH_MAX + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_spFile(%s, %p)", (char*) pFilename, path));

   return FindFile( pFilename, path );
}

FHANDLE hb_spOpen( BYTE * pFilename, USHORT uiFlags )
{
   BYTE path[ _POSIX_PATH_MAX + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_spOpen(%p, %hu)", pFilename, uiFlags));

   if( FindFile( pFilename, path ) )
      return hb_fsOpen( path, uiFlags );
   else
      return hb_fsOpen( pFilename, uiFlags );
}

FHANDLE hb_spCreate( BYTE * pFilename, USHORT uiAttr )
{
   BYTE path[ _POSIX_PATH_MAX + 1 ];
   PHB_FNAME pFilepath = hb_fsFNameSplit( (char*) pFilename );

   HB_TRACE(HB_TR_DEBUG, ("hb_spCreate(%p, %hu)", pFilename, uiAttr));

   if( ! pFilepath->szPath && hb_set.HB_SET_DEFAULT )
      pFilepath->szPath = hb_set.HB_SET_DEFAULT;

   hb_fsFNameMerge( (char*) path, pFilepath );
   hb_xfree( pFilepath );

   return hb_fsCreate( path, uiAttr );
}

FHANDLE hb_spCreateEx( BYTE * pFilename, USHORT uiAttr, USHORT uiFlags )
{
   BYTE path[ _POSIX_PATH_MAX + 1 ];
   PHB_FNAME pFilepath = hb_fsFNameSplit( (char*) pFilename );

   HB_TRACE(HB_TR_DEBUG, ("hb_spCreateEx(%p, %hu, %hu)", pFilename, uiAttr, uiFlags));

   if( ! pFilepath->szPath && hb_set.HB_SET_DEFAULT )
      pFilepath->szPath = hb_set.HB_SET_DEFAULT;

   hb_fsFNameMerge( (char*) path, pFilepath );
   hb_xfree( pFilepath );

   return hb_fsCreateEx( path, uiAttr, uiFlags );
}
