/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * A search path shim for the FileSys API (C level)
 *
 * Copyright 2001 David G. Holm <dholm@jsd-llc.com>
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

HB_BOOL hb_spFile( const char * pFilename, char * pRetPath )
{
   char * Path;
   HB_BOOL bIsFile = HB_FALSE;
   PHB_FNAME pFilepath;

   HB_TRACE(HB_TR_DEBUG, ("hb_spFile(%s, %p)", pFilename, pRetPath));

   if( pRetPath )
      Path = pRetPath;
   else
      Path = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFilepath = hb_fsFNameSplit( pFilename );

   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( Path, pFilepath );
      bIsFile = hb_fsFile( Path );
   }
   else
   {
      const char * szDefault = hb_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         hb_fsFNameMerge( Path, pFilepath );
         bIsFile = hb_fsFile( Path );
      }

      if( ! bIsFile && hb_setGetPath() )
      {
         HB_PATHNAMES * NextPath = hb_setGetFirstSetPath();

         while( bIsFile == HB_FALSE && NextPath )
         {
            pFilepath->szPath = NextPath->szPath;
            hb_fsFNameMerge( Path, pFilepath );
            bIsFile = hb_fsFile( Path );
            NextPath = NextPath->pNext;
         }
      }

      /*
       * This code is intentional. To eliminate race condition,
       * in pending hb_spCreate()/hb_spOpen() call when we have to know
       * real path and file name we have to set its deterministic value
       * here. If it's not necessary the caller may drop this value.
       */
      if( ! bIsFile )
      {
         pFilepath->szPath = szDefault ? szDefault : ".";
         hb_fsFNameMerge( Path, pFilepath );
      }
   }

   hb_xfree( pFilepath );

   if( pRetPath == NULL )
      hb_xfree( Path );

   return bIsFile;
}

HB_BOOL hb_spFileExists( const char * pFilename, char * pRetPath )
{
   char * Path;
   HB_BOOL bIsFile = HB_FALSE;
   PHB_FNAME pFilepath;

   HB_TRACE(HB_TR_DEBUG, ("hb_spFile(%s, %p)", pFilename, pRetPath));

   if( pRetPath )
      Path = pRetPath;
   else
      Path = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFilepath = hb_fsFNameSplit( pFilename );

   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( Path, pFilepath );
      bIsFile = hb_fsFileExists( Path );
   }
   else
   {
      const char * szDefault = hb_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         hb_fsFNameMerge( Path, pFilepath );
         bIsFile = hb_fsFileExists( Path );
      }

      if( ! bIsFile && hb_setGetPath() )
      {
         HB_PATHNAMES * NextPath = hb_setGetFirstSetPath();

         while( bIsFile == HB_FALSE && NextPath )
         {
            pFilepath->szPath = NextPath->szPath;
            hb_fsFNameMerge( Path, pFilepath );
            bIsFile = hb_fsFileExists( Path );
            NextPath = NextPath->pNext;
         }
      }

      /*
       * This code is intentional. To eliminate race condition,
       * in pending hb_spCreate()/hb_spOpen() call when we have to know
       * real path and file name we have to set its deterministic value
       * here. If it's not necessary the caller may drop this value.
       */
      if( ! bIsFile )
      {
         pFilepath->szPath = szDefault ? szDefault : ".";
         hb_fsFNameMerge( Path, pFilepath );
      }
   }

   hb_xfree( pFilepath );

   if( pRetPath == NULL )
      hb_xfree( Path );

   return bIsFile;
}

HB_FHANDLE hb_spOpen( const char * pFilename, HB_USHORT uiFlags )
{
   char path[ HB_PATH_MAX ];

   HB_TRACE(HB_TR_DEBUG, ("hb_spOpen(%p, %hu)", pFilename, uiFlags));

   if( hb_spFile( pFilename, path ) )
      return hb_fsOpen( path, uiFlags );
   else
      return hb_fsOpen( pFilename, uiFlags );
}

HB_FHANDLE hb_spCreate( const char * pFilename, HB_FATTR ulAttr )
{
   char path[ HB_PATH_MAX ];
   PHB_FNAME pFilepath;

   HB_TRACE(HB_TR_DEBUG, ("hb_spCreate(%p, %u)", pFilename, ulAttr));

   pFilepath = hb_fsFNameSplit( pFilename );
   if( ! pFilepath->szPath )
      pFilepath->szPath = hb_setGetDefault();

   hb_fsFNameMerge( path, pFilepath );
   hb_xfree( pFilepath );

   return hb_fsCreate( path, ulAttr );
}

HB_FHANDLE hb_spCreateEx( const char * pFilename, HB_FATTR ulAttr, HB_USHORT uiFlags )
{
   char path[ HB_PATH_MAX ];
   PHB_FNAME pFilepath;

   HB_TRACE(HB_TR_DEBUG, ("hb_spCreateEx(%p, %u, %hu)", pFilename, ulAttr, uiFlags));

   pFilepath = hb_fsFNameSplit( pFilename );
   if( ! pFilepath->szPath )
      pFilepath->szPath = hb_setGetDefault();

   hb_fsFNameMerge( path, pFilepath );
   hb_xfree( pFilepath );

   return hb_fsCreateEx( path, ulAttr, uiFlags );
}
