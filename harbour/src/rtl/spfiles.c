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

HB_BOOL hb_spFile( const char * pszFilename, char * pszRetPath )
{
   char * pszPath;
   HB_BOOL bIsFile = HB_FALSE;
   PHB_FNAME pFilepath;

   HB_TRACE( HB_TR_DEBUG, ( "hb_spFile(%s, %p)", pszFilename, pszRetPath ) );

   if( pszRetPath )
      pszPath = pszRetPath;
   else
      pszPath = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFilepath = hb_fsFNameSplit( pszFilename );

   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( pszPath, pFilepath );
      bIsFile = hb_fsFile( pszPath );
   }
   else
   {
      const char * szDefault = hb_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         hb_fsFNameMerge( pszPath, pFilepath );
         bIsFile = hb_fsFile( pszPath );
      }

      if( ! bIsFile && hb_setGetPath() )
      {
         HB_PATHNAMES * pNextPath = hb_setGetFirstSetPath();

         while( bIsFile == HB_FALSE && pNextPath )
         {
            pFilepath->szPath = pNextPath->szPath;
            hb_fsFNameMerge( pszPath, pFilepath );
            bIsFile = hb_fsFile( pszPath );
            pNextPath = pNextPath->pNext;
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
         hb_fsFNameMerge( pszPath, pFilepath );
      }
   }

   hb_xfree( pFilepath );

   if( pszRetPath == NULL )
      hb_xfree( pszPath );

   return bIsFile;
}

HB_BOOL hb_spFileExists( const char * pszFilename, char * pszRetPath )
{
   char * pszPath;
   HB_BOOL bIsFile = HB_FALSE;
   PHB_FNAME pFilepath;

   HB_TRACE( HB_TR_DEBUG, ( "hb_spFile(%s, %p)", pszFilename, pszRetPath ) );

   if( pszRetPath )
      pszPath = pszRetPath;
   else
      pszPath = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFilepath = hb_fsFNameSplit( pszFilename );

   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( pszPath, pFilepath );
      bIsFile = hb_fsFileExists( pszPath );
   }
   else
   {
      const char * szDefault = hb_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         hb_fsFNameMerge( pszPath, pFilepath );
         bIsFile = hb_fsFileExists( pszPath );
      }

      if( ! bIsFile && hb_setGetPath() )
      {
         HB_PATHNAMES * pNextPath = hb_setGetFirstSetPath();

         while( bIsFile == HB_FALSE && pNextPath )
         {
            pFilepath->szPath = pNextPath->szPath;
            hb_fsFNameMerge( pszPath, pFilepath );
            bIsFile = hb_fsFileExists( pszPath );
            pNextPath = pNextPath->pNext;
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
         hb_fsFNameMerge( pszPath, pFilepath );
      }
   }

   hb_xfree( pFilepath );

   if( pszRetPath == NULL )
      hb_xfree( pszPath );

   return bIsFile;
}

HB_FHANDLE hb_spOpen( const char * pszFilename, HB_USHORT uiFlags )
{
   char szPath[ HB_PATH_MAX ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_spOpen(%p, %hu)", pszFilename, uiFlags ) );

   if( hb_spFile( pszFilename, szPath ) )
      return hb_fsOpen( szPath, uiFlags );
   else
      return hb_fsOpen( pszFilename, uiFlags );
}

HB_FHANDLE hb_spCreate( const char * pszFilename, HB_FATTR ulAttr )
{
   char szPath[ HB_PATH_MAX ];
   PHB_FNAME pFilepath;

   HB_TRACE( HB_TR_DEBUG, ( "hb_spCreate(%p, %u)", pszFilename, ulAttr ) );

   pFilepath = hb_fsFNameSplit( pszFilename );
   if( ! pFilepath->szPath )
      pFilepath->szPath = hb_setGetDefault();

   hb_fsFNameMerge( szPath, pFilepath );
   hb_xfree( pFilepath );

   return hb_fsCreate( szPath, ulAttr );
}

HB_FHANDLE hb_spCreateEx( const char * pszFilename, HB_FATTR ulAttr, HB_USHORT uiFlags )
{
   char szPath[ HB_PATH_MAX ];
   PHB_FNAME pFilepath;

   HB_TRACE( HB_TR_DEBUG, ( "hb_spCreateEx(%p, %u, %hu)", pszFilename, ulAttr, uiFlags ) );

   pFilepath = hb_fsFNameSplit( pszFilename );
   if( ! pFilepath->szPath )
      pFilepath->szPath = hb_setGetDefault();

   hb_fsFNameMerge( szPath, pFilepath );
   hb_xfree( pFilepath );

   return hb_fsCreateEx( szPath, ulAttr, uiFlags );
}
