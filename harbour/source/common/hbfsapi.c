/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour common FileSys API (accessed from standalone utilities and the RTL)
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* Split given filename into path, name and extension, plus determine drive */
PHB_FNAME hb_fsFNameSplit( char * pszFileName )
{
   PHB_FNAME pFileName;
   char * pszPos;
   char * pszAt;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsFNameSplit(%s)", pszFileName));

   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit: Filename: |%s|\n", pszFileName));

   /* Grab memory, set defaults */

   pFileName = ( PHB_FNAME ) hb_xgrab( sizeof( HB_FNAME ) );

   pszPos = pFileName->szBuffer;

   /* Find the end of the path part, and find out where the
      name+ext starts */

   pszAt = NULL;
   if( pszFileName[ 0 ] != '\0' )
   {
      int iPos = strlen( pszFileName );

      while( --iPos >= 0 )
      {
         if( strchr( OS_PATH_DELIMITER_LIST, pszFileName[ iPos ] ) )
         {
            pszAt = pszFileName + iPos;
            break;
         }
      }
   }

   if( pszAt )
   {
      pFileName->szPath = pszPos;
      strncpy( pszPos, pszFileName, pszAt - pszFileName + 1 );
      pszPos += pszAt - pszFileName + 1;
      *pszPos++ = '\0';
      pszFileName = pszAt + 1;
   }
   else
      pFileName->szPath = NULL;

   /* From this point pszFileName will point to the name+ext part of the path */

   /* Split the filename part to name and extension */

   pszAt = strrchr( pszFileName, '.' );
   if( pszAt && pszAt != pszFileName )
   {
      pFileName->szName = pszPos;
      strncpy( pszPos, pszFileName, pszAt - pszFileName );
      pszPos += pszAt - pszFileName;
      *pszPos++ = '\0';

      pFileName->szExtension = pszPos;
      strcpy( pszPos, pszAt );
      pszPos += strlen( pszAt ) + 1;
   }
   else
   {
      if( pszFileName[ 0 ] != '\0' )
      {
         pFileName->szName = pszPos;
         strcpy( pszPos, pszFileName );
         pszPos += strlen( pszFileName ) + 1;
      }
      else
         pFileName->szName = NULL;

      pFileName->szExtension = NULL;
   }

   /* Duplicate the drive letter from the path for easy access on
      platforms where applicable. Note that the drive info is always
      present also in the path itself. */

   if( pFileName->szPath && ( pszAt = strchr( pFileName->szPath, ':' ) ) != NULL )
   {
      pFileName->szDrive = pszPos;
      strncpy( pszPos, pFileName->szPath, pszAt - pFileName->szPath + 1 );
      pszPos += pszAt - pFileName->szPath + 1;
      *pszPos = '\0';
   }
   else
      pFileName->szDrive = NULL;

   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit:   szPath: |%s|\n", pFileName->szPath));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit:   szName: |%s|\n", pFileName->szName));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit:    szExt: |%s|\n", pFileName->szExtension));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit:  szDrive: |%s|\n", pFileName->szDrive));

   return pFileName;
}

/* NOTE: szFileName buffer must be at least _POSIX_PATH_MAX long */

/* This function joins path, name and extension into a string with a filename */
char * hb_fsFNameMerge( char * pszFileName, PHB_FNAME pFileName )
{
   char * pszName;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsFNameMerge(%p, %p)", pszFileName, pFileName));

   /* Set the result to an empty string */
   pszFileName[ 0 ] = '\0';

   /* Strip preceding path separators from the filename */
   pszName = pFileName->szName;
   if( pszName && pszName[ 0 ] != '\0' && strchr( OS_PATH_DELIMITER_LIST, pszName[ 0 ] ) )
      pszName++;

   /* Add path if specified */
   if( pFileName->szPath )
      strcat( pszFileName, pFileName->szPath );

   /* If we have a path, append a path separator to the path if there
      was none. */
   if( pszFileName[ 0 ] != '\0' && ( pszName || pFileName->szExtension ) )
   {
      int iLen = strlen( pszFileName ) - 1;

      if( strchr( OS_PATH_DELIMITER_LIST, pszFileName[ iLen ] ) == NULL )
      {
         char szPathSep[ 2 ];

         szPathSep[ 0 ] = OS_PATH_DELIMITER;
         szPathSep[ 1 ] = '\0';

         strcat( pszFileName, szPathSep );
      }
   }

   /* Add filename (without extension) if specified */
   if( pszName )
      strcat( pszFileName, pszName );

   /* Add extension if specified */
   if( pFileName->szExtension )
   {
      /* Add a dot if the extension doesn't have it */
      if( pFileName->szExtension[ 0 ] != '\0' &&
          pFileName->szExtension[ 0 ] != '.' )
         strcat( pszFileName, "." );

      strcat( pszFileName, pFileName->szExtension );
   }

   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge:   szPath: |%s|\n", pFileName->szPath));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge:   szName: |%s|\n", pFileName->szName));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge:    szExt: |%s|\n", pFileName->szExtension));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge:  szDrive: |%s|\n", pFileName->szDrive));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge: Filename: |%s|\n", pszFileName));

   return pszFileName;
}

