/*
 * Harbour Project source code:
 * Harbour common FileSys API (accessed from standalone utilities and the RTL)
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
#include "hb_io.h"
#include "hbset.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
   #include "hbwinuni.h"
   #if ! defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES  ( ( DWORD ) -1 )
   #endif
   #if ! defined( FILE_ATTRIBUTE_DEVICE )
      #define FILE_ATTRIBUTE_DEVICE    0x00000040
   #endif
#elif defined( HB_OS_OS2 )
   #define INCL_DOSFILEMGR
   #define INCL_DOSERRORS
   #include <os2.h>
   #include <stdio.h>
#elif defined( HB_OS_UNIX )
   #include <sys/types.h>
   #include <sys/stat.h>
#endif
#if ! defined( HB_OS_WIN )
   #include <errno.h>
#endif

/*
 * Function that adds zero or more paths to a list of pathnames to search
 */
void hb_fsAddSearchPath( const char * szPath, HB_PATHNAMES ** pSearchList )
{
   char * pPath;
   char * pDelim;
   HB_BOOL fFree = HB_TRUE;

   while( *pSearchList )
   {
      pSearchList = &( *pSearchList )->pNext;
   }

   pPath = hb_strdup( szPath );
   while( ( pDelim = strchr( pPath, HB_OS_PATH_LIST_SEP_CHR ) ) != NULL )
   {
      *pDelim = '\0';
      *pSearchList = ( HB_PATHNAMES * ) hb_xgrab( sizeof( HB_PATHNAMES ) );
      (*pSearchList)->szPath = pPath;
      (*pSearchList)->fFree  = fFree;
      pSearchList = &(*pSearchList)->pNext;
      pPath = pDelim + 1;
      fFree = HB_FALSE;
   }
   *pSearchList = ( HB_PATHNAMES * ) hb_xgrab( sizeof( HB_PATHNAMES ) );
   ( *pSearchList )->szPath = pPath;
   ( *pSearchList )->pNext  = NULL;
   ( *pSearchList )->fFree  = fFree;
}

/*
 * free list of pathnames to search
 */
void hb_fsFreeSearchPath( HB_PATHNAMES * pSearchList )
{
   HB_PATHNAMES * pNext;

   /* Only the first path holds an allocated string.
      All of the other paths in the list are part of
      that first string. */

   while( pSearchList )
   {
      if( pSearchList->fFree )
         hb_xfree( pSearchList->szPath );
      pNext = pSearchList->pNext;
      hb_xfree( pSearchList );
      pSearchList = pNext;
   }
}

/* Split given filename into path, name and extension, plus determine drive */
PHB_FNAME hb_fsFNameSplit( const char * pszFileName )
{
   PHB_FNAME pFileName;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsFNameSplit(%s)", pszFileName ) );

   HB_TRACE( HB_TR_INFO, ( "hb_fsFNameSplit: Filename: |%s|", pszFileName ) );

   /* Grab memory, set defaults */
   pFileName = ( PHB_FNAME ) hb_xgrab( sizeof( HB_FNAME ) );

   pFileName->szPath =
   pFileName->szName =
   pFileName->szExtension =
   pFileName->szDrive = NULL;

   if( pszFileName )
   {
      char * pszPos, cDirSep;
      HB_ISIZ iSize, iPos;

      iPos = iSize = hb_strnlen( pszFileName, HB_PATH_MAX - 1 );
      cDirSep = ( char ) hb_setGetDirSeparator();

      pszPos = pFileName->szBuffer;

      /* Find the end of the path part, and find out where the
         name+ext starts */

      while( --iPos >= 0 )
      {
         if( pszFileName[ iPos ] == cDirSep ||
             strchr( HB_OS_PATH_DELIM_CHR_LIST, pszFileName[ iPos ] ) )
         {
            pFileName->szPath = pszPos;
            hb_strncpy( pszPos, pszFileName, iPos + 1 );
            pszPos += iPos + 2;
            pszFileName += iPos + 1;
            iSize -= iPos + 1;
            break;
         }
      }

      /* From this point pszFileName will point to the name+ext part of the path */
      /* Split the filename part to name and extension */
      iPos = iSize;
      while( --iPos > 0 )
      {
         if( pszFileName[ iPos ] == '.' )
         {
            pFileName->szExtension = pszPos;
            hb_strncpy( pszPos, pszFileName + iPos, iSize - iPos );
            pszPos += iSize - iPos + 1;
            iSize = iPos;
            break;
         }
      }
      if( iSize )
      {
         pFileName->szName = pszPos;
         hb_strncpy( pszPos, pszFileName, iSize );
         pszPos += iSize + 1;
      }

      /* Duplicate the drive letter from the path for easy access on
         platforms where applicable. Note that the drive info is always
         present also in the path itself. */

      if( pFileName->szPath )
      {
         iPos = 0;
         while( iPos < HB_MAX_DRIVE_LENGTH && pFileName->szPath[ iPos ] != '\0' )
         {
            if( pFileName->szPath[ iPos ] == ':' )
            {
               pFileName->szDrive = pszPos;
               hb_strncpy( pszPos, pFileName->szPath, iPos );
               break;
            }
            ++iPos;
         }
      }
   }

   HB_TRACE( HB_TR_INFO, ( "hb_fsFNameSplit:   szPath: |%s|", pFileName->szPath ) );
   HB_TRACE( HB_TR_INFO, ( "hb_fsFNameSplit:   szName: |%s|", pFileName->szName ) );
   HB_TRACE( HB_TR_INFO, ( "hb_fsFNameSplit:    szExt: |%s|", pFileName->szExtension ) );
   HB_TRACE( HB_TR_INFO, ( "hb_fsFNameSplit:  szDrive: |%s|", pFileName->szDrive ) );

   return pFileName;
}

/* NOTE: szFileName buffer must be at least HB_PATH_MAX long.
 *       Because some freign code may not be updated yet then
 *       hb_fsFNameMerge() efectively uses only HB_PATH_MAX buffer
 *       but it will be changed in the future.
 */

/* This function joins path, name and extension into a string with a filename */
char * hb_fsFNameMerge( char * pszFileName, PHB_FNAME pFileName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsFNameMerge(%p, %p)", pszFileName, pFileName ) );

   if( pszFileName && pFileName )
   {
      const char * pszName;
      char cDirSep;

      /* dir separator set by user */
      cDirSep = ( char ) hb_setGetDirSeparator();

      /* Set the result to an empty string */
      pszFileName[ 0 ] = '\0';

      /* Strip preceding path separators from the filename */
      pszName = pFileName->szName;
      if( pszName && pszName[ 0 ] != '\0' && ( pszName[ 0 ] == cDirSep ||
          strchr( HB_OS_PATH_DELIM_CHR_LIST, pszName[ 0 ] ) != NULL ) )
         pszName++;

      /* Add path if specified */
      if( pFileName->szPath )
         hb_strncat( pszFileName, pFileName->szPath, HB_PATH_MAX - 1 - 1 );

      /* If we have a path, append a path separator to the path if there
         was none. */
      if( pszFileName[ 0 ] != '\0' && ( pszName || pFileName->szExtension ) )
      {
         int iLen = ( int ) strlen( pszFileName ) - 1;

         if( iLen < HB_PATH_MAX - 1 - 2 && pszFileName[ iLen ] != cDirSep &&
             strchr( HB_OS_PATH_DELIM_CHR_LIST, pszFileName[ iLen ] ) == NULL )
         {
            pszFileName[ iLen + 1 ] = HB_OS_PATH_DELIM_CHR;
            pszFileName[ iLen + 2 ] = '\0';
         }
      }

      /* Add filename (without extension) if specified */
      if( pszName )
         hb_strncat( pszFileName, pszName, HB_PATH_MAX - 1 - 1 );

      /* Add extension if specified */
      if( pFileName->szExtension )
      {
         /* Add a dot if the extension doesn't have it */
         if( pFileName->szExtension[ 0 ] != '\0' &&
             pFileName->szExtension[ 0 ] != '.' )
            hb_strncat( pszFileName, ".", HB_PATH_MAX - 1 - 1 );

         hb_strncat( pszFileName, pFileName->szExtension, HB_PATH_MAX - 1 - 1 );
      }

      HB_TRACE( HB_TR_INFO, ( "hb_fsFNameMerge:   szPath: |%s|", pFileName->szPath ) );
      HB_TRACE( HB_TR_INFO, ( "hb_fsFNameMerge:   szName: |%s|", pFileName->szName ) );
      HB_TRACE( HB_TR_INFO, ( "hb_fsFNameMerge:    szExt: |%s|", pFileName->szExtension ) );
      HB_TRACE( HB_TR_INFO, ( "hb_fsFNameMerge:  szDrive: |%s|", pFileName->szDrive ) );
      HB_TRACE( HB_TR_INFO, ( "hb_fsFNameMerge: Filename: |%s|", pszFileName ) );
   }

   return pszFileName;
}

HB_BOOL hb_fsNameExists( const char * pszFileName )
{
   HB_BOOL fExist = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsNameExists(%p)", pszFileName ) );

   if( pszFileName != NULL )
   {
#if defined( HB_OS_WIN )
      LPTSTR lpFree;
      LPCTSTR lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );

      fExist = ( GetFileAttributes( lpFileName ) != INVALID_FILE_ATTRIBUTES );

      if( lpFree )
         hb_xfree( lpFree );
#else
      char * pszFree = NULL;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      {
#  if defined( HB_OS_DOS )
#     if defined( __DJGPP__ ) || defined( __BORLANDC__ )
         fExist = _chmod( pszFileName, 0, 0 ) != -1;
#     else
         unsigned int iAttr = 0;
         fExist = _dos_getfileattr( pszFileName, &iAttr ) == 0;
#     endif
#  elif defined( HB_OS_OS2 )
         FILESTATUS3 fs3;
         fExist = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD,
                                    &fs3, sizeof( fs3 ) ) == NO_ERROR;
#  elif defined( HB_OS_UNIX )
         struct stat statbuf;
         fExist = stat( pszFileName, &statbuf ) == 0;
#  else
         int iTODO; /* To force warning */
#  endif
      }

      if( pszFree )
         hb_xfree( pszFree );
#endif
   }

   return fExist;
}

HB_BOOL hb_fsFileExists( const char * pszFileName )
{
   HB_BOOL fExist = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsFileExists(%p)", pszFileName ) );

   if( pszFileName != NULL )
   {
#if defined( HB_OS_WIN )
      LPTSTR lpFree;
      LPCTSTR lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );
      DWORD dwAttr;

      dwAttr = GetFileAttributes( lpFileName );
      fExist = ( dwAttr != INVALID_FILE_ATTRIBUTES ) &&
               ( dwAttr & ( FILE_ATTRIBUTE_DIRECTORY |
                            FILE_ATTRIBUTE_DEVICE ) ) == 0;

      if( lpFree )
         hb_xfree( lpFree );
#else
      char * pszFree = NULL;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      {
#  if defined( HB_OS_DOS )
#     if defined( __DJGPP__ ) || defined( __BORLANDC__ )
         int iAttr = _chmod( pszFileName, 0, 0 );
         fExist = iAttr != -1 && ( iAttr & 0x10 ) == 0;
#     else
         unsigned int iAttr = 0;
         fExist = _dos_getfileattr( pszFileName, &iAttr ) == 0 &&
                  ( iAttr & 0x10 ) == 0;
#     endif
#  elif defined( HB_OS_OS2 )
         FILESTATUS3 fs3;
         fExist = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD,
                                    &fs3, sizeof( fs3 ) ) == NO_ERROR &&
                  ( fs3.attrFile & FILE_DIRECTORY ) == 0;
#  elif defined( HB_OS_UNIX )
         struct stat statbuf;

         fExist = stat( pszFileName, &statbuf ) == 0 &&
                  S_ISREG( statbuf.st_mode );
#  else
         int iTODO; /* To force warning */
#  endif
      }

      if( pszFree )
         hb_xfree( pszFree );
#endif
   }

   return fExist;
}

HB_BOOL hb_fsDirExists( const char * pszDirName )
{
   HB_BOOL fExist = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsDirExists(%p)", pszDirName ) );

   if( pszDirName != NULL )
   {
#if defined( HB_OS_WIN )
      LPTSTR lpFree;
      LPCTSTR lpDirName = HB_FSNAMECONV( pszDirName, &lpFree );
      DWORD dwAttr;

      dwAttr = GetFileAttributes( lpDirName );
      fExist = ( dwAttr != INVALID_FILE_ATTRIBUTES ) &&
               ( dwAttr & FILE_ATTRIBUTE_DIRECTORY );

      if( lpFree )
         hb_xfree( lpFree );
#else
      char * pszFree = NULL;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      {
#  if defined( HB_OS_DOS )
#     if defined( __DJGPP__ ) || defined( __BORLANDC__ )
         int iAttr = _chmod( pszDirName, 0, 0 );
         fExist = iAttr != -1 && ( iAttr & 0x10 ) != 0;
#     else
         unsigned int iAttr = 0;
         fExist = _dos_getfileattr( pszDirName, &iAttr ) == 0 &&
                  ( iAttr & 0x10 ) != 0;
#     endif
#  elif defined( HB_OS_OS2 )
         FILESTATUS3 fs3;
         fExist = DosQueryPathInfo( ( PCSZ ) pszDirName, FIL_STANDARD,
                                    &fs3, sizeof( fs3 ) ) == NO_ERROR &&
                  ( fs3.attrFile & FILE_DIRECTORY ) != 0;
#  elif defined( HB_OS_UNIX )
         struct stat statbuf;
         fExist = stat( pszDirName, &statbuf ) == 0 &&
                  S_ISDIR( statbuf.st_mode );
#  else
         int iTODO; /* To force warning */
#  endif
      }

      if( pszFree )
         hb_xfree( pszFree );
#endif
   }

   return fExist;
}

HB_BOOL hb_fsMaxFilesError( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsMaxFilesError()" ) );

#if defined( HB_OS_WIN )
   return GetLastError() == ERROR_TOO_MANY_OPEN_FILES;
#else
   return errno == EMFILE;
#endif
}
