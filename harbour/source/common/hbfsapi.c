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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapifs.h"

#if defined( HB_OS_WIN_32 )
   #if !defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES ( ( DWORD ) -1 )
   #endif
#elif defined( HB_OS_UNIX )
   #include <errno.h>
   #include <sys/types.h>
   #include <sys/stat.h>
#endif

/* NOTE: Not really belongs here, but until we can't find a better place 
         it will do it. [vszakats] */
extern void hb_fhnd_ForceLink( void );

/*
 * Function that adds zero or more paths to a list of pathnames to search
 */
HB_EXPORT void hb_fsAddSearchPath( const char * szPath, HB_PATHNAMES ** pSearchList )
{
   char * pPath;
   char * pDelim;
   BOOL fFree = TRUE;

   while( *pSearchList )
   {
      pSearchList = &(*pSearchList)->pNext;
   }

   pPath = hb_strdup( szPath );
   while( ( pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR ) ) != NULL )
   {
      *pDelim = '\0';
      *pSearchList = ( HB_PATHNAMES * ) hb_xgrab( sizeof( HB_PATHNAMES ) );
      (*pSearchList)->szPath = pPath;
      (*pSearchList)->fFree  = fFree;
      pSearchList = &(*pSearchList)->pNext;
      pPath = pDelim + 1;
      fFree = FALSE;
   }
   *pSearchList = ( HB_PATHNAMES * ) hb_xgrab( sizeof( HB_PATHNAMES ) );
   (*pSearchList)->szPath = pPath;
   (*pSearchList)->pNext  = NULL;
   (*pSearchList)->fFree  = fFree;
}

/*
 * free list of pathnames to search
 */
HB_EXPORT void hb_fsFreeSearchPath( HB_PATHNAMES * pSearchList )
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
HB_EXPORT PHB_FNAME hb_fsFNameSplit( const char * pszFileName )
{
   PHB_FNAME pFileName;
   char * pszPos;
   int iSize, iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsFNameSplit(%s)", pszFileName));

   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit: Filename: |%s|\n", pszFileName));

   iPos = iSize = hb_strnlen( pszFileName, _POSIX_PATH_MAX );

   /* Grab memory, set defaults */
   pFileName = ( PHB_FNAME ) hb_xgrab( sizeof( HB_FNAME ) );

   pszPos = pFileName->szBuffer;

   pFileName->szPath = pFileName->szName = pFileName->szExtension =
   pFileName->szDrive = NULL;

   /* Find the end of the path part, and find out where the
      name+ext starts */

   while( --iPos >= 0 )
   {
      if( strchr( OS_PATH_DELIMITER_LIST, pszFileName[ iPos ] ) )
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

   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit:   szPath: |%s|\n", pFileName->szPath));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit:   szName: |%s|\n", pFileName->szName));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit:    szExt: |%s|\n", pFileName->szExtension));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameSplit:  szDrive: |%s|\n", pFileName->szDrive));

   return pFileName;
}

/* NOTE: szFileName buffer must be at least _POSIX_PATH_MAX long */

/* This function joins path, name and extension into a string with a filename */
HB_EXPORT char * hb_fsFNameMerge( char * pszFileName, PHB_FNAME pFileName )
{
   static char s_szPathSep[] = { OS_PATH_DELIMITER, 0 }; /* see NOTE below */
   char * pszName;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsFNameMerge(%p, %p)", pszFileName, pFileName));

   /* Set the result to an empty string */
   pszFileName[ 0 ] = '\0';

   /* Strip preceding path separators from the filename */
   pszName = pFileName->szName;
   if( pszName && pszName[ 0 ] != '\0' && strchr( OS_PATH_DELIMITER_LIST, pszName[ 0 ] ) != NULL )
      pszName++;

   /* Add path if specified */
   if( pFileName->szPath )
      hb_strncat( pszFileName, pFileName->szPath, _POSIX_PATH_MAX - 1 );

   /*
      NOTE: be _very_ careful about "optimizing" this next section code!
            (specifically, initialising s_szPathSep) as MSVC with /Ni
            (or anything that infers it like /Ox) will cause you trouble.
    */

   /* If we have a path, append a path separator to the path if there
      was none. */
   if( pszFileName[ 0 ] != '\0' && ( pszName || pFileName->szExtension ) )
   {
      int iLen = strlen( pszFileName ) - 1;

      if( strchr( OS_PATH_DELIMITER_LIST, pszFileName[ iLen ] ) == NULL )
      {
         /*
             char s_szPathSep[ 2 ];

             s_szPathSep[ 0 ] = OS_PATH_DELIMITER;
             s_szPathSep[ 1 ] = '\0';

          */
         hb_strncat( pszFileName, s_szPathSep, _POSIX_PATH_MAX - 1 );
      }
   }

   /* Add filename (without extension) if specified */
   if( pszName )
      hb_strncat( pszFileName, pszName, _POSIX_PATH_MAX - 1 );

   /* Add extension if specified */
   if( pFileName->szExtension )
   {
      /* Add a dot if the extension doesn't have it */
      if( pFileName->szExtension[ 0 ] != '\0' &&
          pFileName->szExtension[ 0 ] != '.' )
         hb_strncat( pszFileName, ".", _POSIX_PATH_MAX - 1 );

      hb_strncat( pszFileName, pFileName->szExtension, _POSIX_PATH_MAX - 1 );
   }

   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge:   szPath: |%s|\n", pFileName->szPath));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge:   szName: |%s|\n", pFileName->szName));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge:    szExt: |%s|\n", pFileName->szExtension));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge:  szDrive: |%s|\n", pFileName->szDrive));
   HB_TRACE(HB_TR_INFO, ("hb_fsFNameMerge: Filename: |%s|\n", pszFileName));

   return pszFileName;
}

HB_EXPORT BOOL hb_fsFileExists( const char * pszFileName )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsFileExists(%p)", pszFileName));

   if( pszFileName == NULL )
      return FALSE;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      struct SREGS sregs;

      regs.HB_XREGS.ax = 0x4300;
      regs.HB_XREGS.dx = FP_OFF( pszFileName );
      sregs.ds = FP_SEG( pszFileName );

      HB_DOS_INT86X( 0x21, &regs, &regs, &sregs );

      return regs.x.cflag == 0;
   }
#elif defined( HB_OS_WIN_32 )
   {
      DWORD   dwAttr;

      dwAttr = GetFileAttributesA( pszFileName );
      return ( dwAttr != INVALID_FILE_ATTRIBUTES ) && 
             ( dwAttr & ( FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_DEVICE ) ) == 0;
   }
#elif defined( HB_OS_UNIX )
   {
      struct stat statbuf;

      return stat( pszFileName, &statbuf ) == 0 && 
             ( statbuf.st_mode & S_IFMT ) == S_IFREG;
   }
#else
   {
      return FALSE;
   }
#endif
}
