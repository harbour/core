/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour File Find API (C level)
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *                Viktor Szakats <viktor.szakats@syenar.hu>
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

/* TOFIX: Set size to 0 on DOS/Win32 on directory entries */
/* TOFIX: Call tzset(). For what platforms ? */

#define INCL_DOSFILEMGR
#define INCL_DOSERRORS
#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hb_io.h"

/* ------------------------------------------------------------- */

#if defined(HB_OS_DOS)

   #if defined(__DJGPP__) || defined(__RSX32__)
      #include <sys/param.h>
      #include <sys/stat.h>
   #endif
   #include <dos.h>
   #include <dir.h>
   #include <time.h>

   typedef struct
   {
      void *          dta;
      struct ffblk    entry;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#elif defined(HB_OS_OS2)

   #include <sys/stat.h>
   #include <time.h>

   typedef struct
   {
      HDIR            hFindFile;
      FILEFINDBUF3    entry;
      ULONG           fileTypes;
      ULONG           findSize;
      ULONG           findCount;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#elif defined(HB_OS_WIN_32)
   
   typedef struct
   {
      HANDLE          hFindFile;
      WIN32_FIND_DATA pFindFileData;
      DWORD           dwAttr;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#elif defined(HB_OS_UNIX)

   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <errno.h>
   #include <dirent.h>
   #include <time.h>

   #if !defined(HAVE_POSIX_IO)
      #define HAVE_POSIX_IO
   #endif
   
   typedef struct
   {
      DIR *           dir;
      struct dirent   entry;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#else

   typedef void HB_FFIND_INFO, * PHB_FFIND_INFO;

#endif

/* ------------------------------------------------------------- */

static USHORT AttrToHarbour( USHORT uiMask )
{
   USHORT uiRetMask;

   HB_TRACE(HB_TR_DEBUG, ("AttrToHarbour(%hu)", uiMask));

#if defined(HB_OS_DOS)

   uiRetMask = 0;
   if( uiMask & FA_ARCH )   uiRetMask |= HB_FA_ARCHIVE;
   if( uiMask & FA_DIREC )  uiRetMask |= HB_FA_DIRECTORY;
   if( uiMask & FA_HIDDEN ) uiRetMask |= HB_FA_HIDDEN;
   if( uiMask & FA_RDONLY ) uiRetMask |= HB_FA_READONLY;
   if( uiMask & FA_LABEL )  uiRetMask |= HB_FA_LABEL;
   if( uiMask & FA_SYSTEM ) uiRetMask |= HB_FA_SYSTEM;

#elif defined(HB_OS_OS2)

   uiRetMask = 0;
   if( uiMask & FILE_ARCHIVED )  uiRetMask |= HB_FA_ARCHIVE;
   if( uiMask & FILE_DIRECTORY ) uiRetMask |= HB_FA_DIRECTORY;
   if( uiMask & FILE_HIDDEN )    uiRetMask |= HB_FA_HIDDEN;
   if( uiMask & FILE_READONLY )  uiRetMask |= HB_FA_READONLY;
   if( uiMask & FILE_SYSTEM )    uiRetMask |= HB_FA_SYSTEM;

#elif defined(HB_OS_WIN_32)

   uiRetMask = 0;
   if( uiMask & FILE_ATTRIBUTE_ARCHIVE )   uiRetMask |= HB_FA_ARCHIVE;
   if( uiMask & FILE_ATTRIBUTE_DIRECTORY ) uiRetMask |= HB_FA_DIRECTORY;
   if( uiMask & FILE_ATTRIBUTE_HIDDEN )    uiRetMask |= HB_FA_HIDDEN;
   if( uiMask & FILE_ATTRIBUTE_READONLY )  uiRetMask |= HB_FA_READONLY;
   if( uiMask & FILE_ATTRIBUTE_SYSTEM )    uiRetMask |= HB_FA_SYSTEM;

#elif defined(HB_OS_UNIX)

   uiRetMask = 0;
   if( S_ISREG( uiMask ) )  uiRetMask |= HB_FA_ARCHIVE;
   if( S_ISDIR( uiMask ) )  uiRetMask |= HB_FA_DIRECTORY;
   if( S_ISLNK( uiMask ) )  uiRetMask |= HB_FA_REPARSE;
   if( S_ISCHR( uiMask ) )  uiRetMask |= HB_FA_COMPRESSED;
   if( S_ISBLK( uiMask ) )  uiRetMask |= HB_FA_DEVICE;
   if( S_ISFIFO( uiMask ) ) uiRetMask |= HB_FA_TEMPORARY;
   if( S_ISSOCK( uiMask ) ) uiRetMask |= HB_FA_SPARSE;

#else

   HB_SYMBOL_UNUSED( uiMask );
   uiRetMask = 0;

#endif

   return uiRetMask;
}

static USHORT AttrFromHarbour( USHORT uiMask )
{
   USHORT uiRetMask;

   HB_TRACE(HB_TR_DEBUG, ("AttrToHarbour(%hu)", uiMask));

#if defined(HB_OS_DOS)

   uiRetMask = 0;
   if( uiMask & HB_FA_ARCHIVE )   uiRetMask |= FA_ARCH;
   if( uiMask & HB_FA_DIRECTORY ) uiRetMask |= FA_DIREC;
   if( uiMask & HB_FA_HIDDEN )    uiRetMask |= FA_HIDDEN;
   if( uiMask & HB_FA_READONLY )  uiRetMask |= FA_RDONLY;
   if( uiMask & HB_FA_LABEL )     uiRetMask |= FA_LABEL;
   if( uiMask & HB_FA_SYSTEM )    uiRetMask |= FA_SYSTEM;

#elif defined(HB_OS_OS2)

   uiRetMask = 0;
   if( uiMask & HB_FA_ARCHIVE )   uiRetMask |= FILE_ARCHIVED;
   if( uiMask & HB_FA_DIRECTORY ) uiRetMask |= FILE_DIRECTORY;
   if( uiMask & HB_FA_HIDDEN )    uiRetMask |= FILE_HIDDEN;
   if( uiMask & HB_FA_READONLY )  uiRetMask |= FILE_READONLY;
   if( uiMask & HB_FA_SYSTEM )    uiRetMask |= FILE_SYSTEM;

#elif defined(HB_OS_WIN_32)

   uiRetMask = 0;
   if( uiMask & HB_FA_ARCHIVE )   uiRetMask |= FILE_ATTRIBUTE_ARCHIVE ;
   if( uiMask & HB_FA_DIRECTORY ) uiRetMask |= FILE_ATTRIBUTE_DIRECTORY;
   if( uiMask & HB_FA_HIDDEN )    uiRetMask |= FILE_ATTRIBUTE_HIDDEN;
   if( uiMask & HB_FA_READONLY )  uiRetMask |= FILE_ATTRIBUTE_READONLY;
   if( uiMask & HB_FA_SYSTEM )    uiRetMask |= FILE_ATTRIBUTE_SYSTEM;

#elif defined(HB_OS_UNIX)

   uiRetMask = 0;
   if( S_ISREG( uiMask ) )  uiRetMask |= HB_FA_ARCHIVE;
   if( S_ISDIR( uiMask ) )  uiRetMask |= HB_FA_DIRECTORY;
   if( S_ISLNK( uiMask ) )  uiRetMask |= HB_FA_REPARSE;
   if( S_ISCHR( uiMask ) )  uiRetMask |= HB_FA_COMPRESSED;
   if( S_ISBLK( uiMask ) )  uiRetMask |= HB_FA_DEVICE;
   if( S_ISFIFO( uiMask ) ) uiRetMask |= HB_FA_TEMPORARY;
   if( S_ISSOCK( uiMask ) ) uiRetMask |= HB_FA_SPARSE;

#else

   HB_SYMBOL_UNUSED( uiMask );
   uiRetMask = 0;

#endif

   return uiRetMask;
}

static void hb_fsFindFill( PHB_FFIND ffind )
{
   PHB_FFIND_INFO info = ffind->info;

   USHORT nYear;
   USHORT nMonth;
   USHORT nDay;

   USHORT nHour;
   USHORT nMin;
   USHORT nSec;

   USHORT nAttr;

   /* Set the default values in case some platforms don't 
      support some of these, or they may fail on them. */
   
   ffind->szName[ 0 ] = '\0';
   ffind->size = 0;

   /* Convert platform specific find info structure into 
      the Harbour spcific structure. */

#if defined(HB_OS_DOS)

   {
      strncpy( ffind->szName, info->entry.ff_name, _POSIX_PATH_MAX );
      ffind->size = info->entry.ff_fsize;
   
      nAttr = info->entry.ff_attrib;
   
      {
         time_t ftime;
         struct tm * ft;
         struct stat sStat;
   
         stat( info->entry.ff_name, &sStat );
         
         ftime = sStat.st_mtime;
         ft = localtime( &ftime );
   
         nYear  = ft->tm_year + 1900;
         nMonth = ft->tm_mon + 1;
         nDay   = ft->tm_mday;

         nHour  = ft->tm_hour;
         nMin   = ft->tm_min;
         nSec   = ft->tm_sec;
      }
   }

#elif defined(HB_OS_OS2)

   {
      struct stat sStat;

      stat( info->entry.achName, &sStat );

      strncpy( ffind->szName, info->entry.achName, _POSIX_PATH_MAX );
      ffind->size = sStat.st_size;

      nAttr = info->entry.attrFile;

      {
         time_t ftime;
         struct tm * ft;

         ftime = sStat.st_mtime;
         ft = localtime( &ftime );
   
         nYear  = ft->tm_year + 1900;
         nMonth = ft->tm_mon + 1;
         nDay   = ft->tm_mday;

         nHour  = ft->tm_hour;
         nMin   = ft->tm_min;
         nSec   = ft->tm_sec;
      }
   }

#elif defined(HB_OS_WIN_32)

   {
      strncpy( ffind->szName, info->pFindFileData.cFileName, _POSIX_PATH_MAX );

      /* TOFIX: nFileSizeHigh is not yet used. */
      ffind->size = info->pFindFileData.nFileSizeLow;
   
      nAttr = ( USHORT ) info->pFindFileData.dwFileAttributes;
   
      {
         FILETIME ft;
         SYSTEMTIME time;
   
         if( FileTimeToLocalFileTime( &info->pFindFileData.ftLastWriteTime, &ft ) && 
             FileTimeToSystemTime( &ft, &time ) )
         {
            nYear  = time.wYear;
            nMonth = time.wMonth;
            nDay   = time.wDay;
       
            nHour  = time.wHour;
            nMin   = time.wMinute;
            nSec   = time.wSecond;
         }
         else
         {
            nYear  =
            nMonth =
            nDay   =
            nHour  =
            nMin   =
            nSec   = 0;
         }
      }
   }

#elif defined(HB_OS_UNIX)

   {
      struct stat sStat;

      stat( info->entry->d_name, &sStat );

      strncpy( ffind->szName, info->entry->d_name, _POSIX_PATH_MAX );
      ffind->size = sStat.st_size;

      nAttr = sStat.st_mode;

      {
         time_t ftime;
         struct tm * ft;

         ftime = sStat.st_mtime;
         ft = localtime( &ftime );
   
         nYear  = ft->tm_year + 1900;
         nMonth = ft->tm_mon + 1;
         nDay   = ft->tm_mday;

         nHour  = ft->tm_hour;
         nMin   = ft->tm_min;
         nSec   = ft->tm_sec;
      }
   }

#elif defined(HB_OS_MAC)

   {
      /* TODO */
   }

#else

   {
      HB_SYMBOL_UNUSED( info );

      nAttr  =
      nYear  =
      nMonth =
      nDay   =
      nHour  =
      nMin   =
      nSec   = 0;
   }

#endif

   /* Do the conversions common for all platforms */

   ffind->szName[ _POSIX_PATH_MAX ] = '\0';
   ffind->szName[ 8 + 1 + 3 ] = '\0';

   ffind->attr = AttrToHarbour( nAttr );

   ffind->lDate = hb_dateEncode( nYear, nMonth, nDay );
   hb_dateStrPut( ffind->szDate, nYear, nMonth, nDay );
   
   sprintf( ffind->szTime, "%02d:%02d:%02d", nHour, nMin, nSec );
}

PHB_FFIND hb_fsFindFirst( char * pszFileName, USHORT uiAttr )
{
   PHB_FFIND ffind = ( PHB_FFIND ) hb_xgrab( sizeof( HB_FFIND ) );
   BOOL bFound;

   /* Make sure we have this cleared */
   
   ffind->info = NULL;

   /* Do platform dependant first search */

#if defined(HB_OS_DOS)

   {
      PHB_FFIND_INFO info = ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );

      bFound = ( findfirst( pszFileName, &info->entry, AttrFromHarbour( uiAttr ) ) == 0 );
   }

#elif defined(HB_OS_OS2)

   {
      PHB_FFIND_INFO info = ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );

      info->hFindFile = HDIR_CREATE;

      bFound = DosFindFirst( pszFileName,
                             &info->hFindFile,
                             ( LONG ) AttrFromHarbour( uiAttr ),
                             &info->entry,
                             sizeof( info->entry ),
                             &info->findCount,
                             FIL_STANDARD ) == NO_ERROR && info->findCount > 0;
   }

#elif defined(HB_OS_WIN_32)

   {
      PHB_FFIND_INFO info = ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );

      info->hFindFile = FindFirstFile( pszFileName, &info->pFindFileData );
      info->dwAttr    = AttrFromHarbour( uiAttr );

      if( info->hFindFile != INVALID_HANDLE_VALUE )
      {
         if( info->dwAttr == 0 || ( info->dwAttr & info->pFindFileData.dwFileAttributes ) )
         {
            bFound = TRUE;
         }
         else
         {
            bFound = FALSE;
  
            while( FindNextFile( info->hFindFile, &info->pFindFileData ) )
            {
               if( info->dwAttr == 0 || ( info->dwAttr & info->pFindFileData.dwFileAttributes ) )
               {
                  bFound = TRUE;
                  break;
               }
            }
         }
      }
      else
         bFound = FALSE;
   }

#elif defined(HB_OS_UNIX)

   {
      PHB_FFIND_INFO info = ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );

      info->dir = opendir( pszFileName );

      if( info->dir )
      {
         info->entry = readdir( info->dir );

         /* TOFIX: uiAttr check */

         bFound = ( info->entry != NULL );
      }
      else
         bFound = FALSE;
   }

#elif defined(HB_OS_MAC)

   {
      /* TODO */

      bFound = FALSE;
   }

#else

   {
      HB_SYMBOL_UNUSED( pszFileName );
      HB_SYMBOL_UNUSED( uiAttr );

      bFound = FALSE;
   }

#endif

   /* Return file info or close the search automatically */

   if( bFound )
      hb_fsFindFill( ffind );
   else
   {
      hb_fsFindClose( ffind );
      ffind = NULL;
   }

   return ffind;
}

BOOL hb_fsFindNext( PHB_FFIND ffind )
{
   PHB_FFIND_INFO info = ffind->info;
   BOOL bFound;

   /* Do platform dependant search */

#if defined(HB_OS_DOS)

   {
      bFound = ( findnext( &info->entry ) == 0 );
   }

#elif defined(HB_OS_OS2)

   {
      bFound = DosFindNext( info->hFindFile, &info->entry, sizeof( info->entry ), &info->findCount ) == NO_ERROR && 
                            info->findCount > 0;
   }

#elif defined(HB_OS_WIN_32)

   {
      bFound = FALSE;

      while( FindNextFile( info->hFindFile, &info->pFindFileData ) )
      {
         if( info->dwAttr == 0 || ( info->dwAttr & info->pFindFileData.dwFileAttributes ) )
         {
            bFound = TRUE;
            break;
         }
      }
   }

#elif defined(HB_OS_UNIX)

   {
      bFound = ( ( entry = readdir( info->dir ) ) != NULL );
   }

#elif defined(HB_OS_MAC)

   {
      /* TODO */

      bFound = FALSE;
   }

#else

   {
      HB_SYMBOL_UNUSED( info );

      bFound = FALSE;
   }

#endif

   /* Return file info */

   if( bFound )
      hb_fsFindFill( ffind );

   return bFound;
}

void hb_fsFindClose( PHB_FFIND ffind )
{
   if( ffind != NULL )
   {
      /* Do platform dependant cleanup */

      if( ffind->info != NULL )
      {
         PHB_FFIND_INFO info = ffind->info;

#if defined(HB_OS_DOS)
        
         #if !defined(__DJGPP__)
         {
            findclose( &info->entry );
         }
         #endif
        
#elif defined(HB_OS_OS2)
         
         {
            DosFindClose( info->hFindFile );
         }
         
#elif defined(HB_OS_WIN_32)
   
         {
            FindClose( info->hFindFile );
         }
   
#elif defined(HB_OS_UNIX)
   
         {
            closedir( info->dir );
         }
   
#elif defined(HB_OS_MAC)

         {
            /* TODO */
         }

#else

         {
            /* Intentionally do nothing */

            HB_SYMBOL_UNUSED( info );
         }

#endif

         hb_xfree( ( void * ) ffind->info );
      }

      hb_xfree( ( void * ) ffind );
   }
}
