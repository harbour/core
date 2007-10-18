/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour File Find API (C level)
 *
 * Copyright 2001-2002 Luiz Rafael Culik <culik@sl.conex.net>
 *                     Viktor Szakats <viktor.szakats@syenar.hu>
 *                     Paul Tucker <ptucker@sympatico.ca>
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

#if defined(HB_OS_LINUX)
#  define _LARGEFILE64_SOURCE
#endif

#define INCL_DOSFILEMGR
#define INCL_DOSERRORS
#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hb_io.h"


HB_FILE_VER( "$Id$" )

/* ------------------------------------------------------------- */

#include <ctype.h>

#if defined(HB_OS_DOS)

   #if defined(__DJGPP__) || defined(__RSX32__)
      #include <sys/param.h>
   #endif
   #if defined(__DJGPP__) || defined(__RSX32__) || defined(__BORLANDC__)
      #include <sys/stat.h>
   #endif
   #include <dos.h>
#if !defined(__WATCOMC__)
   #include <dir.h>
#endif
   #include <time.h>

#if defined(__WATCOMC__)
   typedef struct
   {
      struct find_t    entry;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;
   
   #define FA_ARCH      _A_ARCH
   #define FA_DIREC     _A_SUBDIR
   #define FA_HIDDEN    _A_HIDDEN
   #define FA_RDONLY    _A_RDONLY
   #define FA_LABEL     _A_VOLID
   #define FA_SYSTEM    _A_SYSTEM
   
   #define ff_name   name
   #define ff_fsize  size
   #define ff_attrib attrib
#else
   typedef struct
   {
      struct ffblk    entry;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;
#endif

#elif defined(HB_OS_OS2)

   #include <sys/types.h>
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
      HANDLE            hFindFile;
      WIN32_FIND_DATAA  pFindFileData;
      DWORD             dwAttr;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

   #define HB_WIN_32_MATCH() \
      ( \
        ( ( info->pFindFileData.dwFileAttributes & ( FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM ) ) == 0 ) || \
        ( ( info->dwAttr & info->pFindFileData.dwFileAttributes & ( FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM ) ) != 0 ) \
      )

#elif defined(HB_OS_UNIX)

   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <dirent.h>
   #include <time.h>

   typedef struct
   {
      DIR *           dir;
      struct dirent * entry;
      char            pattern[ _POSIX_PATH_MAX + 1 ];
      char            path[ _POSIX_PATH_MAX + 1 ];
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#else

   typedef void HB_FFIND_INFO, * PHB_FFIND_INFO;

#endif

/* ------------------------------------------------------------- */

USHORT hb_fsAttrFromRaw( ULONG raw_attr )
{
   USHORT uiAttr;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrFromRaw(%hu)", raw_attr));

#if defined(HB_OS_DOS)

   uiAttr = 0;
   if( raw_attr & FA_ARCH )   uiAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FA_DIREC )  uiAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FA_HIDDEN ) uiAttr |= HB_FA_HIDDEN;
   if( raw_attr & FA_RDONLY ) uiAttr |= HB_FA_READONLY;
   if( raw_attr & FA_LABEL )  uiAttr |= HB_FA_LABEL;
   if( raw_attr & FA_SYSTEM ) uiAttr |= HB_FA_SYSTEM;

#elif defined(HB_OS_OS2)

   uiAttr = 0;
   if( raw_attr & FILE_ARCHIVED )  uiAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FILE_DIRECTORY ) uiAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FILE_HIDDEN )    uiAttr |= HB_FA_HIDDEN;
   if( raw_attr & FILE_READONLY )  uiAttr |= HB_FA_READONLY;
   if( raw_attr & FILE_SYSTEM )    uiAttr |= HB_FA_SYSTEM;

#elif defined(HB_OS_WIN_32)

   uiAttr = 0;
   if( raw_attr & FILE_ATTRIBUTE_ARCHIVE )   uiAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FILE_ATTRIBUTE_DIRECTORY ) uiAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FILE_ATTRIBUTE_HIDDEN )    uiAttr |= HB_FA_HIDDEN;
   if( raw_attr & FILE_ATTRIBUTE_READONLY )  uiAttr |= HB_FA_READONLY;
   if( raw_attr & FILE_ATTRIBUTE_SYSTEM )    uiAttr |= HB_FA_SYSTEM;
   if( raw_attr & FILE_ATTRIBUTE_NORMAL )    uiAttr |= HB_FA_NORMAL;

#ifdef HB_EXTENSION
   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      HB_FA_DEVICE not supported
      HB_FA_VOLCOMP needs to be checked */
   if( raw_attr & FILE_ATTRIBUTE_ENCRYPTED )     uiAttr |= HB_FA_ENCRYPTED;
   if( raw_attr & FILE_ATTRIBUTE_TEMPORARY )     uiAttr |= HB_FA_TEMPORARY;
   if( raw_attr & FILE_ATTRIBUTE_SPARSE_FILE )   uiAttr |= HB_FA_SPARSE;
   if( raw_attr & FILE_ATTRIBUTE_REPARSE_POINT ) uiAttr |= HB_FA_REPARSE;
   if( raw_attr & FILE_ATTRIBUTE_COMPRESSED )    uiAttr |= HB_FA_COMPRESSED;
   if( raw_attr & FILE_ATTRIBUTE_OFFLINE )       uiAttr |= HB_FA_OFFLINE;
   /* FILE_ATTRIBUTE_NOT_CONTENT_INDEXED */
   /* not defined in some older winnt.h  */
   if( raw_attr & 0x00002000 )                   uiAttr |= HB_FA_NOTINDEXED;
   if( raw_attr & 0x00008000 )                   uiAttr |= HB_FA_VOLCOMP;
#endif

#elif defined(HB_OS_UNIX)

   uiAttr = 0;
   if( S_ISREG( raw_attr ) )  uiAttr |= HB_FA_ARCHIVE;
   if( S_ISDIR( raw_attr ) )  uiAttr |= HB_FA_DIRECTORY;
   if( S_ISLNK( raw_attr ) )  uiAttr |= HB_FA_REPARSE;
   if( S_ISCHR( raw_attr ) )  uiAttr |= HB_FA_COMPRESSED;
   if( S_ISBLK( raw_attr ) )  uiAttr |= HB_FA_DEVICE;
   if( S_ISFIFO( raw_attr ) ) uiAttr |= HB_FA_TEMPORARY;
   if( S_ISSOCK( raw_attr ) ) uiAttr |= HB_FA_SPARSE;

#else

   HB_SYMBOL_UNUSED( raw_attr );
   uiAttr = 0;

#endif

   return uiAttr;
}

ULONG hb_fsAttrToRaw( USHORT uiAttr )
{
   ULONG raw_attr;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrToRaw(%hu)", uiAttr));

#if defined(HB_OS_DOS)

   raw_attr = 0;
   if( uiAttr & HB_FA_ARCHIVE )   raw_attr |= FA_ARCH;
   if( uiAttr & HB_FA_DIRECTORY ) raw_attr |= FA_DIREC;
   if( uiAttr & HB_FA_HIDDEN )    raw_attr |= FA_HIDDEN;
   if( uiAttr & HB_FA_READONLY )  raw_attr |= FA_RDONLY;
   if( uiAttr & HB_FA_LABEL )     raw_attr |= FA_LABEL;
   if( uiAttr & HB_FA_SYSTEM )    raw_attr |= FA_SYSTEM;

#elif defined(HB_OS_OS2)

   raw_attr = 0;
   if( uiAttr & HB_FA_ARCHIVE )   raw_attr |= FILE_ARCHIVED;
   if( uiAttr & HB_FA_DIRECTORY ) raw_attr |= FILE_DIRECTORY;
   if( uiAttr & HB_FA_HIDDEN )    raw_attr |= FILE_HIDDEN;
   if( uiAttr & HB_FA_READONLY )  raw_attr |= FILE_READONLY;
   if( uiAttr & HB_FA_SYSTEM )    raw_attr |= FILE_SYSTEM;

#elif defined(HB_OS_WIN_32)

   raw_attr = 0;

   if( uiAttr & HB_FA_ARCHIVE )   raw_attr |= FILE_ATTRIBUTE_ARCHIVE;
   if( uiAttr & HB_FA_DIRECTORY ) raw_attr |= FILE_ATTRIBUTE_DIRECTORY;
   if( uiAttr & HB_FA_HIDDEN )    raw_attr |= FILE_ATTRIBUTE_HIDDEN;
   if( uiAttr & HB_FA_READONLY )  raw_attr |= FILE_ATTRIBUTE_READONLY;
   if( uiAttr & HB_FA_SYSTEM )    raw_attr |= FILE_ATTRIBUTE_SYSTEM;
   if( uiAttr & HB_FA_NORMAL )    raw_attr |= FILE_ATTRIBUTE_NORMAL;

#ifdef HB_EXTENSION
   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      HB_FA_DEVICE not supported
      HB_FA_VOLCOMP needs to be checked */
   if( uiAttr & HB_FA_ENCRYPTED )  raw_attr |= FILE_ATTRIBUTE_ENCRYPTED;
   if( uiAttr & HB_FA_TEMPORARY )  raw_attr |= FILE_ATTRIBUTE_TEMPORARY;
   if( uiAttr & HB_FA_SPARSE )     raw_attr |= FILE_ATTRIBUTE_SPARSE_FILE;
   if( uiAttr & HB_FA_REPARSE )    raw_attr |= FILE_ATTRIBUTE_REPARSE_POINT;
   if( uiAttr & HB_FA_COMPRESSED ) raw_attr |= FILE_ATTRIBUTE_COMPRESSED;
   if( uiAttr & HB_FA_OFFLINE )    raw_attr |= FILE_ATTRIBUTE_OFFLINE;
   if( uiAttr & HB_FA_NOTINDEXED ) raw_attr |= 0x00002000; /* FILE_ATTRIBUTE_NOT_CONTENT_INDEXED not defined in some older winnt.h */
   if( uiAttr & HB_FA_VOLCOMP )    raw_attr |= 0x00008000;
#endif

#elif defined(HB_OS_UNIX)

   raw_attr = 0;
   if( uiAttr & HB_FA_ARCHIVE )    raw_attr |= S_IFREG;
   if( uiAttr & HB_FA_DIRECTORY )  raw_attr |= S_IFDIR;
   if( uiAttr & HB_FA_REPARSE )    raw_attr |= S_IFLNK;
   if( uiAttr & HB_FA_COMPRESSED ) raw_attr |= S_IFCHR;
   if( uiAttr & HB_FA_DEVICE )     raw_attr |= S_IFBLK;
   if( uiAttr & HB_FA_TEMPORARY )  raw_attr |= S_IFIFO;
   if( uiAttr & HB_FA_SPARSE )     raw_attr |= S_IFSOCK;

#else

   HB_SYMBOL_UNUSED( uiAttr );
   raw_attr = 0;

#endif

   return raw_attr;
}

/* Converts a CA-Cl*pper compatible file attribute string
   to the internal reprensentation. */

USHORT hb_fsAttrEncode( const char * szAttr )
{
   const char * pos = szAttr;
   char ch;
   USHORT uiAttr = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrEncode(%p)", szAttr));

   while( ( ch = toupper( *pos ) ) != '\0' )
   {
      switch( ch )
      {
         case 'R': uiAttr |= HB_FA_READONLY;   break;
         case 'H': uiAttr |= HB_FA_HIDDEN;     break;
         case 'S': uiAttr |= HB_FA_SYSTEM;     break;
         case 'V': uiAttr |= HB_FA_LABEL;      break;
         case 'D': uiAttr |= HB_FA_DIRECTORY;  break;
         case 'A': uiAttr |= HB_FA_ARCHIVE;    break;
#ifdef HB_EXTENSION
         case 'E': uiAttr |= HB_FA_ENCRYPTED;  break;
         case 'T': uiAttr |= HB_FA_TEMPORARY;  break;
         case 'P': uiAttr |= HB_FA_SPARSE;     break;
         case 'L': uiAttr |= HB_FA_REPARSE;    break;
         case 'C': uiAttr |= HB_FA_COMPRESSED; break;
         case 'O': uiAttr |= HB_FA_OFFLINE;    break;
         case 'X': uiAttr |= HB_FA_NOTINDEXED; break;
         case 'I': uiAttr |= HB_FA_DEVICE;     break;
         case 'M': uiAttr |= HB_FA_VOLCOMP;    break;
#endif
      }

      pos++;
   }

   return uiAttr;
}

/* Converts a file attribute (ffind->attr) to the CA-Cl*pper
   compatible file attribute string format. */

/* NOTE: szAttr buffer must be at least 16 chars long */

char * hb_fsAttrDecode( USHORT uiAttr, char * szAttr )
{
   char * ptr = szAttr;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrDecode(%hu, %p)", uiAttr, szAttr));

   /* Using the same order as CA-Cl*pper did: RHSVDA. */
   if( uiAttr & HB_FA_READONLY   ) *ptr++ = 'R';
   if( uiAttr & HB_FA_HIDDEN     ) *ptr++ = 'H';
   if( uiAttr & HB_FA_SYSTEM     ) *ptr++ = 'S';
   if( uiAttr & HB_FA_LABEL      ) *ptr++ = 'V';
   if( uiAttr & HB_FA_DIRECTORY  ) *ptr++ = 'D';
   if( uiAttr & HB_FA_ARCHIVE    ) *ptr++ = 'A';
#ifdef HB_EXTENSION
   if( uiAttr & HB_FA_ENCRYPTED  ) *ptr++ = 'E';
   if( uiAttr & HB_FA_TEMPORARY  ) *ptr++ = 'T';
   if( uiAttr & HB_FA_SPARSE     ) *ptr++ = 'P';
   if( uiAttr & HB_FA_REPARSE    ) *ptr++ = 'L';
   if( uiAttr & HB_FA_COMPRESSED ) *ptr++ = 'C';
   if( uiAttr & HB_FA_OFFLINE    ) *ptr++ = 'O';
   if( uiAttr & HB_FA_NOTINDEXED ) *ptr++ = 'X';
   if( uiAttr & HB_FA_DEVICE     ) *ptr++ = 'I';
   if( uiAttr & HB_FA_VOLCOMP    ) *ptr++ = 'M';
#endif

   *ptr = '\0';

   return szAttr;
}

/* Finds the first then the next matching file on 
   each call. Does low-level (platform dependent 
   filtering if needed. */

static BOOL hb_fsFindNextLow( PHB_FFIND ffind )
{
   BOOL bFound;

   USHORT nYear = 0;
   USHORT nMonth = 0;
   USHORT nDay = 0;

   USHORT nHour = 0;
   USHORT nMin = 0;
   USHORT nSec = 0;

   ULONG raw_attr = 0;

   /* Set the default values in case some platforms don't
      support some of these, or they may fail on them. */

   ffind->szName[ 0 ] = '\0';
   ffind->size = 0;

   /* Do platform dependant first/next search */

#if defined(HB_OS_DOS)

   {
      PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

      /* Handling HB_FA_LABEL doesn't need any special tricks 
         under the DOS platform. */
         
      if( ffind->bFirst )
      {
         ffind->bFirst = FALSE;
         
         tzset();
         
#if defined(__WATCOMC__)
         bFound = ( _dos_findfirst( ffind->pszFileMask, ( USHORT ) hb_fsAttrToRaw( ffind->attrmask ), &info->entry ) == 0 );
#else
         bFound = ( findfirst( ffind->pszFileMask, &info->entry, ( USHORT ) hb_fsAttrToRaw( ffind->attrmask ) ) == 0 );
#endif
      }
      else
      {
#if defined(__WATCOMC__)
         bFound = ( _dos_findnext( &info->entry ) == 0 );
#else
         bFound = ( findnext( &info->entry ) == 0 );
#endif
      }

      /* Fill Harbour found file info */

      if( bFound )
      {
         strncpy( ffind->szName, info->entry.ff_name, _POSIX_PATH_MAX );
         ffind->size = info->entry.ff_fsize;
      
         raw_attr = info->entry.ff_attrib;
      
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
      hb_fsSetIOError( bFound, 0 );
   }

#elif defined(HB_OS_OS2)

   {
      PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

      /* TODO: HB_FA_LABEL handling */
         
      if( ffind->bFirst )
      {
         ffind->bFirst = FALSE;
         
         tzset();
         
         info->hFindFile = HDIR_CREATE;
         info->findCount = 1;
         
         bFound = DosFindFirst( ffind->pszFileMask,
                                &info->hFindFile,
                                ( LONG ) hb_fsAttrToRaw( ffind->attrmask ),
                                &info->entry,
                                sizeof( info->entry ),
                                &info->findCount,
                                FIL_STANDARD ) == NO_ERROR && info->findCount > 0;
      }
      else
         bFound = DosFindNext( info->hFindFile, 
                               &info->entry, 
                               sizeof( info->entry ), 
                               &info->findCount ) == NO_ERROR && info->findCount > 0;

      /* Fill Harbour found file info */

      if( bFound )
      {
         struct stat sStat;
      
         stat( info->entry.achName, &sStat );
      
         strncpy( ffind->szName, info->entry.achName, _POSIX_PATH_MAX );
         ffind->size = sStat.st_size;
      
         raw_attr = info->entry.attrFile;
      
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
      hb_fsSetIOError( bFound, 0 );
   }

#elif defined(HB_OS_WIN_32)

   {
      PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

      bFound = FALSE;
         
      if( ffind->attrmask & HB_FA_LABEL )
      {
         if( ffind->bFirst )
         {
            ffind->bFirst = FALSE;
            ffind->szName[ 0 ] = '\0';

            bFound = GetVolumeInformationA( ffind->pszFileMask, ffind->szName, _POSIX_PATH_MAX, NULL, NULL, NULL, NULL, 0 );
         }
      }
      else
      {
         if( ffind->bFirst )
         {
            ffind->bFirst = FALSE;

            info->hFindFile = FindFirstFileA( ffind->pszFileMask, &info->pFindFileData );
            info->dwAttr    = ( DWORD ) hb_fsAttrToRaw( ffind->attrmask );

            if( ( info->hFindFile != INVALID_HANDLE_VALUE ) && HB_WIN_32_MATCH() )
               bFound = TRUE;
         }

         if( ! bFound && info->hFindFile != INVALID_HANDLE_VALUE )
         {
            while( FindNextFileA( info->hFindFile, &info->pFindFileData ) )
            {
               if( HB_WIN_32_MATCH() )
               {
                  bFound = TRUE;
                  break;
               }
            }
         }

         /* Fill Harbour found file info */

         if( bFound )
         {
            strncpy( ffind->szName, info->pFindFileData.cFileName, _POSIX_PATH_MAX );

            if( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
               ffind->size = 0;
            else
               ffind->size = ( HB_FOFFSET ) info->pFindFileData.nFileSizeLow +
                     ( ( HB_FOFFSET ) info->pFindFileData.nFileSizeHigh << 32 );

            raw_attr = ( USHORT ) info->pFindFileData.dwFileAttributes;

            /* NOTE: One of these may fail when searching on an UNC path, I
                     don't know yet what's the reason. [vszakats] */

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
            }
         }
      }
      hb_fsSetIOError( bFound, 0 );
   }

#elif defined(HB_OS_UNIX)

   {
      PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

      char dirname[ _POSIX_PATH_MAX + 1 ];
      char string[ _POSIX_PATH_MAX + 1 ];

      bFound = FALSE;

      /* TODO: HB_FA_LABEL handling */

      string[ 0 ] = '\0';
      if( ffind->bFirst )
      {
         char * pos;

         ffind->bFirst = FALSE;
         
         dirname[ 0 ] = '\0';
         info->pattern[ 0 ] = '\0';
         
         /* hb_strncpy( string, pszFileName, sizeof( string ) - 1 ); */
         hb_strncpy( string, ffind->pszFileMask, sizeof( string ) - 1 ); 
         pos = strrchr( string, OS_PATH_DELIMITER );
         if( pos )
         {
            hb_strncpy( info->pattern, pos + 1, sizeof( info->pattern ) - 1 );
            *( pos + 1 ) = '\0';
            hb_strncpy( dirname, string, sizeof( dirname ) - 1 );
         }
         else
         {
            hb_strncpy( info->pattern, string, sizeof( info->pattern ) - 1 );
            dirname[ 0 ] = '.';
            dirname[ 1 ] = OS_PATH_DELIMITER;
            dirname[ 2 ] = '\0';
         }

         tzset();

         info->dir = opendir( dirname );
         hb_strncpy( info->path, dirname, sizeof( info->path ) - 1 );
      }

      if( info->dir != NULL && info->pattern[ 0 ] != '\0' )
      {
         while( ( info->entry = readdir( info->dir ) ) != NULL )
         {
            hb_strncpy( string, info->entry->d_name, sizeof( string ) - 1 );
            if( hb_strMatchFile( string, info->pattern ) )
            {
               bFound = TRUE;
               break;
            }
         }
      }

      /* Fill Harbour found file info */
      if( bFound )
      {
         hb_strncpy( dirname, info->path, sizeof( dirname ) - 1 );
         hb_strncat( dirname, info->entry->d_name, sizeof( dirname ) - 1 );
         {
            time_t ftime;
            struct tm * ft;
#if defined(HB_OS_LINUX) && defined(__USE_LARGEFILE64)
            /*
             * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
             * define and efectively enables lseek64/flock64/ftruncate64 functions
             * on 32bit machines.
             */
            struct stat64 sStat;
            if( stat64( dirname, &sStat ) == 0 )
#else
            struct stat sStat;
            if( stat( dirname, &sStat ) == 0 )
#endif
            {
               strncpy( ffind->szName, info->entry->d_name, _POSIX_PATH_MAX );
               ffind->size = sStat.st_size;

               raw_attr = sStat.st_mode;

               ftime = sStat.st_mtime;
               ft = localtime( &ftime );

               nYear  = ft->tm_year + 1900;
               nMonth = ft->tm_mon + 1;
               nDay   = ft->tm_mday;

               nHour  = ft->tm_hour;
               nMin   = ft->tm_min;
               nSec   = ft->tm_sec;
            }
            else
               bFound = FALSE;
         }
      }
      hb_fsSetIOError( bFound, 0 );
   }

#else

   {
      /* HB_SYMBOL_UNUSED( ffind ); */

      HB_SYMBOL_UNUSED( nYear );
      HB_SYMBOL_UNUSED( nMonth );
      HB_SYMBOL_UNUSED( nDay );
      HB_SYMBOL_UNUSED( nHour );
      HB_SYMBOL_UNUSED( nMin );
      HB_SYMBOL_UNUSED( nSec );
      HB_SYMBOL_UNUSED( raw_attr );

      bFound = FALSE;

      hb_fsSetError( (USHORT) FS_ERROR );
   }

#endif

   /* Fill common Harbour found file info */

   if( bFound )
   {
      /* Do the conversions common for all platforms */
      
      ffind->szName[ _POSIX_PATH_MAX ] = '\0';

      ffind->attr = hb_fsAttrFromRaw( raw_attr );

      ffind->lDate = hb_dateEncode( nYear, nMonth, nDay );
      hb_dateStrPut( ffind->szDate, nYear, nMonth, nDay );
      ffind->szDate[ 8 ] = '\0';

      snprintf( ffind->szTime, sizeof( ffind->szTime ), "%02d:%02d:%02d", nHour, nMin, nSec );
   }

   return bFound;
}

HB_EXPORT PHB_FFIND hb_fsFindFirst( const char * pszFileMask, USHORT attrmask )
{
   PHB_FFIND ffind = ( PHB_FFIND ) hb_xgrab( sizeof( HB_FFIND ) );

   /* Allocate platform dependent file find info storage */

   ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );

   /* Store search parameters */

   ffind->pszFileMask = pszFileMask;
   ffind->attrmask = attrmask;
   ffind->bFirst = TRUE;

   /* Find first/next matching file */

   if( hb_fsFindNext( ffind ) )
      return ffind;

   /* If no file found at all, free stuff allocated so far and return NULL. */

   hb_fsFindClose( ffind );

   return NULL;
}

/* Finds next matching file, and applies a filter which makes 
   searching CA-Cl*pper/MS-DOS compatible. */

HB_EXPORT BOOL hb_fsFindNext( PHB_FFIND ffind )
{
   while( hb_fsFindNextLow( ffind ) )
   {
      /* Filter the result to stay MS-DOS and CA-Cl*pper compatible. */

      if( !( ( ( ffind->attrmask & HB_FA_HIDDEN    ) == 0 && ( ffind->attr & HB_FA_HIDDEN    ) != 0 ) ||
             ( ( ffind->attrmask & HB_FA_SYSTEM    ) == 0 && ( ffind->attr & HB_FA_SYSTEM    ) != 0 ) ||
             ( ( ffind->attrmask & HB_FA_LABEL     ) == 0 && ( ffind->attr & HB_FA_LABEL     ) != 0 ) ||
             ( ( ffind->attrmask & HB_FA_DIRECTORY ) == 0 && ( ffind->attr & HB_FA_DIRECTORY ) != 0 ) ) )
      {
         return TRUE;
      }
   }

   return FALSE;
}

HB_EXPORT void hb_fsFindClose( PHB_FFIND ffind )
{
   if( ffind != NULL )
   {
      /* Do platform dependant cleanup */

      if( ffind->info != NULL )
      {
         PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

#if defined(HB_OS_DOS)

         #if defined(__DJGPP__) || defined(__BORLANDC__)
         {
            HB_SYMBOL_UNUSED( info );
         }
         #else
         {
#if defined(__WATCOMC__)
            _dos_findclose( &info->entry );
#else
            findclose( &info->entry );
#endif
         }
         #endif

#elif defined(HB_OS_OS2)

         {
            DosFindClose( info->hFindFile );
         }

#elif defined(HB_OS_WIN_32)

         if( info->hFindFile != INVALID_HANDLE_VALUE )
         {
            FindClose( info->hFindFile );
         }

#elif defined(HB_OS_UNIX)

         {
            closedir( info->dir );
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
