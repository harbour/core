/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour File Find API (C level)
 *
 * Copyright 2001-2002 Luiz Rafael Culik <culik@sl.conex.net>
 * Copyright 2001-2002 Viktor Szakats (harbour syenar.net)
 * Copyright 2001-2002 Paul Tucker <ptucker@sympatico.ca>
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

#if !defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE 1
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbdate.h"
#include "hb_io.h"

/* ------------------------------------------------------------- */

#if defined( HB_OS_DOS )

   #if defined( __DJGPP__ ) || defined( __RSX32__ )
      #include <sys/param.h>
   #endif
   #if defined( __DJGPP__ ) || defined( __RSX32__ ) || defined( __BORLANDC__ )
      #include <sys/stat.h>
   #endif
   #include <dos.h>
#if !defined( __WATCOMC__ )
   #include <dir.h>
#endif
   #include <time.h>

#if defined( __WATCOMC__ )
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

#elif defined( HB_OS_OS2 )

   #define INCL_DOSFILEMGR
   #define INCL_DOSERRORS

   #include <os2.h>

   #include <sys/types.h>
   #include <sys/stat.h>
   #include <time.h>

   typedef struct
   {
      HDIR            hFindFile;
      PFILEFINDBUF3   entry;
      PFILEFINDBUF3   next;
      ULONG           fileTypes;
      ULONG           findSize;
      ULONG           findCount;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#elif defined( HB_OS_WIN )

   #include <windows.h>

   typedef struct
   {
      HANDLE            hFindFile;
      WIN32_FIND_DATA   pFindFileData;
      DWORD             dwAttr;
      HB_BOOL           fLabelDone;
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

   #define _HB_WIN_MASKATTR     ( FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM )
   #define _HB_WIN_MATCH() \
      ( \
         ( ( info->pFindFileData.dwFileAttributes & _HB_WIN_MASKATTR ) == 0 ) || \
         ( ( info->dwAttr & info->pFindFileData.dwFileAttributes & _HB_WIN_MASKATTR ) != 0 ) \
      )

   #if defined( __DMC__)
      #if !defined( FILE_ATTRIBUTE_ENCRYPTED )
         #define FILE_ATTRIBUTE_ENCRYPTED       0x00004000L
      #endif
      #if !defined( FILE_ATTRIBUTE_SPARSE_FILE )
         #define FILE_ATTRIBUTE_SPARSE_FILE     0x00000200L
      #endif
      #if !defined( FILE_ATTRIBUTE_REPARSE_POINT )
         #define FILE_ATTRIBUTE_REPARSE_POINT   0x00000400L
      #endif
   #endif

#elif defined( HB_OS_UNIX )

   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <dirent.h>
   #include <time.h>

   typedef struct
   {
      DIR *           dir;
      struct dirent * entry;
      char            pattern[ HB_PATH_MAX ];
      char            path[ HB_PATH_MAX ];
   } HB_FFIND_INFO, * PHB_FFIND_INFO;

#else

   typedef void HB_FFIND_INFO, * PHB_FFIND_INFO;

#endif

#if !defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * define and efectively enables lseek64/flock64/ftruncate64 functions
       * on 32bit machines.
       */
      #define HB_USE_LARGEFILE64
   #elif defined( HB_OS_UNIX ) && defined( O_LARGEFILE )
      #define HB_USE_LARGEFILE64
   #endif
#endif

/* ------------------------------------------------------------- */

HB_FATTR hb_fsAttrFromRaw( HB_FATTR raw_attr )
{
   HB_FATTR ulAttr;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrFromRaw(%u)", raw_attr));

#if defined( HB_OS_DOS )

   ulAttr = 0;
   if( raw_attr & FA_ARCH )   ulAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FA_DIREC )  ulAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FA_HIDDEN ) ulAttr |= HB_FA_HIDDEN;
   if( raw_attr & FA_RDONLY ) ulAttr |= HB_FA_READONLY;
   if( raw_attr & FA_LABEL )  ulAttr |= HB_FA_LABEL;
   if( raw_attr & FA_SYSTEM ) ulAttr |= HB_FA_SYSTEM;

#elif defined( HB_OS_OS2 )

   ulAttr = 0;
   if( raw_attr & FILE_ARCHIVED )  ulAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FILE_DIRECTORY ) ulAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FILE_HIDDEN )    ulAttr |= HB_FA_HIDDEN;
   if( raw_attr & FILE_READONLY )  ulAttr |= HB_FA_READONLY;
   if( raw_attr & FILE_SYSTEM )    ulAttr |= HB_FA_SYSTEM;

#elif defined( HB_OS_WIN )

   ulAttr = 0;
   if( raw_attr & FILE_ATTRIBUTE_ARCHIVE )   ulAttr |= HB_FA_ARCHIVE;
   if( raw_attr & FILE_ATTRIBUTE_DIRECTORY ) ulAttr |= HB_FA_DIRECTORY;
   if( raw_attr & FILE_ATTRIBUTE_HIDDEN )    ulAttr |= HB_FA_HIDDEN;
   if( raw_attr & FILE_ATTRIBUTE_READONLY )  ulAttr |= HB_FA_READONLY;
   if( raw_attr & FILE_ATTRIBUTE_SYSTEM )    ulAttr |= HB_FA_SYSTEM;
   if( raw_attr & FILE_ATTRIBUTE_NORMAL )    ulAttr |= HB_FA_NORMAL;

   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      HB_FA_DEVICE not supported
      HB_FA_VOLCOMP needs to be checked */
   if( raw_attr & FILE_ATTRIBUTE_ENCRYPTED )     ulAttr |= HB_FA_ENCRYPTED;
   if( raw_attr & FILE_ATTRIBUTE_TEMPORARY )     ulAttr |= HB_FA_TEMPORARY;
   if( raw_attr & FILE_ATTRIBUTE_SPARSE_FILE )   ulAttr |= HB_FA_SPARSE;
   if( raw_attr & FILE_ATTRIBUTE_REPARSE_POINT ) ulAttr |= HB_FA_REPARSE;
   if( raw_attr & FILE_ATTRIBUTE_COMPRESSED )    ulAttr |= HB_FA_COMPRESSED;
   if( raw_attr & FILE_ATTRIBUTE_OFFLINE )       ulAttr |= HB_FA_OFFLINE;
   /* FILE_ATTRIBUTE_NOT_CONTENT_INDEXED */
   /* not defined in some older winnt.h  */
   if( raw_attr & 0x00002000 )                   ulAttr |= HB_FA_NOTINDEXED;
   if( raw_attr & 0x00008000 )                   ulAttr |= HB_FA_VOLCOMP;

#elif defined( HB_OS_UNIX )

   ulAttr = ( ( raw_attr & S_IXOTH ) ? HB_FA_XOTH : 0 ) |
            ( ( raw_attr & S_IWOTH ) ? HB_FA_WOTH : 0 ) |
            ( ( raw_attr & S_IROTH ) ? HB_FA_ROTH : 0 ) |
            ( ( raw_attr & S_IXGRP ) ? HB_FA_XGRP : 0 ) |
            ( ( raw_attr & S_IWGRP ) ? HB_FA_WGRP : 0 ) |
            ( ( raw_attr & S_IRGRP ) ? HB_FA_RGRP : 0 ) |
            ( ( raw_attr & S_IXUSR ) ? HB_FA_XUSR : 0 ) |
            ( ( raw_attr & S_IWUSR ) ? HB_FA_WUSR : 0 ) |
            ( ( raw_attr & S_IRUSR ) ? HB_FA_RUSR : 0 ) |
            ( ( raw_attr & S_ISVTX ) ? HB_FA_SVTX : 0 ) |
            ( ( raw_attr & S_ISGID ) ? HB_FA_SGID : 0 ) |
            ( ( raw_attr & S_ISUID ) ? HB_FA_SUID : 0 );

   if( S_ISREG( raw_attr ) )  ulAttr |= HB_FA_FILE;
   if( S_ISDIR( raw_attr ) )  ulAttr |= HB_FA_DIRECTORY;
   if( S_ISLNK( raw_attr ) )  ulAttr |= HB_FA_LINK;
   if( S_ISCHR( raw_attr ) )  ulAttr |= HB_FA_CHRDEVICE;
   if( S_ISBLK( raw_attr ) )  ulAttr |= HB_FA_BLKDEVICE;
   if( S_ISFIFO( raw_attr ) ) ulAttr |= HB_FA_FIFO;
#if ! defined( HB_OS_VXWORKS )
   if( S_ISSOCK( raw_attr ) ) ulAttr |= HB_FA_SOCKET;
#endif

#else

   HB_SYMBOL_UNUSED( raw_attr );
   ulAttr = 0;

#endif

   return ulAttr;
}

HB_FATTR hb_fsAttrToRaw( HB_FATTR ulAttr )
{
   HB_FATTR raw_attr;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrToRaw(%u)", ulAttr));

#if defined( HB_OS_DOS )

   raw_attr = 0;
   if( ulAttr & HB_FA_ARCHIVE )   raw_attr |= FA_ARCH;
   if( ulAttr & HB_FA_DIRECTORY ) raw_attr |= FA_DIREC;
   if( ulAttr & HB_FA_HIDDEN )    raw_attr |= FA_HIDDEN;
   if( ulAttr & HB_FA_READONLY )  raw_attr |= FA_RDONLY;
   if( ulAttr & HB_FA_LABEL )     raw_attr |= FA_LABEL;
   if( ulAttr & HB_FA_SYSTEM )    raw_attr |= FA_SYSTEM;

#elif defined( HB_OS_OS2 )

   raw_attr = 0;
   if( ulAttr & HB_FA_ARCHIVE )   raw_attr |= FILE_ARCHIVED;
   if( ulAttr & HB_FA_DIRECTORY ) raw_attr |= FILE_DIRECTORY;
   if( ulAttr & HB_FA_HIDDEN )    raw_attr |= FILE_HIDDEN;
   if( ulAttr & HB_FA_READONLY )  raw_attr |= FILE_READONLY;
   if( ulAttr & HB_FA_SYSTEM )    raw_attr |= FILE_SYSTEM;

#elif defined( HB_OS_WIN )

   raw_attr = 0;

   if( ulAttr & HB_FA_ARCHIVE )   raw_attr |= FILE_ATTRIBUTE_ARCHIVE;
   if( ulAttr & HB_FA_DIRECTORY ) raw_attr |= FILE_ATTRIBUTE_DIRECTORY;
   if( ulAttr & HB_FA_HIDDEN )    raw_attr |= FILE_ATTRIBUTE_HIDDEN;
   if( ulAttr & HB_FA_READONLY )  raw_attr |= FILE_ATTRIBUTE_READONLY;
   if( ulAttr & HB_FA_SYSTEM )    raw_attr |= FILE_ATTRIBUTE_SYSTEM;
   if( ulAttr & HB_FA_NORMAL )    raw_attr |= FILE_ATTRIBUTE_NORMAL;

   /* Note that FILE_ATTRIBUTE_NORMAL is not needed
      HB_FA_DEVICE not supported
      HB_FA_VOLCOMP needs to be checked */
   if( ulAttr & HB_FA_ENCRYPTED )  raw_attr |= FILE_ATTRIBUTE_ENCRYPTED;
   if( ulAttr & HB_FA_TEMPORARY )  raw_attr |= FILE_ATTRIBUTE_TEMPORARY;
   if( ulAttr & HB_FA_SPARSE )     raw_attr |= FILE_ATTRIBUTE_SPARSE_FILE;
   if( ulAttr & HB_FA_REPARSE )    raw_attr |= FILE_ATTRIBUTE_REPARSE_POINT;
   if( ulAttr & HB_FA_COMPRESSED ) raw_attr |= FILE_ATTRIBUTE_COMPRESSED;
   if( ulAttr & HB_FA_OFFLINE )    raw_attr |= FILE_ATTRIBUTE_OFFLINE;
   if( ulAttr & HB_FA_NOTINDEXED ) raw_attr |= 0x00002000; /* FILE_ATTRIBUTE_NOT_CONTENT_INDEXED not defined in some older winnt.h */
   if( ulAttr & HB_FA_VOLCOMP )    raw_attr |= 0x00008000;

#elif defined( HB_OS_UNIX )

   raw_attr = HB_FA_POSIX_ATTR( ulAttr );

   if( ulAttr & HB_FA_FILE )       raw_attr |= S_IFREG;
   if( ulAttr & HB_FA_DIRECTORY )  raw_attr |= S_IFDIR;
   if( ulAttr & HB_FA_LINK )       raw_attr |= S_IFLNK;
   if( ulAttr & HB_FA_CHRDEVICE )  raw_attr |= S_IFCHR;
   if( ulAttr & HB_FA_BLKDEVICE )  raw_attr |= S_IFBLK;
   if( ulAttr & HB_FA_FIFO )       raw_attr |= S_IFIFO;
   if( ulAttr & HB_FA_SOCKET )     raw_attr |= S_IFSOCK;

#else

   HB_SYMBOL_UNUSED( ulAttr );
   raw_attr = 0;

#endif

   return raw_attr;
}

/* Converts a CA-Cl*pper compatible file attribute string
   to the internal reprensentation. */

HB_FATTR hb_fsAttrEncode( const char * szAttr )
{
   const char * pos = szAttr;
   char ch;
   HB_FATTR ulAttr = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrEncode(%p)", szAttr));

   while( ( ch = ( char ) HB_TOUPPER( *pos ) ) != '\0' )
   {
      switch( ch )
      {
         case 'R': ulAttr |= HB_FA_READONLY;   break;
         case 'H': ulAttr |= HB_FA_HIDDEN;     break;
         case 'S': ulAttr |= HB_FA_SYSTEM;     break;
         case 'V': ulAttr |= HB_FA_LABEL;      break;
         case 'D': ulAttr |= HB_FA_DIRECTORY;  break;
         case 'A': ulAttr |= HB_FA_ARCHIVE;    break;
      }

      pos++;
   }

   return ulAttr;
}

/* Converts a file attribute (ffind->attr) to the CA-Cl*pper
   compatible file attribute string format. */

/* NOTE: szAttr buffer must be at least 16 chars long */

char * hb_fsAttrDecode( HB_FATTR ulAttr, char * szAttr )
{
   char * ptr = szAttr;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsAttrDecode(%u, %p)", ulAttr, szAttr));

   /* Using the same order as CA-Cl*pper did: RHSVDA. */
   if( ulAttr & HB_FA_READONLY   ) *ptr++ = 'R';
   if( ulAttr & HB_FA_HIDDEN     ) *ptr++ = 'H';
   if( ulAttr & HB_FA_SYSTEM     ) *ptr++ = 'S';
   if( ulAttr & HB_FA_LABEL      ) *ptr++ = 'V';
   if( ulAttr & HB_FA_DIRECTORY  ) *ptr++ = 'D';
   if( ulAttr & HB_FA_ARCHIVE    ) *ptr++ = 'A';

   *ptr = '\0';

   return szAttr;
}

/* Finds the first then the next matching file on
   each call. Does low-level (platform dependent
   filtering if needed. */

static HB_BOOL hb_fsFindNextLow( PHB_FFIND ffind )
{
   HB_BOOL bFound;

   int iYear = 0;
   int iMonth = 0;
   int iDay = 0;

   int iHour = 0;
   int iMin = 0;
   int iSec = 0;

   HB_FATTR raw_attr = 0;

   /* Set the default values in case some platforms don't
      support some of these, or they may fail on them. */

   ffind->szName[ 0 ] = '\0';
   ffind->size = 0;

   /* Do platform dependant first/next search */

   hb_vmUnlock();

#if defined( HB_OS_DOS )

   {
      PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

      /* Handling HB_FA_LABEL doesn't need any special tricks
         under the DOS platform. */

      if( ffind->bFirst )
      {
         ffind->bFirst = HB_FALSE;

         /* tzset(); */

#if defined( __WATCOMC__ )
         bFound = ( _dos_findfirst( ffind->pszFileMask, ( HB_USHORT ) hb_fsAttrToRaw( ffind->attrmask ), &info->entry ) == 0 );
#else
         bFound = ( findfirst( ffind->pszFileMask, &info->entry, ( HB_USHORT ) hb_fsAttrToRaw( ffind->attrmask ) ) == 0 );
#endif
      }
      else
      {
#if defined( __WATCOMC__ )
         bFound = ( _dos_findnext( &info->entry ) == 0 );
#else
         bFound = ( findnext( &info->entry ) == 0 );
#endif
      }

      /* Fill Harbour found file info */

      if( bFound )
      {
         hb_strncpy( ffind->szName, info->entry.ff_name, sizeof( ffind->szName ) - 1 );
         ffind->size = info->entry.ff_fsize;

         raw_attr = info->entry.ff_attrib;

         {
            time_t ftime;
            struct tm * ft;
            struct stat sStat;

            stat( info->entry.ff_name, &sStat );

            ftime = sStat.st_mtime;
            ft = localtime( &ftime );

            iYear  = ft->tm_year + 1900;
            iMonth = ft->tm_mon + 1;
            iDay   = ft->tm_mday;

            iHour  = ft->tm_hour;
            iMin   = ft->tm_min;
            iSec   = ft->tm_sec;
         }
      }
      hb_fsSetIOError( bFound, 0 );
   }

#elif defined( HB_OS_OS2 )

   {
      PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;
      APIRET rc = NO_ERROR;

      /* TODO: HB_FA_LABEL handling */

      if( ffind->bFirst )
      {
         ffind->bFirst = HB_FALSE;

         /* tzset(); */

         info->hFindFile = HDIR_CREATE;
         info->findCount = 128;
         rc = DosAllocMem( (PPVOID) &info->entry, 4 * 1024, OBJ_TILE | PAG_COMMIT | PAG_WRITE );

         if( rc == NO_ERROR )
         {
            bFound = DosFindFirst( ( PCSZ ) ffind->pszFileMask,
                                   &info->hFindFile,
                                   ( ULONG ) hb_fsAttrToRaw( ffind->attrmask ),
                                   info->entry,
                                   4 * 1024,
                                   &info->findCount,
                                   FIL_STANDARD ) == NO_ERROR && info->findCount > 0;

            if( bFound )
               info->next = info->entry;
         }
         else
         {
            info->entry = NULL;
            bFound = HB_FALSE;
         }
      }
      else
      {
         if( info->findCount > 0 )
            bFound = HB_TRUE;
         else
         {
            info->findCount = 128;

            bFound = DosFindNext( info->hFindFile,
                                  info->entry,
                                  4 * 1024,
                                  &info->findCount ) == NO_ERROR && info->findCount > 0;
            if( bFound )
               info->next = info->entry;
         }
      }

      if( bFound )
      {
         hb_strncpy( ffind->szName, info->next->achName, sizeof( ffind->szName ) - 1 );
         ffind->size = info->next->cbFile;
         raw_attr = info->next->attrFile;

         iYear  = info->next->fdateLastWrite.year + 1980;
         iMonth = info->next->fdateLastWrite.month;
         iDay   = info->next->fdateLastWrite.day;

         iHour  = info->next->ftimeLastWrite.hours;
         iMin   = info->next->ftimeLastWrite.minutes;
         iSec   = info->next->ftimeLastWrite.twosecs;

         if( info->next->oNextEntryOffset > 0 )
         {
            info->next = ( PFILEFINDBUF3 )( ( char * ) info->next + info->next->oNextEntryOffset );
            info->findCount--;
         }
         else
            info->findCount = 0;
      }

      hb_fsSetIOError( bFound, 0 );
   }

#elif defined( HB_OS_WIN )

   {
      PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;
      PHB_FNAME pFileName = NULL;
      LPTSTR lpFileMask = NULL;

      bFound = HB_FALSE;

#if !defined( HB_OS_WIN_CE )
      if( ( ffind->attrmask & HB_FA_LABEL ) != 0 && !info->fLabelDone )
      {
         TCHAR szName[ HB_PATH_MAX ];
         const char * mask = NULL;

         info->fLabelDone = HB_TRUE;

         if( ffind->pszFileMask && *ffind->pszFileMask )
         {
            pFileName = hb_fsFNameSplit( ffind->pszFileMask );
            mask = pFileName->szName;
            if( pFileName->szPath && pFileName->szPath[ 0 ] &&
                ( pFileName->szPath[ 1 ] ||
                  pFileName->szPath[ 0 ] != HB_OS_PATH_DELIM_CHR ) )
               lpFileMask = HB_TCHAR_CONVTO( pFileName->szPath );
         }
         bFound = GetVolumeInformation( lpFileMask, szName, sizeof( szName ),
                                        NULL, NULL, NULL, NULL, 0 );
         if( bFound )
         {
            HB_TCHAR_COPYFROM( ffind->szName, szName, sizeof( ffind->szName ) - 1 );
            if( mask && *mask && ! hb_strMatchFile( ffind->szName, mask ) )
            {
               ffind->szName[ 0 ] = '\0';
               bFound = HB_FALSE;
            }
         }
      }
#endif

      if( !bFound &&
          ( ffind->attrmask & ( HB_FA_LABEL | HB_FA_HIDDEN | HB_FA_SYSTEM |
                                HB_FA_DIRECTORY ) ) != HB_FA_LABEL )
      {
         if( ffind->bFirst )
         {
            ffind->bFirst = HB_FALSE;
            if( lpFileMask )
               HB_TCHAR_FREE( lpFileMask );
            lpFileMask = HB_TCHAR_CONVTO( ffind->pszFileMask );
            info->hFindFile = FindFirstFile( lpFileMask, &info->pFindFileData );
            info->dwAttr    = ( DWORD ) hb_fsAttrToRaw( ffind->attrmask );

            if( ( info->hFindFile != INVALID_HANDLE_VALUE ) && _HB_WIN_MATCH() )
               bFound = HB_TRUE;
         }

         if( ! bFound && info->hFindFile != INVALID_HANDLE_VALUE )
         {
            while( FindNextFile( info->hFindFile, &info->pFindFileData ) )
            {
               if( _HB_WIN_MATCH() )
               {
                  bFound = HB_TRUE;
                  break;
               }
            }
         }

         /* Fill Harbour found file info */

         if( bFound )
         {
            HB_TCHAR_COPYFROM( ffind->szName, info->pFindFileData.cFileName, sizeof( ffind->szName ) - 1 );

            if( info->pFindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
               ffind->size = 0;
            else
            {
#if defined( __XCC__ ) || ( defined( __POCC__ ) && __POCC__ >= 500 )
               /* NOTE: PellesC 5.00.1 will go into an infinite loop if we don't
                        split this into two operations. [vszakats] */
               ffind->size  = ( HB_FOFFSET ) info->pFindFileData.nFileSizeLow;
               ffind->size += ( HB_FOFFSET ) info->pFindFileData.nFileSizeHigh << 32;
#else
               ffind->size = ( HB_FOFFSET ) info->pFindFileData.nFileSizeLow +
                           ( ( HB_FOFFSET ) info->pFindFileData.nFileSizeHigh << 32 );
#endif
            }

            raw_attr = ( HB_FATTR ) info->pFindFileData.dwFileAttributes;

            /* NOTE: One of these may fail when searching on an UNC path, I
                     don't know yet what's the reason. [vszakats] */

            {
               FILETIME ft;
               SYSTEMTIME time;

               if( FileTimeToLocalFileTime( &info->pFindFileData.ftLastWriteTime, &ft ) &&
                   FileTimeToSystemTime( &ft, &time ) )
               {
                  iYear  = time.wYear;
                  iMonth = time.wMonth;
                  iDay   = time.wDay;
                  iHour  = time.wHour;
                  iMin   = time.wMinute;
                  iSec   = time.wSecond;
               }
            }
         }
      }
      hb_fsSetIOError( bFound, 0 );

      if( pFileName )
         hb_xfree( pFileName );
      if( lpFileMask )
         HB_TCHAR_FREE( lpFileMask );
   }

#elif defined( HB_OS_UNIX )

   {
      PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

      char dirname[ HB_PATH_MAX ];

      bFound = HB_FALSE;

      /* TODO: HB_FA_LABEL handling */

      if( ffind->bFirst )
      {
         char * pos;

         ffind->bFirst = HB_FALSE;

         hb_strncpy( dirname, ffind->pszFileMask, sizeof( dirname ) - 1 );
         pos = strrchr( dirname, HB_OS_PATH_DELIM_CHR );
         if( pos )
         {
            hb_strncpy( info->pattern, pos + 1, sizeof( info->pattern ) - 1 );
            *( pos + 1 ) = '\0';
         }
         else
         {
            hb_strncpy( info->pattern, dirname, sizeof( info->pattern ) - 1 );
            dirname[ 0 ] = '.';
            dirname[ 1 ] = HB_OS_PATH_DELIM_CHR;
            dirname[ 2 ] = '\0';
         }

         /* tzset(); */

         info->dir = opendir( dirname );
         hb_strncpy( info->path, dirname, sizeof( info->path ) - 1 );
      }

      if( info->dir && info->pattern[ 0 ] != '\0' )
      {
         while( ( info->entry = readdir( info->dir ) ) != NULL )
         {
            if( hb_strMatchFile( info->entry->d_name, info->pattern ) )
            {
               bFound = HB_TRUE;
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
            struct tm lt;
#if defined( HB_USE_LARGEFILE64 )
            struct stat64 sStat;
            if( stat64( dirname, &sStat ) == 0 )
#else
            struct stat sStat;
            if( stat( dirname, &sStat ) == 0 )
#endif
            {
               hb_strncpy( ffind->szName, info->entry->d_name, sizeof( ffind->szName ) - 1 );
               ffind->size = sStat.st_size;

               raw_attr = sStat.st_mode;

               ftime = sStat.st_mtime;
#  if defined( HB_HAS_LOCALTIME_R )
               localtime_r( &ftime, &lt );
#  else
               lt = *localtime( &ftime );
#  endif

               iYear  = lt.tm_year + 1900;
               iMonth = lt.tm_mon + 1;
               iDay   = lt.tm_mday;

               iHour  = lt.tm_hour;
               iMin   = lt.tm_min;
               iSec   = lt.tm_sec;
            }
            else
               bFound = HB_FALSE;
         }
      }
      hb_fsSetIOError( bFound, 0 );
   }

#else

   {
      int iTODO; /* TODO: for given platform */

      /* HB_SYMBOL_UNUSED( ffind ); */

      HB_SYMBOL_UNUSED( iYear );
      HB_SYMBOL_UNUSED( iMonth );
      HB_SYMBOL_UNUSED( iDay );
      HB_SYMBOL_UNUSED( iHour );
      HB_SYMBOL_UNUSED( iMin );
      HB_SYMBOL_UNUSED( iSec );
      HB_SYMBOL_UNUSED( raw_attr );

      bFound = HB_FALSE;

      hb_fsSetIOError( bFound, 0 );
   }

#endif

   /* Fill common Harbour found file info */

   if( bFound )
   {
      /* Do the conversions common for all platforms */
      ffind->szName[ sizeof( ffind->szName ) - 1 ] = '\0';

      /* Convert from OS codepage */
      {
         char * pszFree = NULL;
         HB_SIZE nSize = sizeof( ffind->szName );
         const char * pszResult = hb_osDecodeCP( ffind->szName, &pszFree, &nSize );

         if( pszFree )
         {
            hb_strncpy( ffind->szName, pszResult, sizeof( ffind->szName ) - 1 );
            hb_xfree( pszFree );
         }
      }

      ffind->attr = hb_fsAttrFromRaw( raw_attr );

      ffind->lDate = hb_dateEncode( iYear, iMonth, iDay );
      hb_dateStrPut( ffind->szDate, iYear, iMonth, iDay );
      ffind->szDate[ 8 ] = '\0';

      hb_snprintf( ffind->szTime, sizeof( ffind->szTime ), "%02d:%02d:%02d", iHour, iMin, iSec );
   }
   hb_vmLock();

   return bFound;
}

PHB_FFIND hb_fsFindFirst( const char * pszFileMask, HB_FATTR attrmask )
{
   PHB_FFIND ffind;

   ffind = ( PHB_FFIND ) hb_xgrab( sizeof( HB_FFIND ) );
   memset( ffind, 0, sizeof( HB_FFIND ) );

   /* Allocate platform dependent file find info storage */
   ffind->info = ( void * ) hb_xgrab( sizeof( HB_FFIND_INFO ) );
   memset( ffind->info, 0, sizeof( HB_FFIND_INFO ) );

   /* Store search parameters */
   ffind->pszFileMask = pszFileMask;
   ffind->attrmask = attrmask;
   ffind->bFirst = HB_TRUE;

   /* Find first/next matching file */

   if( hb_fsFindNext( ffind ) )
      return ffind;

   /* If no file found at all, free stuff allocated so far and return NULL. */

   hb_fsFindClose( ffind );

   return NULL;
}

/* Finds next matching file, and applies a filter which makes
   searching CA-Cl*pper/MS-DOS compatible. */

HB_BOOL hb_fsFindNext( PHB_FFIND ffind )
{
   while( hb_fsFindNextLow( ffind ) )
   {
      /* Filter the result to stay MS-DOS and CA-Cl*pper compatible. */

      if( !( ( ( ffind->attrmask & HB_FA_HIDDEN    ) == 0 && ( ffind->attr & HB_FA_HIDDEN    ) != 0 ) ||
             ( ( ffind->attrmask & HB_FA_SYSTEM    ) == 0 && ( ffind->attr & HB_FA_SYSTEM    ) != 0 ) ||
             ( ( ffind->attrmask & HB_FA_LABEL     ) == 0 && ( ffind->attr & HB_FA_LABEL     ) != 0 ) ||
             ( ( ffind->attrmask & HB_FA_DIRECTORY ) == 0 && ( ffind->attr & HB_FA_DIRECTORY ) != 0 ) ) )
      {
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

void hb_fsFindClose( PHB_FFIND ffind )
{
   if( ffind )
   {
      /* Do platform dependant cleanup */

      if( ffind->info )
      {
         PHB_FFIND_INFO info = ( PHB_FFIND_INFO ) ffind->info;

         hb_vmUnlock();

#if defined( HB_OS_DOS )

         #if defined( __DJGPP__ ) || defined( __BORLANDC__ )
         {
            HB_SYMBOL_UNUSED( info );
         }
         #else
         {
#if defined( __WATCOMC__ )
            _dos_findclose( &info->entry );
#else
            findclose( &info->entry );
#endif
         }
         #endif

#elif defined( HB_OS_OS2 )

         {
            DosFindClose( info->hFindFile );
            if( info->entry )
               DosFreeMem( info->entry );
         }

#elif defined( HB_OS_WIN )

         if( info->hFindFile != INVALID_HANDLE_VALUE )
         {
            FindClose( info->hFindFile );
         }

#elif defined( HB_OS_UNIX )

         if( info->dir )
         {
            closedir( info->dir );
         }

#else

         {
            /* Intentionally do nothing */
            int iTODO; /* TODO: for given platform */

            HB_SYMBOL_UNUSED( info );
         }

#endif

         hb_vmLock();

         hb_xfree( ffind->info );
      }

      hb_xfree( ffind );
   }
}
