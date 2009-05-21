/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    functions to access files with shared handles and locks
 *    (buffers in the future)
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_INTERNAL_
struct _HB_FILE;
typedef struct _HB_FILE * PHB_FILE;


#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbthread.h"
#include "hbvm.h"

#if !defined(HB_OS_WIN_CE)
#  include <sys/types.h>
#  include <sys/stat.h>
#endif

#define HB_FLOCK_RESIZE       16

typedef struct
{
   HB_FOFFSET     start;
   HB_FOFFSET     len;
}
HB_FLOCK, * PHB_FLOCK;

typedef struct _HB_FILE
{
   ULONG          device;
   ULONG          inode;
   int            used;
   BOOL           shared;
   BOOL           readonly;
   HB_FHANDLE     hFile;
   PHB_FLOCK      pLocks;
   UINT           uiLocks;
   UINT           uiSize;
   struct _HB_FILE * pNext;
   struct _HB_FILE * pPrev;
}
HB_FILE;

static HB_CRITICAL_NEW( s_fileMtx );

static PHB_FILE s_openFiles = NULL;

/*
void hb_fileDsp( PHB_FILE pFile, const char * szMsg )
{
   UINT uiPos = 0;
   fprintf( stderr, "\r\n[%s][", szMsg );
   while( uiPos < pFile->uiLocks )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos ];
      fprintf( stderr, "%lld:%lld ", pLock->start, pLock->len );
      ++uiPos;
   }
   fprintf( stderr, "]" );
   fflush(stderr);
}
*/

static PHB_FILE hb_fileFind( ULONG device, ULONG inode )
{
   if( s_openFiles && ( device || inode ) )
   {
      PHB_FILE pFile = s_openFiles;
      do
      {
         if( pFile->device == device && pFile->inode == inode )
            return pFile;
         pFile = pFile->pNext;
      }
      while( s_openFiles != pFile );
   }
   return NULL;
}

static PHB_FILE hb_fileNew( HB_FHANDLE hFile, BOOL fShared, BOOL fReadonly,
                            ULONG device, ULONG inode, BOOL fBind )
{
   PHB_FILE pFile = hb_fileFind( device, inode );

   if( !pFile )
   {
      pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );
      memset( pFile, 0, sizeof( HB_FILE ) );
      pFile->device   = device;
      pFile->inode    = inode;
      pFile->hFile    = hFile;
      pFile->shared   = fShared;
      pFile->readonly = fReadonly;

      if( fBind )
      {
         if( s_openFiles )
         {
            pFile->pNext = s_openFiles;
            pFile->pPrev = s_openFiles->pPrev;
            pFile->pPrev->pNext = pFile;
            s_openFiles->pPrev = pFile;
         }
         else
            s_openFiles = pFile->pNext = pFile->pPrev = pFile;
      }
   }
   pFile->used++;

   return pFile;
}

static UINT hb_fileFindOffset( PHB_FILE pFile, HB_FOFFSET ulOffset )
{
   UINT uiFirst, uiLast, uiMiddle;

   uiFirst = 0;
   uiLast = pFile->uiLocks;
   uiMiddle = uiLast >> 1;

   while( uiFirst < uiLast )
   {
      HB_FOFFSET ulEnd = pFile->pLocks[ uiMiddle ].start +
                         pFile->pLocks[ uiMiddle ].len;
      if( ulEnd <= ulOffset )
         uiFirst = uiMiddle + 1;
      else
         uiLast = uiMiddle;
      uiMiddle = ( uiFirst + uiLast ) >> 1;
   }

   return uiMiddle;
}

static void hb_fileInsertLock( PHB_FILE pFile, UINT uiPos,
                               HB_FOFFSET ulStart, HB_FOFFSET ulLen )
{
   if( pFile->uiLocks == pFile->uiSize )
   {
      pFile->uiSize += HB_FLOCK_RESIZE;
      pFile->pLocks = ( PHB_FLOCK ) hb_xrealloc( pFile->pLocks,
                                          sizeof( HB_FLOCK ) * pFile->uiSize );
      memset( &pFile->pLocks[ pFile->uiLocks ], 0,
              sizeof( HB_FLOCK ) * HB_FLOCK_RESIZE );
   }
   memmove( &pFile->pLocks[ uiPos + 1 ], &pFile->pLocks[ uiPos ],
            ( pFile->uiLocks - uiPos ) * sizeof( HB_FLOCK ) );
   pFile->pLocks[ uiPos ].start = ulStart;
   pFile->pLocks[ uiPos ].len   = ulLen;
   pFile->uiLocks++;
}

static void hb_fileDeleteLock( PHB_FILE pFile, UINT uiPos )
{
   pFile->uiLocks--;
   memmove( &pFile->pLocks[ uiPos ], &pFile->pLocks[ uiPos + 1 ],
            ( pFile->uiLocks - uiPos ) * sizeof( HB_FLOCK ) );
   if( pFile->uiSize - pFile->uiLocks >= ( HB_FLOCK_RESIZE << 1 ) )
   {
      pFile->uiSize -= HB_FLOCK_RESIZE;
      pFile->pLocks = ( PHB_FLOCK ) hb_xrealloc( pFile->pLocks,
                                          sizeof( HB_FLOCK ) * pFile->uiSize );
   }
}

static BOOL hb_fileSetLock( PHB_FILE pFile, BOOL * pfLockFS,
                            HB_FOFFSET ulStart, HB_FOFFSET ulLen )
{
   BOOL fLJoin, fRJoin;
   UINT uiPos;

   uiPos = hb_fileFindOffset( pFile, ulStart );
   fLJoin = fRJoin = FALSE;
   if( uiPos < pFile->uiLocks )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos ];
      if( ulStart + ulLen > pLock->start )
         return FALSE;
      if( ulStart + ulLen == pLock->start )
         fRJoin = TRUE;
   }
   if( uiPos > 0 )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos - 1 ];
      if( pLock->start + pLock->len == ulStart )
         fLJoin = TRUE;
   }
   if( fLJoin )
   {
      if( fRJoin )
      {
         pFile->pLocks[ uiPos - 1 ].len += ulLen + pFile->pLocks[ uiPos ].len;
         hb_fileDeleteLock( pFile, uiPos );
      }
      else
         pFile->pLocks[ uiPos - 1 ].len += ulLen;
   }
   else if( fRJoin )
   {
      pFile->pLocks[ uiPos ].start -= ulLen;
      pFile->pLocks[ uiPos ].len   += ulLen;
   }
   else
      hb_fileInsertLock( pFile, uiPos, ulStart, ulLen );

   if( pFile->shared )
      * pfLockFS = TRUE;
   return TRUE;
}

static BOOL hb_fileUnlock( PHB_FILE pFile, BOOL * pfLockFS,
                           HB_FOFFSET ulStart, HB_FOFFSET ulLen )
{
   BOOL fResult = FALSE;
   UINT uiPos;

   uiPos = hb_fileFindOffset( pFile, ulStart );
   if( uiPos < pFile->uiLocks )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos ];
      if( ulStart >= pLock->start &&
          ulStart + ulLen <= pLock->start + pLock->len )
      {
         if( ulStart == pLock->start )
         {
            if( ulLen == pLock->len )
               hb_fileDeleteLock( pFile, uiPos );
            else
            {
               pLock->start += ulLen;
               pLock->len   -= ulLen;
            }
         }
         else if( ulStart + ulLen == pLock->start + pLock->len )
            pLock->len -= ulLen;
         else
         {
            hb_fileInsertLock( pFile, uiPos + 1, ulStart + ulLen,
                               pLock->start + pLock->len - ulStart - ulLen );
            pLock->len = ulStart - pLock->start;
         }
         if( pFile->shared )
            * pfLockFS = TRUE;
         fResult = TRUE;
      }
   }
   return fResult;
}

/*
 * public API functions
 */

PHB_FILE hb_fileExtOpen( BYTE * pFilename, BYTE * pDefExt,
                         USHORT uiExFlags, BYTE * pPaths,
                         PHB_ITEM pError )
{
   PHB_FILE pFile = NULL;
#if defined( HB_OS_UNIX )
   struct stat statbuf;
   BOOL fResult;
#endif
   BOOL fShared, fReadonly;
   HB_FHANDLE hFile;
   BYTE * pszFile;

   fShared = ( uiExFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0;
   fReadonly = ( uiExFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) ) == FO_READ;
   pszFile = hb_fsExtName( pFilename, pDefExt, uiExFlags, pPaths );

#if defined( HB_OS_UNIX )
   hb_vmUnlock();
   fResult = stat( ( char * ) pszFile, &statbuf ) == 0;
   hb_fsSetIOError( fResult, 0 );
   hb_vmLock();

   if( fResult )
   {
      hb_threadEnterCriticalSection( &s_fileMtx );
      pFile = hb_fileFind( statbuf.st_dev, statbuf.st_ino );
      if( pFile )
      {
         if( !fShared || ! pFile->shared || ( uiExFlags & FXO_TRUNCATE ) != 0 ||
             ( !fReadonly && pFile->readonly ) )
            fResult = FALSE;
         else
            pFile->used++;
      }
      hb_threadLeaveCriticalSection( &s_fileMtx );
   }

   if( pFile )
   {
      if( !fResult )
      {
         hb_fsSetError( ( uiExFlags & FXO_TRUNCATE ) ? 5 : 32 );
         pFile = NULL;
      }
      else if( uiExFlags & FXO_COPYNAME )
         hb_strncpy( ( char * ) pFilename, ( char * ) pszFile, HB_PATH_MAX - 1 );

      if( pError )
      {
         hb_errPutFileName( pError, ( char * ) pszFile );
         if( !fResult )
         {
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutGenCode( pError, ( USHORT ) ( ( uiExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
         }
      }
   }
   else
#endif
   {
      hFile = hb_fsExtOpen( pFilename, pDefExt, uiExFlags, pPaths, pError );
      if( hFile != FS_ERROR )
      {
         ULONG device = 0, inode = 0;
#if defined( HB_OS_UNIX )
         hb_vmUnlock();
         if( fstat( hFile, &statbuf ) == 0 )
         {
            device = ( ULONG ) statbuf.st_dev;
            inode  = ( ULONG ) statbuf.st_ino;
         }
         hb_vmLock();
#endif

         hb_threadEnterCriticalSection( &s_fileMtx );
         pFile = hb_fileNew( hFile, fShared, fReadonly, device, inode, TRUE );
         hb_threadLeaveCriticalSection( &s_fileMtx );

         if( !pFile || pFile->hFile != hFile )
            hb_fsClose( hFile );
         if( !pFile )
            hb_fsSetError( 32 );
      }
   }
   hb_xfree( pszFile );

   return pFile;
}

void hb_fileClose( PHB_FILE pFile )
{
   HB_FHANDLE hFile = FS_ERROR;

   hb_threadEnterCriticalSection( &s_fileMtx );

   if( --pFile->used == 0 )
   {
      if( pFile->pNext )
      {
         pFile->pPrev->pNext = pFile->pNext;
         pFile->pNext->pPrev = pFile->pPrev;
         if( pFile == s_openFiles )
         {
            s_openFiles = pFile->pNext;
            if( pFile == s_openFiles )
               s_openFiles = NULL;
         }
      }

      hFile = pFile->hFile;

      if( pFile->pLocks )
         hb_xfree( pFile->pLocks );

      hb_xfree( pFile );
   }

   hb_threadLeaveCriticalSection( &s_fileMtx );

   if( hFile != FS_ERROR )
      hb_fsClose( hFile );
}

BOOL hb_fileLock( PHB_FILE pFile, HB_FOFFSET ulStart, HB_FOFFSET ulLen,
                  int iType )
{
   BOOL fResult, fLockFS = FALSE;

   if( ( iType & FL_MASK ) == FL_UNLOCK )
   {
      hb_threadEnterCriticalSection( &s_fileMtx );
      fResult = hb_fileUnlock( pFile, &fLockFS, ulStart, ulLen );
      hb_threadLeaveCriticalSection( &s_fileMtx );
      if( fLockFS )
         hb_fsLockLarge( pFile->hFile, ulStart, ulLen, ( USHORT ) iType );
   }
   else
   {
      hb_threadEnterCriticalSection( &s_fileMtx );
      fResult = hb_fileSetLock( pFile, &fLockFS, ulStart, ulLen );
      hb_threadLeaveCriticalSection( &s_fileMtx );
      if( fLockFS )
      {
         fResult = hb_fsLockLarge( pFile->hFile, ulStart, ulLen, ( USHORT ) iType );
         if( !fResult )
         {
            hb_threadEnterCriticalSection( &s_fileMtx );
            hb_fileUnlock( pFile, &fLockFS, ulStart, ulLen );
            hb_threadLeaveCriticalSection( &s_fileMtx );
         }
      }
   }

   return fResult;
}

ULONG hb_fileReadAt( PHB_FILE pFile, BYTE * buffer, ULONG ulSize,
                     HB_FOFFSET llOffset )
{
   return hb_fsReadAt( pFile->hFile, buffer, ulSize, llOffset );
}

ULONG hb_fileWriteAt( PHB_FILE pFile, const BYTE * buffer, ULONG ulSize,
                      HB_FOFFSET llOffset )
{
   return hb_fsWriteAt( pFile->hFile, buffer, ulSize, llOffset );
}

BOOL hb_fileTruncAt( PHB_FILE pFile, HB_FOFFSET llOffset )
{
   return hb_fsTruncAt( pFile->hFile, llOffset );
}

HB_FOFFSET hb_fileSize( PHB_FILE pFile )
{
   return hb_fsSeekLarge( pFile->hFile, 0, FS_END );
}

void hb_fileCommit( PHB_FILE pFile )
{
   hb_fsCommit( pFile->hFile );
}

HB_FHANDLE hb_fileHandle( PHB_FILE pFile )
{
   return pFile ? pFile->hFile : FS_ERROR;
}

PHB_FILE hb_fileCreateTemp( const BYTE * pszDir, const BYTE * pszPrefix,
                            ULONG ulAttr, BYTE * pszName )
{
   PHB_FILE pFile = NULL;
   HB_FHANDLE hFile;

   hFile = hb_fsCreateTemp( pszDir, pszPrefix, ulAttr, pszName );
   if( hFile != FS_ERROR )
      pFile = hb_fileNew( hFile, FALSE, FALSE, 0, 0, FALSE );

   return pFile;
}

PHB_FILE hb_fileCreateTempEx( BYTE * pszName,
                              const BYTE * pszDir,
                              const BYTE * pszPrefix,
                              const BYTE * pszExt,
                              ULONG ulAttr )
{
   PHB_FILE pFile = NULL;
   HB_FHANDLE hFile;

   hFile = hb_fsCreateTempEx( pszName, pszDir, pszPrefix, pszExt, ulAttr );
   if( hFile != FS_ERROR )
      pFile = hb_fileNew( hFile, FALSE, FALSE, 0, 0, FALSE );

   return pFile;
}
