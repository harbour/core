/*
 * Harbour Project source code:
 *    functions to access files with shared handles and locks
 *    (buffers in the future)
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_INTERNAL_

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbthread.h"
#include "hbvm.h"

#if ! defined( HB_OS_WIN_CE )
#  include <sys/types.h>
#  include <sys/stat.h>
#  if defined( HB_OS_UNIX )
#     include <unistd.h>
#  endif
#endif


#define HB_FLOCK_RESIZE  16

typedef struct
{
   HB_FOFFSET start;
   HB_FOFFSET len;
}
HB_FLOCK, * PHB_FLOCK;

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   HB_ULONG       device;
   HB_ULONG       inode;
   int            used;
   HB_BOOL        shared;
   HB_BOOL        readonly;
   HB_FHANDLE     hFile;
   HB_FHANDLE     hFileRO;
   PHB_FLOCK      pLocks;
   HB_UINT        uiLocks;
   HB_UINT        uiSize;
   struct _HB_FILE * pNext;
   struct _HB_FILE * pPrev;
}
HB_FILE;

static const HB_FILE_FUNCS * s_fileMethods( void );

static HB_CRITICAL_NEW( s_fileMtx );

static PHB_FILE s_openFiles = NULL;

#if 0
void hb_fileDsp( PHB_FILE pFile, const char * szMsg )
{
   HB_UINT uiPos = 0;

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
#endif

static PHB_FILE hb_fileFind( HB_ULONG device, HB_ULONG inode )
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

static PHB_FILE hb_fileNew( HB_FHANDLE hFile, HB_BOOL fShared, HB_BOOL fReadonly,
                            HB_ULONG device, HB_ULONG inode, HB_BOOL fBind )
{
   PHB_FILE pFile = hb_fileFind( device, inode );

   if( ! pFile )
   {
      pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );
      memset( pFile, 0, sizeof( HB_FILE ) );
      pFile->pFuncs   = s_fileMethods();
      pFile->device   = device;
      pFile->inode    = inode;
      pFile->hFile    = hFile;
      pFile->hFileRO  = FS_ERROR;
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

static HB_UINT hb_fileFindOffset( PHB_FILE pFile, HB_FOFFSET nOffset )
{
   HB_UINT uiFirst, uiLast, uiMiddle;

   uiFirst = 0;
   uiLast = pFile->uiLocks;
   uiMiddle = uiLast >> 1;

   while( uiFirst < uiLast )
   {
      HB_FOFFSET nEnd = pFile->pLocks[ uiMiddle ].start +
                        pFile->pLocks[ uiMiddle ].len;
      if( nEnd > 0 && nEnd <= nOffset )
         uiFirst = uiMiddle + 1;
      else
         uiLast = uiMiddle;
      uiMiddle = ( uiFirst + uiLast ) >> 1;
   }

   return uiMiddle;
}

static void hb_fileInsertLock( PHB_FILE pFile, HB_UINT uiPos,
                               HB_FOFFSET nStart, HB_FOFFSET nLen )
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
   pFile->pLocks[ uiPos ].start = nStart;
   pFile->pLocks[ uiPos ].len   = nLen;
   pFile->uiLocks++;
}

static void hb_fileDeleteLock( PHB_FILE pFile, HB_UINT uiPos )
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

static HB_BOOL hb_fileSetLock( PHB_FILE pFile, HB_BOOL * pfLockFS,
                               HB_FOFFSET nStart, HB_FOFFSET nLen )
{
   HB_BOOL fLJoin, fRJoin;
   HB_UINT uiPos;

   uiPos = hb_fileFindOffset( pFile, nStart );
   fLJoin = fRJoin = HB_FALSE;
   if( uiPos < pFile->uiLocks )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos ];
      HB_FOFFSET nEnd = nStart + nLen;
      if( nEnd <= 0 || nEnd > pLock->start )
         return HB_FALSE;
      if( nEnd == pLock->start )
         fRJoin = HB_TRUE;
   }
   if( uiPos > 0 )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos - 1 ];
      if( pLock->start + pLock->len == nStart )
         fLJoin = HB_TRUE;
   }
   if( fLJoin )
   {
      if( fRJoin )
      {
         pFile->pLocks[ uiPos - 1 ].len += nLen + pFile->pLocks[ uiPos ].len;
         hb_fileDeleteLock( pFile, uiPos );
      }
      else
         pFile->pLocks[ uiPos - 1 ].len += nLen;
   }
   else if( fRJoin )
   {
      pFile->pLocks[ uiPos ].start -= nLen;
      pFile->pLocks[ uiPos ].len   += nLen;
   }
   else
      hb_fileInsertLock( pFile, uiPos, nStart, nLen );

   if( pFile->shared )
      *pfLockFS = HB_TRUE;
   return HB_TRUE;
}

static HB_BOOL hb_fileUnlock( PHB_FILE pFile, HB_BOOL * pfLockFS,
                              HB_FOFFSET nStart, HB_FOFFSET nLen )
{
   HB_BOOL fResult = HB_FALSE;
   HB_UINT uiPos;

   uiPos = hb_fileFindOffset( pFile, nStart );
   if( uiPos < pFile->uiLocks )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos ];
      if( nStart >= pLock->start && pLock->len >= nLen &&
          nStart - pLock->start <= pLock->len - nLen )
      {
         if( pfLockFS && pFile->shared )
            *pfLockFS = HB_TRUE;
         else if( nStart == pLock->start )
         {
            if( nLen == pLock->len )
               hb_fileDeleteLock( pFile, uiPos );
            else
            {
               pLock->start += nLen;
               pLock->len   -= nLen;
            }
         }
         else if( nStart + nLen == pLock->start + pLock->len )
            pLock->len -= nLen;
         else
         {
            hb_fileInsertLock( pFile, uiPos + 1, nStart + nLen,
                               pLock->start + pLock->len - nStart - nLen );
            pLock = &pFile->pLocks[ uiPos ];
            pLock->len = nStart - pLock->start;
         }
         fResult = HB_TRUE;
      }
   }
   return fResult;
}

static HB_BOOL hb_fileTestLock( PHB_FILE pFile,
                                HB_FOFFSET nStart, HB_FOFFSET nLen )
{
   HB_UINT uiPos;

   uiPos = hb_fileFindOffset( pFile, nStart );
   if( uiPos < pFile->uiLocks )
   {
      PHB_FLOCK pLock = &pFile->pLocks[ uiPos ];
      HB_FOFFSET nEnd = nStart + nLen;
      if( nEnd <= 0 || nEnd > pLock->start )
         return HB_TRUE;
   }

   return HB_FALSE;
}


/*
 * file methods
 */

static HB_BOOL s_fileAccept( const char * pFilename )
{
   return pFilename && *pFilename;
}

static PHB_FILE s_fileExtOpen( const char * pFilename, const char * pDefExt,
                               HB_USHORT uiExFlags, const char * pPaths,
                               PHB_ITEM pError )
{
   PHB_FILE pFile = NULL;

#if defined( HB_OS_UNIX )
   struct stat statbuf;
   HB_BOOL fResult;
#endif
   HB_BOOL fShared, fReadonly;
   HB_FHANDLE hFile;
   char * pszFile;

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
         if( ! fShared || ! pFile->shared || ( uiExFlags & FXO_TRUNCATE ) != 0 )
            fResult = HB_FALSE;
         else if( ! fReadonly && pFile->readonly )
            pFile = NULL;
         else
            pFile->used++;
      }
      hb_threadLeaveCriticalSection( &s_fileMtx );
   }

   if( pFile )
   {
      if( ! fResult )
      {
         hb_fsSetError( ( uiExFlags & FXO_TRUNCATE ) ? 5 : 32 );
         pFile = NULL;
      }
      else if( uiExFlags & FXO_COPYNAME )
         hb_strncpy( ( char * ) pFilename, pszFile, HB_PATH_MAX - 1 );

      if( pError )
      {
         hb_errPutFileName( pError, pszFile );
         if( ! fResult )
         {
            hb_errPutOsCode( pError, hb_fsError() );
            hb_errPutGenCode( pError, ( HB_ERRCODE ) ( ( uiExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
         }
      }
   }
   else
#endif
   {
      hFile = hb_fsExtOpen( pFilename, pDefExt, uiExFlags, pPaths, pError );
      if( hFile != FS_ERROR )
      {
         HB_ULONG device = 0, inode = 0;
#if defined( HB_OS_UNIX )
         hb_vmUnlock();
         if( fstat( hFile, &statbuf ) == 0 )
         {
            device = ( HB_ULONG ) statbuf.st_dev;
            inode  = ( HB_ULONG ) statbuf.st_ino;
         }
         hb_vmLock();
#endif

         hb_threadEnterCriticalSection( &s_fileMtx );
         pFile = hb_fileNew( hFile, fShared, fReadonly, device, inode, HB_TRUE );
         if( pFile->hFile != hFile )
         {
            if( pFile->hFileRO == FS_ERROR && ! fReadonly && pFile->readonly )
            {
               pFile->hFileRO = pFile->hFile;
               pFile->hFile = hFile;
               pFile->readonly = HB_FALSE;
               hFile = FS_ERROR;
            }
            if( pFile->uiLocks == 0 )
            {
#if ! defined( HB_USE_SHARELOCKS ) || defined( HB_USE_BSDLOCKS )
               if( pFile->hFileRO != FS_ERROR )
               {
                  hb_fsClose( pFile->hFileRO );
                  pFile->hFileRO = FS_ERROR;
               }
#endif
               if( hFile != FS_ERROR )
               {
                  hb_fsClose( hFile );
                  hFile = FS_ERROR;
#if defined( HB_USE_SHARELOCKS ) && ! defined( HB_USE_BSDLOCKS )
                  /* TOFIX: possible race condition */
                  hb_fsLockLarge( hFile, HB_SHARELOCK_POS, HB_SHARELOCK_SIZE,
                                  FL_LOCK | FLX_SHARED );
#endif
               }
            }
         }
         else
            hFile = FS_ERROR;
         hb_threadLeaveCriticalSection( &s_fileMtx );

         if( hFile != FS_ERROR )
         {
            /* TOFIX: possible race condition in MT mode,
             *        close() is not safe due to existing locks
             *        which are removed.
             */
            hb_fsClose( hFile );
         }
      }
   }
   hb_xfree( pszFile );

   return pFile;
}

static void s_fileClose( PHB_FILE pFile )
{
   HB_FHANDLE hFile = FS_ERROR, hFileRO = FS_ERROR;

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
      hFileRO = pFile->hFileRO;

      if( pFile->pLocks )
         hb_xfree( pFile->pLocks );

      hb_xfree( pFile );
   }

   hb_threadLeaveCriticalSection( &s_fileMtx );

   hb_fsSetError( 0 );

   if( hFile != FS_ERROR )
      hb_fsClose( hFile );
   if( hFileRO != FS_ERROR )
      hb_fsClose( hFileRO );
}

static HB_BOOL s_fileLock( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen,
                           int iType )
{
   HB_BOOL fResult, fLockFS = HB_FALSE;

   if( ( iType & FL_MASK ) == FL_UNLOCK )
   {
      hb_threadEnterCriticalSection( &s_fileMtx );
      fResult = hb_fileUnlock( pFile, &fLockFS, nStart, nLen );
      hb_threadLeaveCriticalSection( &s_fileMtx );
      if( fLockFS )
      {
         hb_fsLockLarge( pFile->hFile, nStart, nLen, ( HB_USHORT ) iType );
         hb_threadEnterCriticalSection( &s_fileMtx );
         hb_fileUnlock( pFile, NULL, nStart, nLen );
         hb_threadLeaveCriticalSection( &s_fileMtx );
      }
      else
         hb_fsSetError( fResult ? 0 : 33 );
   }
   else
   {
      hb_threadEnterCriticalSection( &s_fileMtx );
      fResult = hb_fileSetLock( pFile, &fLockFS, nStart, nLen );
      hb_threadLeaveCriticalSection( &s_fileMtx );
      if( fLockFS )
      {
#if defined( HB_OS_UNIX )
         if( pFile->readonly )
            iType |= FLX_SHARED;
#endif
         fResult = hb_fsLockLarge( pFile->hFile, nStart, nLen, ( HB_USHORT ) iType );
         if( ! fResult )
         {
            hb_threadEnterCriticalSection( &s_fileMtx );
            hb_fileUnlock( pFile, NULL, nStart, nLen );
            hb_threadLeaveCriticalSection( &s_fileMtx );
         }
      }
      else
         hb_fsSetError( fResult ? 0 : 33 );
   }

   return fResult;
}

static int s_fileLockTest( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen,
                           int iType )
{
   HB_BOOL fLocked;
   int iResult;

   hb_threadEnterCriticalSection( &s_fileMtx );
   fLocked = hb_fileTestLock( pFile, nStart, nLen );
   hb_threadLeaveCriticalSection( &s_fileMtx );
   if( fLocked )
   {
#if defined( HB_OS_UNIX )
      iResult = getpid();
#else
      iResult = 1;
#endif
   }
   else
      iResult = hb_fsLockTest( pFile->hFile, nStart, nLen, ( HB_USHORT ) iType );

   return iResult;
}

static HB_SIZE s_fileReadAt( PHB_FILE pFile, void * buffer, HB_SIZE nSize,
                             HB_FOFFSET nOffset )
{
   return hb_fsReadAt( pFile->hFile, buffer, nSize, nOffset );
}

static HB_SIZE s_fileWriteAt( PHB_FILE pFile, const void * buffer, HB_SIZE nSize,
                              HB_FOFFSET nOffset )
{
   return hb_fsWriteAt( pFile->hFile, buffer, nSize, nOffset );
}

static HB_BOOL s_fileTruncAt( PHB_FILE pFile, HB_FOFFSET nOffset )
{
   return hb_fsTruncAt( pFile->hFile, nOffset );
}

static HB_FOFFSET s_fileSize( PHB_FILE pFile )
{
   return hb_fsSeekLarge( pFile->hFile, 0, FS_END );
}

static void s_fileFlush( PHB_FILE pFile, HB_BOOL fDirty )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( fDirty );
}

static void s_fileCommit( PHB_FILE pFile )
{
   hb_fsCommit( pFile->hFile );
}

static HB_FHANDLE s_fileHandle( PHB_FILE pFile )
{
   return pFile ? pFile->hFile : FS_ERROR;
}

static const HB_FILE_FUNCS * s_fileMethods( void )
{
   /* methods table */
   static const HB_FILE_FUNCS s_fileFuncs =
   {
      s_fileAccept,
      hb_spFileExists,
      hb_fsDelete,
      hb_fsRename,
      s_fileExtOpen,
      s_fileClose,
      s_fileLock,
      s_fileLockTest,
      s_fileReadAt,
      s_fileWriteAt,
      s_fileTruncAt,
      s_fileSize,
      s_fileFlush,
      s_fileCommit,
      s_fileHandle
   };

   return &s_fileFuncs;
}



#define HB_FILE_TYPE_MAX  32

static const HB_FILE_FUNCS * s_pFileTypes[ HB_FILE_TYPE_MAX ];
static int s_iFileTypes = 0;


/*
 * public API functions
 */

HB_BOOL hb_fileRegister( const HB_FILE_FUNCS * pFuncs )
{
   HB_BOOL fResult = HB_FALSE;

   hb_threadEnterCriticalSection( &s_fileMtx );

   if( s_iFileTypes < HB_FILE_TYPE_MAX )
   {
      s_pFileTypes[ s_iFileTypes ] = pFuncs;
      s_iFileTypes++;
      fResult = HB_TRUE;
   }

   hb_threadLeaveCriticalSection( &s_fileMtx );

   return fResult;
}

HB_BOOL hb_fileDelete( const char * pFilename )
{
   int i = s_iFileTypes;

   while( --i >= 0 )
   {
      if( s_pFileTypes[ i ]->Accept( pFilename ) )
         return s_pFileTypes[ i ]->Delete( pFilename );
   }
   return hb_fsDelete( pFilename );
}

HB_BOOL hb_fileExists( const char * pFilename, char * pRetPath )
{
   int i = s_iFileTypes;

   while( --i >= 0 )
   {
      if( s_pFileTypes[ i ]->Accept( pFilename ) )
         return s_pFileTypes[ i ]->Exists( pFilename, pRetPath );
   }
   return hb_spFileExists( pFilename, pRetPath );
}

HB_BOOL hb_fileRename( const char * pFilename, const char * pszNewName )
{
   int i = s_iFileTypes;

   while( --i >= 0 )
   {
      if( s_pFileTypes[ i ]->Accept( pFilename ) )
         return s_pFileTypes[ i ]->Rename( pFilename, pszNewName );
   }
   return hb_fsRename( pFilename, pszNewName );
}

PHB_FILE hb_fileExtOpen( const char * pFilename, const char * pDefExt,
                         HB_USHORT uiExFlags, const char * pPaths,
                         PHB_ITEM pError )
{
   int i = s_iFileTypes;

   while( --i >= 0 )
   {
      if( s_pFileTypes[ i ]->Accept( pFilename ) )
         return s_pFileTypes[ i ]->Open( pFilename, pDefExt, uiExFlags, pPaths, pError );
   }
   return s_fileExtOpen( pFilename, pDefExt, uiExFlags, pPaths, pError );
}

void hb_fileClose( PHB_FILE pFile )
{
   pFile->pFuncs->Close( pFile );
}

HB_BOOL hb_fileLock( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen,
                     int iType )
{
   return pFile->pFuncs->Lock( pFile, nStart, nLen, iType );
}

int hb_fileLockTest( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen,
                     int iType )
{
   return pFile->pFuncs->LockTest( pFile, nStart, nLen, iType );
}

HB_SIZE hb_fileReadAt( PHB_FILE pFile, void * buffer, HB_SIZE nSize,
                       HB_FOFFSET nOffset )
{
   return pFile->pFuncs->ReadAt( pFile, buffer, nSize, nOffset );
}

HB_SIZE hb_fileWriteAt( PHB_FILE pFile, const void * buffer, HB_SIZE nSize,
                        HB_FOFFSET nOffset )
{
   return pFile->pFuncs->WriteAt( pFile, buffer, nSize, nOffset );
}

HB_BOOL hb_fileTruncAt( PHB_FILE pFile, HB_FOFFSET nOffset )
{
   return pFile->pFuncs->TruncAt( pFile, nOffset );
}

HB_FOFFSET hb_fileSize( PHB_FILE pFile )
{
   return pFile->pFuncs->Size( pFile );
}

void hb_fileFlush( PHB_FILE pFile, HB_BOOL fDirty )
{
   pFile->pFuncs->Flush( pFile, fDirty );
}

void hb_fileCommit( PHB_FILE pFile )
{
   pFile->pFuncs->Commit( pFile );
}

HB_FHANDLE hb_fileHandle( PHB_FILE pFile )
{
   return pFile->pFuncs->Handle( pFile );
}

/* internal FILE structures only */

PHB_FILE hb_fileCreateTemp( const char * pszDir, const char * pszPrefix,
                            HB_FATTR ulAttr, char * pszName )
{
   PHB_FILE pFile = NULL;
   HB_FHANDLE hFile;

   hFile = hb_fsCreateTemp( pszDir, pszPrefix, ulAttr, pszName );
   if( hFile != FS_ERROR )
      pFile = hb_fileNew( hFile, HB_FALSE, HB_FALSE, 0, 0, HB_FALSE );

   return pFile;
}

PHB_FILE hb_fileCreateTempEx( char * pszName,
                              const char * pszDir,
                              const char * pszPrefix,
                              const char * pszExt,
                              HB_FATTR ulAttr )
{
   PHB_FILE pFile = NULL;
   HB_FHANDLE hFile;

   hFile = hb_fsCreateTempEx( pszName, pszDir, pszPrefix, pszExt, ulAttr );
   if( hFile != FS_ERROR )
      pFile = hb_fileNew( hFile, HB_FALSE, HB_FALSE, 0, 0, HB_FALSE );

   return pFile;
}
