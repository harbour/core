/*
 * Functions to access files with shared handles and locks
 * (buffers in the future)
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbthread.h"
#include "hbvm.h"
#include "directry.ch"

#if defined( HB_OS_UNIX )
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <unistd.h>
#endif

#if ! defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64()/flock64()/ftruncate64()
       * functions on 32-bit machines.
       */
      #define HB_USE_LARGEFILE64
   #elif defined( HB_OS_UNIX ) && defined( O_LARGEFILE )
      #define HB_USE_LARGEFILE64
   #endif
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
   int            mode;
   HB_BOOL        shared;
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
#if defined( HB_OS_UNIX )
   static PHB_FILE hb_fileposNew( PHB_FILE pFile );
#endif

static HB_CRITICAL_NEW( s_fileMtx );
static HB_CRITICAL_NEW( s_lockMtx );

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

static PHB_FILE hb_fileNew( HB_FHANDLE hFile, HB_BOOL fShared, int iMode,
                            HB_ULONG device, HB_ULONG inode, HB_BOOL fBind )
{
   PHB_FILE pFile = hb_fileFind( device, inode );

   if( ! pFile )
   {
      pFile = ( PHB_FILE ) hb_xgrabz( sizeof( HB_FILE ) );
      pFile->pFuncs  = s_fileMethods();
      pFile->device  = device;
      pFile->inode   = inode;
      pFile->hFile   = hFile;
      pFile->hFileRO = FS_ERROR;
      pFile->shared  = fShared;
      pFile->mode    = iMode;

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

static HB_BOOL s_fileAccept( PHB_FILE_FUNCS pFuncs, const char * pszFileName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return pszFileName && *pszFileName;
}

static HB_BOOL s_fileExists( PHB_FILE_FUNCS pFuncs, const char * pszFileName, char * pRetPath )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return pRetPath ? hb_spFileExists( pszFileName, pRetPath ) :
                     hb_fsFileExists( pszFileName );
}

static HB_BOOL s_fileDelete( PHB_FILE_FUNCS pFuncs, const char * pszFileName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsDelete( pszFileName );
}

static HB_BOOL s_fileRename( PHB_FILE_FUNCS pFuncs, const char * pszName, const char * pszNewName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsRename( pszName, pszNewName );
}

static HB_BOOL s_fileCopy( PHB_FILE_FUNCS pFuncs, const char * pszSrcFile, const char * pszDstFile )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsCopy( pszSrcFile, pszDstFile );
}

static HB_BOOL s_fileDirExists( PHB_FILE_FUNCS pFuncs, const char * pszDirName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsDirExists( pszDirName );
}

static HB_BOOL s_fileDirMake( PHB_FILE_FUNCS pFuncs, const char * pszDirName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsMkDir( pszDirName );
}

static HB_BOOL s_fileDirRemove( PHB_FILE_FUNCS pFuncs, const char * pszDirName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsRmDir( pszDirName );
}

static double s_fileDirSpace( PHB_FILE_FUNCS pFuncs, const char * pszDirName, HB_USHORT uiType )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsDiskSpace( pszDirName, uiType );
}

static PHB_ITEM s_fileDirectory( PHB_FILE_FUNCS pFuncs, const char * pszDirSpec, const char * pszAttr )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsDirectory( pszDirSpec, pszAttr, HB_TRUE );
}

static HB_BOOL s_fileTimeGet( PHB_FILE_FUNCS pFuncs, const char * pszFileName, long * plJulian, long * plMillisec )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsGetFileTime( pszFileName, plJulian, plMillisec );
}

static HB_BOOL s_fileTimeSet( PHB_FILE_FUNCS pFuncs, const char * pszFileName, long lJulian, long lMillisec )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsSetFileTime( pszFileName, lJulian, lMillisec );
}

static HB_BOOL s_fileAttrGet( PHB_FILE_FUNCS pFuncs, const char * pszFileName, HB_FATTR * pnAttr )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsGetAttr( pszFileName, pnAttr );
}

static HB_BOOL s_fileAttrSet( PHB_FILE_FUNCS pFuncs, const char * pszFileName, HB_FATTR nAttr )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsSetAttr( pszFileName, nAttr );
}

static HB_BOOL s_fileLink( PHB_FILE_FUNCS pFuncs, const char * pszExisting, const char * pszNewName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsLink( pszExisting, pszNewName );
}

static HB_BOOL s_fileLinkSym( PHB_FILE_FUNCS pFuncs, const char * pszTarget, const char * pszNewName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsLinkSym( pszTarget, pszNewName );
}

static char * s_fileLinkRead( PHB_FILE_FUNCS pFuncs, const char * pszFileName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_fsLinkRead( pszFileName );
}

static PHB_FILE s_fileExtOpen( PHB_FILE_FUNCS pFuncs, const char * pszFileName, const char * pDefExt,
                               HB_FATTR nExFlags, const char * pPaths,
                               PHB_ITEM pError )
{
   PHB_FILE pFile = NULL;
#if defined( HB_OS_UNIX )
   HB_BOOL fSeek = HB_FALSE;
#  if defined( HB_USE_LARGEFILE64 )
   struct stat64 statbuf;
#  else
   struct stat statbuf;
#  endif
#endif
   HB_BOOL fResult, fShared;
   int iMode;
   char * pszFile;

   HB_SYMBOL_UNUSED( pFuncs );

   fShared = ( nExFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0;
   iMode = ( int ) ( nExFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) );
   pszFile = hb_fsExtName( pszFileName, pDefExt, nExFlags, pPaths );

   hb_vmUnlock();
#if ! defined( HB_OS_UNIX )
   fResult = HB_TRUE;
#else
#  if defined( HB_USE_SHARELOCKS ) && ! defined( HB_USE_BSDLOCKS )
   if( nExFlags & FXO_SHARELOCK )
   {
      if( iMode == FO_WRITE && fShared )
      {
         if( access( pszFile, R_OK ) == 0 ||
             access( pszFile, F_OK ) != 0 )
         {
            nExFlags = ( nExFlags ^ FO_WRITE ) | FO_READWRITE;
            iMode = FO_READWRITE;
         }
         else
            nExFlags ^= FXO_SHARELOCK;
      }
      else if( iMode == FO_READ && ! fShared )
      {
         nExFlags &= ~ ( HB_FATTR ) ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE );
         fShared = HB_TRUE;
      }
   }
#  endif

   hb_threadEnterCriticalSection( &s_fileMtx );

#  if defined( HB_USE_LARGEFILE64 )
   fResult = stat64( pszFile, &statbuf ) == 0;
#  else
   fResult = stat( pszFile, &statbuf ) == 0;
#  endif
   hb_fsSetIOError( fResult, 0 );

   if( fResult )
   {
      pFile = hb_fileFind( ( HB_ULONG ) statbuf.st_dev,
                           ( HB_ULONG ) statbuf.st_ino );
      if( pFile )
      {
         if( ! fShared || ! pFile->shared || ( nExFlags & FXO_TRUNCATE ) != 0 )
         {
            fResult = HB_FALSE;
            pFile = NULL;
         }
         else if( pFile->mode != FO_READWRITE && pFile->mode != iMode )
         {
            iMode = FO_READWRITE;
            pFile = NULL;
         }
         else
         {
            pFile->used++;
            if( ( nExFlags & FXO_NOSEEKPOS ) == 0 )
            {
#  if defined( HB_OS_VXWORKS )
               fSeek  = ! S_ISFIFO( statbuf.st_mode );
#  else
               fSeek  = ! S_ISFIFO( statbuf.st_mode ) && ! S_ISSOCK( statbuf.st_mode );
#  endif
            }
         }
      }
   }
   else
      fResult = HB_TRUE;

   if( fResult && pFile == NULL )
#endif /* HB_OS_UNIX */
   {
      HB_FHANDLE hFile = hb_fsExtOpen( pszFile, NULL,
                            nExFlags & ~ ( HB_FATTR ) ( FXO_DEFAULTS | FXO_COPYNAME ),
                            NULL, NULL );
      if( hFile != FS_ERROR )
      {
         HB_ULONG device = 0, inode = 0;
#if ! defined( HB_OS_UNIX )
         hb_threadEnterCriticalSection( &s_fileMtx );
#else
#  if defined( HB_USE_LARGEFILE64 )
         if( fstat64( hFile, &statbuf ) == 0 )
#  else
         if( fstat( hFile, &statbuf ) == 0 )
#  endif
         {
            device = ( HB_ULONG ) statbuf.st_dev;
            inode  = ( HB_ULONG ) statbuf.st_ino;
            if( ( nExFlags & FXO_NOSEEKPOS ) == 0 )
            {
#  if defined( HB_OS_VXWORKS )
               fSeek  = ! S_ISFIFO( statbuf.st_mode );
#  else
               fSeek  = ! S_ISFIFO( statbuf.st_mode ) && ! S_ISSOCK( statbuf.st_mode );
#  endif
            }
         }
#endif /* HB_OS_UNIX */

         pFile = hb_fileNew( hFile, fShared, iMode, device, inode, HB_TRUE );
         if( pFile->hFile != hFile )
         {
            if( pFile->mode != FO_READWRITE && iMode == FO_READWRITE )
            {
               HB_FHANDLE hTemp = pFile->hFileRO;
               pFile->hFileRO = pFile->hFile;
               pFile->hFile = hFile;
               pFile->mode = iMode;
               hFile = hTemp;
            }

            if( ! fShared || ! pFile->shared || pFile->mode != FO_READWRITE )
            {
               fResult = HB_FALSE;
               if( pFile->hFileRO == FS_ERROR && pFile->uiLocks != 0 )
               {
                  pFile->hFileRO = hFile;
                  hFile = FS_ERROR;
               }
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
                  /* FIXME: possible race condition */
                  hb_fsLockLarge( pFile->hFile, HB_SHARELOCK_POS, HB_SHARELOCK_SIZE,
                                  FL_LOCK | FLX_SHARED );
#endif
               }
            }
            if( !fResult )
            {
               if( pFile )
               {
                  --pFile->used;
                  pFile = NULL;
               }
               if( hFile != FS_ERROR )
               {
                  /* FIXME: possible race condition in MT mode,
                   *        close() is not safe due to existing locks
                   *        which are removed.
                   */
                  hb_fsClose( hFile );
               }
            }
         }
#if ! defined( HB_OS_UNIX )
         hb_threadLeaveCriticalSection( &s_fileMtx );
#endif
      }
   }

#if defined( HB_OS_UNIX )
   hb_threadLeaveCriticalSection( &s_fileMtx );
   if( pFile && fSeek )
      pFile = hb_fileposNew( pFile );
#endif

   if( ! fResult )
      hb_fsSetError( ( nExFlags & FXO_TRUNCATE ) ? 5 : 32 );
   if( ( nExFlags & FXO_COPYNAME ) != 0 && pFile )
      hb_strncpy( ( char * ) HB_UNCONST( pszFileName ), pszFile, HB_PATH_MAX - 1 );
   if( pError )
   {
      hb_errPutFileName( pError, pszFile );
      if( ! fResult )
      {
         hb_errPutOsCode( pError, hb_fsError() );
         hb_errPutGenCode( pError, ( HB_ERRCODE ) ( ( nExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
   }

   hb_xfree( pszFile );

   hb_vmLock();

   return pFile;
}

static void s_fileClose( PHB_FILE pFile )
{
   hb_vmUnlock();
   hb_fsSetError( 0 );
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
      if( pFile->hFile != FS_ERROR )
         hb_fsClose( pFile->hFile );
      if( pFile->hFileRO != FS_ERROR )
         hb_fsClose( pFile->hFileRO );

      if( pFile->pLocks )
         hb_xfree( pFile->pLocks );

      hb_xfree( pFile );
   }

   hb_threadLeaveCriticalSection( &s_fileMtx );
   hb_vmLock();
}

static HB_BOOL s_fileLock( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen,
                           int iType )
{
   HB_BOOL fResult, fLockFS = HB_FALSE;

   hb_vmUnlock();
   if( ( iType & FL_MASK ) == FL_UNLOCK )
   {
      hb_threadEnterCriticalSection( &s_lockMtx );
      fResult = hb_fileUnlock( pFile, &fLockFS, nStart, nLen );
      hb_threadLeaveCriticalSection( &s_lockMtx );
      if( fLockFS )
      {
         hb_fsLockLarge( pFile->hFile, nStart, nLen, ( HB_USHORT ) iType );
         hb_threadEnterCriticalSection( &s_lockMtx );
         hb_fileUnlock( pFile, NULL, nStart, nLen );
         hb_threadLeaveCriticalSection( &s_lockMtx );
      }
      else
         hb_fsSetError( fResult ? 0 : 33 );
   }
   else
   {
      hb_threadEnterCriticalSection( &s_lockMtx );
      fResult = hb_fileSetLock( pFile, &fLockFS, nStart, nLen );
      hb_threadLeaveCriticalSection( &s_lockMtx );
      if( fLockFS )
      {
#if defined( HB_OS_UNIX )
         if( pFile->mode == FO_READ )
            iType |= FLX_SHARED;
         else if( pFile->mode == FO_WRITE )
            iType &= ~FLX_SHARED;
#endif
         fResult = hb_fsLockLarge( pFile->hFile, nStart, nLen, ( HB_USHORT ) iType );
         if( ! fResult )
         {
            hb_threadEnterCriticalSection( &s_lockMtx );
            hb_fileUnlock( pFile, NULL, nStart, nLen );
            hb_threadLeaveCriticalSection( &s_lockMtx );
         }
      }
      else
         hb_fsSetError( fResult ? 0 : 33 );
   }
   hb_vmLock();

   return fResult;
}

static int s_fileLockTest( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen,
                           int iType )
{
   HB_BOOL fLocked;
   int iResult;

   hb_vmUnlock();

   hb_threadEnterCriticalSection( &s_lockMtx );
   fLocked = hb_fileTestLock( pFile, nStart, nLen );
   hb_threadLeaveCriticalSection( &s_lockMtx );
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

   hb_vmLock();

   return iResult;
}

static HB_SIZE s_fileRead( PHB_FILE pFile, void * buffer, HB_SIZE nSize,
                           HB_MAXINT nTimeout )
{
   HB_SYMBOL_UNUSED( nTimeout );
   return hb_fsReadLarge( pFile->hFile, buffer, nSize );
}

static HB_SIZE s_fileWrite( PHB_FILE pFile, const void * buffer, HB_SIZE nSize,
                            HB_MAXINT nTimeout )
{
   HB_SYMBOL_UNUSED( nTimeout );
   return hb_fsWriteLarge( pFile->hFile, buffer, nSize );
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

static HB_FOFFSET s_fileSeek( PHB_FILE pFile, HB_FOFFSET nOffset,
                              HB_USHORT uiFlags )
{
   return hb_fsSeekLarge( pFile->hFile, nOffset, uiFlags );
}

static HB_FOFFSET s_fileSize( PHB_FILE pFile )
{
   return hb_fsGetSize( pFile->hFile );
}

static HB_BOOL s_fileEof( PHB_FILE pFile )
{
   return hb_fsEof( pFile->hFile );
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

static HB_BOOL s_fileConfigure( PHB_FILE pFile, int iIndex, PHB_ITEM pValue )
{
   HB_SYMBOL_UNUSED( pFile );

   switch( iIndex )
   {
      case HB_VF_IONAME:
         hb_itemPutC( pValue, "FILE:" );
         return HB_TRUE;
   }

   return HB_FALSE;
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

      s_fileExists,
      s_fileDelete,
      s_fileRename,
      s_fileCopy,

      s_fileDirExists,
      s_fileDirMake,
      s_fileDirRemove,
      s_fileDirSpace,
      s_fileDirectory,

      s_fileTimeGet,
      s_fileTimeSet,
      s_fileAttrGet,
      s_fileAttrSet,

      s_fileLink,
      s_fileLinkSym,
      s_fileLinkRead,

      s_fileExtOpen,
      s_fileClose,
      s_fileLock,
      s_fileLockTest,
      s_fileRead,
      s_fileWrite,
      s_fileReadAt,
      s_fileWriteAt,
      s_fileTruncAt,
      s_fileSeek,
      s_fileSize,
      s_fileEof,
      s_fileFlush,
      s_fileCommit,
      s_fileConfigure,
      s_fileHandle
   };

   return &s_fileFuncs;
}


#if defined( HB_OS_UNIX )

typedef struct
{
   const HB_FILE_FUNCS * pFuncs;
   PHB_FILE       pFile;
   HB_FOFFSET     seek_pos;
}
HB_FILEPOS, * PHB_FILEPOS;

#define _PHB_FILEPOS  ( ( PHB_FILEPOS ) pFilePos )
#define _PHB_FILE     _PHB_FILEPOS->pFile

static void s_fileposClose( PHB_FILE pFilePos )
{
   _PHB_FILE->pFuncs->Close( _PHB_FILE );
   hb_xfree( pFilePos );
}

static HB_BOOL s_fileposLock( PHB_FILE pFilePos, HB_FOFFSET nStart, HB_FOFFSET nLen,
                              int iType )
{
   return _PHB_FILE->pFuncs->Lock( _PHB_FILE, nStart, nLen, iType );
}

static int s_fileposLockTest( PHB_FILE pFilePos, HB_FOFFSET nStart, HB_FOFFSET nLen,
                              int iType )
{
   return _PHB_FILE->pFuncs->LockTest( _PHB_FILE, nStart, nLen, iType );
}

static HB_SIZE s_fileposRead( PHB_FILE pFilePos, void * buffer, HB_SIZE nSize,
                              HB_MAXINT nTimeout )
{
   HB_SIZE nDone;

   HB_SYMBOL_UNUSED( nTimeout );
   nDone = _PHB_FILE->pFuncs->ReadAt( _PHB_FILE, buffer, nSize, _PHB_FILEPOS->seek_pos );
   _PHB_FILEPOS->seek_pos += nDone;

   return nDone;
}

static HB_SIZE s_fileposWrite( PHB_FILE pFilePos, const void * buffer, HB_SIZE nSize,
                               HB_MAXINT nTimeout )
{
   HB_SIZE nDone;

   HB_SYMBOL_UNUSED( nTimeout );
   nDone = _PHB_FILE->pFuncs->WriteAt( _PHB_FILE, buffer, nSize, _PHB_FILEPOS->seek_pos );
   _PHB_FILEPOS->seek_pos += nDone;

   return nDone;
}

static HB_SIZE s_fileposReadAt( PHB_FILE pFilePos, void * buffer, HB_SIZE nSize,
                                HB_FOFFSET nOffset )
{
   return _PHB_FILE->pFuncs->ReadAt( _PHB_FILE, buffer, nSize, nOffset );
}

static HB_SIZE s_fileposWriteAt( PHB_FILE pFilePos, const void * buffer, HB_SIZE nSize,
                                 HB_FOFFSET nOffset )
{
   return _PHB_FILE->pFuncs->WriteAt( _PHB_FILE, buffer, nSize, nOffset );
}

static HB_BOOL s_fileposTruncAt( PHB_FILE pFilePos, HB_FOFFSET nOffset )
{
   if( _PHB_FILE->pFuncs->TruncAt( _PHB_FILE, nOffset ) )
   {
      _PHB_FILEPOS->seek_pos = nOffset;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static HB_FOFFSET s_fileposSeek( PHB_FILE pFilePos, HB_FOFFSET nOffset,
                                 HB_USHORT uiFlags )
{
   if( uiFlags & FS_END )
      nOffset += pFilePos->pFuncs->Size( pFilePos );
   else if( uiFlags & FS_RELATIVE )
      nOffset += _PHB_FILEPOS->seek_pos;
   /* else FS_SET */

   if( nOffset >= 0 )
   {
      _PHB_FILEPOS->seek_pos = nOffset;
      hb_fsSetError( 0 );
   }
   else
      hb_fsSetError( 25 ); /* 'Seek Error' */

   return _PHB_FILEPOS->seek_pos;
}

static HB_FOFFSET s_fileposSize( PHB_FILE pFilePos )
{
   return _PHB_FILE->pFuncs->Size( _PHB_FILE );
}

static HB_BOOL s_fileposEof( PHB_FILE pFilePos )
{
   return _PHB_FILEPOS->seek_pos >= pFilePos->pFuncs->Size( pFilePos );
}

static void s_fileposFlush( PHB_FILE pFilePos, HB_BOOL fDirty )
{
   _PHB_FILE->pFuncs->Flush( _PHB_FILE, fDirty );
}

static void s_fileposCommit( PHB_FILE pFilePos )
{
   _PHB_FILE->pFuncs->Commit( _PHB_FILE );
}

static HB_BOOL s_fileposConfigure( PHB_FILE pFilePos, int iIndex, PHB_ITEM pValue )
{
   return _PHB_FILE->pFuncs->Configure( _PHB_FILE, iIndex, pValue );
}

static HB_FHANDLE s_fileposHandle( PHB_FILE pFilePos )
{
   return pFilePos ? _PHB_FILE->pFuncs->Handle( _PHB_FILE ) : FS_ERROR;
}

static const HB_FILE_FUNCS * s_fileposMethods( void )
{
   /* methods table */
   static const HB_FILE_FUNCS s_fileFuncs =
   {
      s_fileAccept,

      s_fileExists,
      s_fileDelete,
      s_fileRename,
      s_fileCopy,

      s_fileDirExists,
      s_fileDirMake,
      s_fileDirRemove,
      s_fileDirSpace,
      s_fileDirectory,

      s_fileTimeGet,
      s_fileTimeSet,
      s_fileAttrGet,
      s_fileAttrSet,

      s_fileLink,
      s_fileLinkSym,
      s_fileLinkRead,

      s_fileExtOpen,
      s_fileposClose,
      s_fileposLock,
      s_fileposLockTest,
      s_fileposRead,
      s_fileposWrite,
      s_fileposReadAt,
      s_fileposWriteAt,
      s_fileposTruncAt,
      s_fileposSeek,
      s_fileposSize,
      s_fileposEof,
      s_fileposFlush,
      s_fileposCommit,
      s_fileposConfigure,
      s_fileposHandle
   };

   return &s_fileFuncs;
}

static PHB_FILE hb_fileposNew( PHB_FILE pFile )
{
   PHB_FILEPOS pFilePos = ( PHB_FILEPOS ) hb_xgrabz( sizeof( HB_FILEPOS ) );

   pFilePos->pFuncs   = s_fileposMethods();
   pFilePos->pFile    = pFile;
   pFilePos->seek_pos = 0;

   return ( PHB_FILE ) pFilePos;
}

#endif /* HB_OS_UNIX */

static const HB_FILE_FUNCS * s_pFileTypes[ HB_FILE_TYPE_MAX ];
static int s_iFileTypes = 0;

static int s_fileFindDrv( const char * pszFileName )
{
   int i = -1;

   if( pszFileName )
   {
      i = s_iFileTypes;

      while( --i >= 0 )
      {
         if( s_pFileTypes[ i ]->Accept( s_pFileTypes[ i ], pszFileName ) )
            break;
      }
   }

   return i;
}


/*
 * public API functions
 */

HB_BOOL hb_fileRegisterFull( const HB_FILE_FUNCS * pFuncs )
{
   HB_BOOL fResult = HB_FALSE;

   hb_vmUnlock();
   hb_threadEnterCriticalSection( &s_lockMtx );

   if( s_iFileTypes < HB_FILE_TYPE_MAX )
   {
      s_pFileTypes[ s_iFileTypes ] = pFuncs;
      s_iFileTypes++;
      fResult = HB_TRUE;
   }

   hb_threadLeaveCriticalSection( &s_lockMtx );
   hb_vmLock();

   return fResult;
}

HB_BOOL hb_fileExists( const char * pszFileName, char * pRetPath )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Exists( s_pFileTypes[ i ], pszFileName, pRetPath );

   return pRetPath ? hb_spFileExists( pszFileName, pRetPath ) :
                     hb_fsFileExists( pszFileName );
}

HB_BOOL hb_fileDelete( const char * pszFileName )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Delete( s_pFileTypes[ i ], pszFileName );

   return hb_fsDelete( pszFileName );
}

HB_BOOL hb_fileRename( const char * pszFileName, const char * pszNewName )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Rename( s_pFileTypes[ i ], pszFileName, pszNewName );

   return hb_fsRename( pszFileName, pszNewName );
}

HB_BOOL hb_fileCopy( const char * pszSrcFile, const char * pszDstFile )
{
   int i = s_fileFindDrv( pszSrcFile );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Copy( s_pFileTypes[ i ], pszSrcFile, pszDstFile );

   return hb_fsCopy( pszSrcFile, pszDstFile );
}

HB_BOOL hb_fileMove( const char * pszSrcFile, const char * pszDstFile )
{
   int iS = s_fileFindDrv( pszSrcFile ),
       iD = s_fileFindDrv( pszDstFile );

   if( iS == iD )
   {
      if( iS >= 0 ?
          s_pFileTypes[ iS ]->Rename( s_pFileTypes[ iS ], pszSrcFile, pszDstFile ) :
          hb_fsRename( pszSrcFile, pszDstFile ) )
         return HB_TRUE;
   }

   return hb_fsCopy( pszSrcFile, pszDstFile ) &&
          hb_fileDelete( pszSrcFile );
}

HB_BOOL hb_fileDirExists( const char * pszDirName )
{
   int i = s_fileFindDrv( pszDirName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->DirExists( s_pFileTypes[ i ], pszDirName );

   return hb_fsDirExists( pszDirName );
}

HB_BOOL hb_fileDirMake( const char * pszDirName )
{
   int i = s_fileFindDrv( pszDirName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->DirMake( s_pFileTypes[ i ], pszDirName );

   return hb_fsMkDir( pszDirName );
}

HB_BOOL hb_fileDirRemove( const char * pszDirName )
{
   int i = s_fileFindDrv( pszDirName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->DirRemove( s_pFileTypes[ i ], pszDirName );

   return hb_fsRmDir( pszDirName );
}

double hb_fileDirSpace( const char * pszDirName, HB_USHORT uiType )
{
   int i = s_fileFindDrv( pszDirName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->DirSpace( s_pFileTypes[ i ], pszDirName, uiType );

   return hb_fsDiskSpace( pszDirName, uiType );
}

PHB_ITEM hb_fileDirectory( const char * pszDirSpec, const char * pszAttr )
{
   int i = s_fileFindDrv( pszDirSpec );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Directory( s_pFileTypes[ i ], pszDirSpec, pszAttr );

   return hb_fsDirectory( pszDirSpec, pszAttr, HB_TRUE );
}

HB_BOOL hb_fileTimeGet( const char * pszFileName, long * plJulian, long * plMillisec )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->TimeGet( s_pFileTypes[ i ], pszFileName, plJulian, plMillisec );

   return hb_fsGetFileTime( pszFileName, plJulian, plMillisec );
}

HB_BOOL hb_fileTimeSet( const char * pszFileName, long lJulian, long lMillisec )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->TimeSet( s_pFileTypes[ i ], pszFileName, lJulian, lMillisec );

   return hb_fsSetFileTime( pszFileName, lJulian, lMillisec );
}

HB_FOFFSET hb_fileSizeGet( const char * pszFileName, HB_BOOL bUseDirEntry )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
   {
      HB_FOFFSET nSize = 0;

      if( bUseDirEntry )
      {
         PHB_ITEM pDir = hb_fileDirectory( pszFileName, "HS" );

         if( pDir )
         {
            PHB_ITEM pEntry = hb_arrayGetItemPtr( pDir, 1 );

            if( pEntry )
               nSize = hb_arrayGetNInt( pEntry, F_SIZE );
            hb_itemRelease( pDir );
         }
      }
      else
      {
         PHB_FILE pFile = hb_fileExtOpen( pszFileName, NULL, FO_READ | FO_COMPAT, NULL, NULL );
         if( pFile )
         {
            HB_ERRCODE uiError;
            nSize = hb_fileSize( pFile );
            uiError = hb_fsError();
            hb_fileClose( pFile );
            hb_fsSetError( uiError );
         }
      }

      return nSize;
   }

   return hb_fsFSize( pszFileName, bUseDirEntry );
}

HB_BOOL hb_fileAttrGet( const char * pszFileName, HB_FATTR * pulAttr )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->AttrGet( s_pFileTypes[ i ], pszFileName, pulAttr );

   return hb_fsGetAttr( pszFileName, pulAttr );
}

HB_BOOL hb_fileAttrSet( const char * pszFileName, HB_FATTR ulAttr )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->AttrSet( s_pFileTypes[ i ], pszFileName, ulAttr );

   return hb_fsSetAttr( pszFileName, ulAttr );
}

HB_BOOL hb_fileLink( const char * pszExisting, const char * pszNewName )
{
   int i = s_fileFindDrv( pszExisting );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Link( s_pFileTypes[ i ], pszExisting, pszNewName );

   return hb_fsLink( pszExisting, pszNewName );
}

HB_BOOL hb_fileLinkSym( const char * pszTarget, const char * pszNewName )
{
   int i = s_fileFindDrv( pszTarget );

   if( i >= 0 )
      return s_pFileTypes[ i ]->LinkSym( s_pFileTypes[ i ], pszTarget, pszNewName );

   return hb_fsLinkSym( pszTarget, pszNewName );
}

char * hb_fileLinkRead( const char * pszFileName )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->LinkRead( s_pFileTypes[ i ], pszFileName );

   return hb_fsLinkRead( pszFileName );
}

PHB_FILE hb_fileExtOpen( const char * pszFileName, const char * pDefExt,
                         HB_FATTR nExFlags, const char * pPaths,
                         PHB_ITEM pError )
{
   int i = s_fileFindDrv( pszFileName );

   if( i >= 0 )
      return s_pFileTypes[ i ]->Open( s_pFileTypes[ i ], pszFileName, pDefExt, nExFlags, pPaths, pError );

   return s_fileExtOpen( NULL, pszFileName, pDefExt, nExFlags, pPaths, pError );
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

HB_SIZE hb_fileRead( PHB_FILE pFile, void * buffer, HB_SIZE nSize,
                     HB_MAXINT nTimeout )
{
   return pFile->pFuncs->Read( pFile, buffer, nSize, nTimeout );
}

HB_SIZE hb_fileWrite( PHB_FILE pFile, const void * buffer, HB_SIZE nSize,
                      HB_MAXINT nTimeout )
{
   return pFile->pFuncs->Write( pFile, buffer, nSize, nTimeout );
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

HB_FOFFSET hb_fileSeek( PHB_FILE pFile, HB_FOFFSET nOffset, HB_USHORT uiFlags )
{
   return pFile->pFuncs->Seek( pFile, nOffset, uiFlags );
}

HB_FOFFSET hb_fileSize( PHB_FILE pFile )
{
   return pFile->pFuncs->Size( pFile );
}

HB_BOOL hb_fileEof( PHB_FILE pFile )
{
   return pFile->pFuncs->Eof( pFile );
}

void hb_fileFlush( PHB_FILE pFile, HB_BOOL fDirty )
{
   pFile->pFuncs->Flush( pFile, fDirty );
}

void hb_fileCommit( PHB_FILE pFile )
{
   pFile->pFuncs->Commit( pFile );
}

HB_BOOL hb_fileConfigure( PHB_FILE pFile, int iIndex, PHB_ITEM pValue )
{
   return pFile->pFuncs->Configure( pFile, iIndex, pValue );
}

HB_FHANDLE hb_fileHandle( PHB_FILE pFile )
{
   return pFile->pFuncs->Handle( pFile );
}

/* internal FILE structures only */

PHB_FILE hb_fileCreateTemp( const char * pszDir,
                            const char * pszPrefix,
                            HB_FATTR ulAttr,
                            char * pszName )
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

PHB_FILE hb_fileFromHandle( HB_FHANDLE hFile )
{
   return hb_fileNew( hFile, HB_FALSE, HB_FALSE, 0, 0, HB_FALSE );
}

HB_BOOL hb_fileDetach( PHB_FILE pFile )
{
   if( pFile )
   {
      if( pFile->pFuncs == s_fileMethods() )
      {
         pFile->hFile = FS_ERROR;
         s_fileClose( pFile );
         return HB_TRUE;
      }
#if defined( HB_OS_UNIX )
      else if( pFile->pFuncs == s_fileposMethods() )
      {
         PHB_FILEPOS pFilePos = ( PHB_FILEPOS ) pFile;

         pFilePos->pFile->hFile = FS_ERROR;
         s_fileposClose( pFile );
         return HB_TRUE;
      }
#endif
   }

   return HB_FALSE;
}

HB_BOOL hb_fileIsLocal( PHB_FILE pFile )
{
   if( pFile )
   {
#if defined( HB_OS_UNIX )
      if( pFile->pFuncs == s_fileMethods() ||
          pFile->pFuncs == s_fileposMethods() )
#else
      if( pFile->pFuncs == s_fileMethods() )
#endif
         return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_fileIsLocalName( const char * pszFileName )
{
   return s_fileFindDrv( pszFileName ) < 0;
}

PHB_FILE hb_filePOpen( const char * pszFileName, const char * pszMode )
{
   PHB_FILE pFile = NULL;
   HB_FHANDLE hFile;

   hFile = hb_fsPOpen( pszFileName, pszMode );
   if( hFile != FS_ERROR )
      pFile = hb_fileNew( hFile, HB_FALSE, HB_FALSE, 0, 0, HB_FALSE );

   return pFile;
}

HB_SIZE hb_fileResult( HB_SIZE nSize )
{
   return nSize == ( HB_SIZE ) FS_ERROR ? 0 : nSize;
}

#define HB_FILELOAD_BUFFERSIZE  65536

HB_BYTE * hb_fileLoadData( PHB_FILE pFile, HB_SIZE nMaxSize,
                           HB_SIZE * pnSize )
{
   HB_BYTE * pFileBuf = NULL;
   HB_SIZE nSize = 0, nRead, nBufSize;
   HB_FOFFSET nFileSize = hb_fileSize( pFile );

   if( nFileSize == FS_ERROR ||
       ( nFileSize == 0 && hb_fsError() != 0 ) )
   {
      for( nBufSize = 0;; )
      {
         if( nBufSize == nSize )
         {
            nBufSize += nBufSize == 0 ? HB_FILELOAD_BUFFERSIZE : nBufSize >> 1;
            if( nMaxSize > 0 && nBufSize > nMaxSize )
            {
               nBufSize = nMaxSize;
               if( nBufSize == nSize )
                  break;
            }
            pFileBuf = ( HB_BYTE * ) hb_xrealloc( pFileBuf, nBufSize );
         }
         nRead = hb_fileRead( pFile, pFileBuf + nSize, nBufSize - nSize, -1 );
         if( nRead == 0 || nRead == ( HB_SIZE ) FS_ERROR )
            break;
         nSize += nRead;
      }
   }
   else if( nFileSize > 0 )
   {
      nBufSize = ( HB_SIZE ) nFileSize;
      if( nMaxSize > 0 && nBufSize > nMaxSize )
         nBufSize = nMaxSize;

      pFileBuf = ( HB_BYTE * ) hb_xgrab( nBufSize + 1 );
      do
      {
         nRead = hb_fileReadAt( pFile, pFileBuf + nSize, nBufSize - nSize, nSize );
         if( nRead == 0 || nRead == ( HB_SIZE ) FS_ERROR )
            break;
         nSize += nRead;
      }
      while( nSize < nBufSize );
   }

   if( nSize > 0 )
   {
      pFileBuf = ( HB_BYTE * ) hb_xrealloc( pFileBuf, nSize + 1 );
      pFileBuf[ nSize ] = '\0';
   }
   else if( pFileBuf )
   {
      hb_xfree( pFileBuf );
      pFileBuf = NULL;
   }

   if( pnSize )
      *pnSize = nSize;

   return pFileBuf;
}

HB_BYTE * hb_fileLoad( const char * pszFileName, HB_SIZE nMaxSize,
                       HB_SIZE * pnSize )
{
   HB_BYTE * pFileBuf = NULL;
   PHB_FILE pFile = hb_fileExtOpen( pszFileName, NULL,
                                    FO_READ | FO_SHARED | FO_PRIVATE |
                                    FXO_SHARELOCK | FXO_NOSEEKPOS,
                                    NULL, NULL );

   if( pFile != NULL )
   {
      pFileBuf = hb_fileLoadData( pFile, nMaxSize, pnSize );
      hb_fileClose( pFile );
   }
   else if( pnSize )
      *pnSize = 0;

   return pFileBuf;
}
