/*                              
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Memory file system
 *   I/O driver for Memory file system
 *
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#define _HB_FILE_IMPLEMENTATION_

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbthread.h"
#include "hbinit.h"

/******************************************************
*
*  Memory file system
*
*******************************************************/

/* change this define for public hb_memfs*() API */
#ifdef HB_MEMFS_PUBLIC_API
#define HB_MEMFS_EXPORT
#else
#define HB_MEMFS_EXPORT static
#endif

#define HB_MEMFS_INITSIZE  16

/* File access flags */
#define FOX_READ        1
#define FOX_WRITE       2
#define FOX_READWRITE   3

/* File sharing flags */
#define FOX_DENYNONE    0
#define FOX_DENYREAD   16
#define FOX_DENYWRITE  32
#define FOX_EXCLUSIVE  48
#define FOX_DENYFLAGS  48


typedef struct _HB_MEMFS_INODE
{
   HB_FOFFSET     llSize;
   HB_FOFFSET     llAlloc;
   char *         pData;
   char *         szName;
   unsigned int   uiCount;
   unsigned int   uiCountRead, uiCountWrite;
   USHORT         uiDeny;
} HB_MEMFS_INODE, * PHB_MEMFS_INODE;


typedef struct _HB_MEMFS_FILE
{
   PHB_MEMFS_INODE  pInode;
   HB_FOFFSET       llPos;
   USHORT           uiFlags;
} HB_MEMFS_FILE, * PHB_MEMFS_FILE;


typedef struct _HB_MEMFS_FS
{
   ULONG              ulCount;
   ULONG              ulAlloc;
   PHB_MEMFS_INODE *  pInodes;
} HB_MEMFS_FS, * PHB_MEMFS_FS;


static HB_MEMFS_FS s_fs;
static USHORT      s_error;

static HB_CRITICAL_NEW( s_mtx );

#define HB_MEMFSMT_LOCK        hb_threadEnterCriticalSection( &s_mtx );
#define HB_MEMFSMT_UNLOCK      hb_threadLeaveCriticalSection( &s_mtx );


static void memfsInit( void )
{
   /* HB_CRITICAL_INIT( s_mtx ); */
   s_error = 0;
   s_fs.ulCount = 0;
   s_fs.ulAlloc = HB_MEMFS_INITSIZE;
   s_fs.pInodes = ( PHB_MEMFS_INODE * ) hb_xgrab( sizeof( PHB_MEMFS_INODE ) * s_fs.ulAlloc );
}


/* Note: returns 1 based index! */
static ULONG memfsInodeFind( const char * szName, ULONG * pulPos )
{
   ULONG   ulLeft, ulRight, ulMiddle;
   int     i;

   ulLeft = 0;
   ulRight = s_fs.ulCount;
   while( ulLeft < ulRight )
   {
      ulMiddle = ( ulLeft + ulRight ) >> 1;
      i = strcmp( szName, s_fs.pInodes[ ulMiddle ]->szName );
      if( i == 0 )
         return ulMiddle + 1;
      else if( i < 0 )
         ulRight = ulMiddle;
      else
         ulLeft = ulMiddle + 1;
   }
   if( pulPos )
      *pulPos = ulLeft;
   return 0;
}


static PHB_MEMFS_INODE memfsInodeAlloc( const char* szName )
{
   PHB_MEMFS_INODE  pInode = hb_xgrab( sizeof( HB_MEMFS_INODE ) );
   ULONG            ulPos;

   pInode->llSize = 0;
   pInode->llAlloc = HB_MEMFS_INITSIZE;
   pInode->pData = hb_xgrab( pInode->llAlloc );
   memset( pInode->pData, 0, pInode->llAlloc );
   pInode->szName = hb_strdup( szName );
   
   pInode->uiCount = 1; 

   /* Insert into inode array. Inode should not exist!!! */
   if( s_fs.ulCount >= s_fs.ulAlloc )
   {
      s_fs.ulAlloc += s_fs.ulAlloc >> 1;
      s_fs.pInodes = ( PHB_MEMFS_INODE * ) hb_xrealloc( s_fs.pInodes, s_fs.ulAlloc * sizeof( PHB_MEMFS_INODE ) );
   }

   if( memfsInodeFind( szName, &ulPos ) )
   {
      hb_errInternal( 9999, "memfsInodeAlloc: Inode already exists", NULL, NULL );
      return NULL;
   }

   if( ulPos < s_fs.ulCount )
   {
      memmove( s_fs.pInodes + ulPos + 1, s_fs.pInodes + ulPos, ( s_fs.ulCount - ulPos ) * sizeof( PHB_MEMFS_INODE ) );
   }
   s_fs.pInodes[ ulPos ] = pInode;
   s_fs.ulCount++;
   return pInode;
}


static void memfsInodeFree( PHB_MEMFS_INODE pInode )
{
   hb_xfree( pInode->pData );
   hb_xfree( pInode->szName );
   hb_xfree( pInode );
}


static PHB_MEMFS_FILE memfsFileAlloc( PHB_MEMFS_INODE pInode )
{
   PHB_MEMFS_FILE  pFile = hb_xgrab( sizeof( HB_MEMFS_FILE ) );

   pFile->pInode = pInode;
   pFile->llPos = 0;

   pInode->uiCount++;
   return pFile;
}


/* ======== Public Memory FS functions ======== */

HB_MEMFS_EXPORT USHORT hb_memfsError( void )
{
   return s_error;
}


HB_MEMFS_EXPORT BOOL hb_memfsFileExists( const char * szName )
{
   BOOL  bRet;

   HB_MEMFSMT_LOCK
   bRet = memfsInodeFind( szName, NULL ) != 0;
   HB_MEMFSMT_UNLOCK
   return bRet;
}


HB_MEMFS_EXPORT BOOL hb_memfsDelete( const char * szName )
{
   PHB_MEMFS_INODE  pInode;
   ULONG            ulFile;

   HB_MEMFSMT_LOCK
   if( ( ulFile = memfsInodeFind( szName, NULL ) ) != 0 )
   {
      HB_MEMFSMT_UNLOCK
      return 0;
   }
   pInode = s_fs.pInodes[ ulFile - 1 ];

   if( ulFile < s_fs.ulCount )
      memmove( s_fs.pInodes + ulFile - 1, s_fs.pInodes + ulFile, ( s_fs.ulCount - ulFile ) * sizeof( PHB_MEMFS_INODE ) );

   s_fs.ulCount--;

   if( --pInode->uiCount == 0 )
      memfsInodeFree( pInode );
   HB_MEMFSMT_UNLOCK
   return 1;
}


HB_MEMFS_EXPORT BOOL hb_memfsRename( const char * szName, const char * szNewName )
{
   ULONG  ulPos;

   HB_MEMFSMT_LOCK
   if( ( ulPos = memfsInodeFind( szName, NULL ) ) == 0 )
   {
      HB_MEMFSMT_UNLOCK
      /* File not found */
      return 0;
   }
   if( memfsInodeFind( szNewName, NULL ) )
   {
      HB_MEMFSMT_UNLOCK
      /* File already exists */
      return 0;
   }
   hb_xfree( s_fs.pInodes[ ulPos - 1 ]->szName );
   s_fs.pInodes[ ulPos - 1 ]->szName = hb_strdup( szNewName );
   HB_MEMFSMT_UNLOCK
   return 1;
}


HB_MEMFS_EXPORT HB_FHANDLE hb_memfsOpen( const char * szName, USHORT uiFlags )
{
   PHB_MEMFS_FILE  pFile;
   ULONG           ulPos;
   USHORT          uiError = 0;

   /* printf( "\nhb_memfsOpen(\"%s\", 0x%04X)\n", szName, uiFlags ); */

   /* 
     Recalculate flags. Bit should indicate feature: 1=read, 2=write, 16=denyread, 32=denywrite.
     So, 3=readwrite, 48=exclusive.
     Compatibility mode == DenyNone.
   */
   uiFlags = ( uiFlags & ( FO_CREAT | FO_TRUNC | FO_EXCL ) ) |
             ( uiFlags & FO_READWRITE ? FOX_READWRITE : ( uiFlags & FO_WRITE ? FOX_WRITE : FOX_READ ) ) |
             ( ( uiFlags & 0xf0 ) == FO_EXCLUSIVE ? FOX_EXCLUSIVE : 
               ( ( uiFlags & 0xf0 ) == FO_DENYWRITE ? FOX_DENYWRITE : 
                 ( ( uiFlags & 0xf0 ) == FO_DENYREAD ? FOX_DENYREAD : FOX_DENYNONE ) ) );

   HB_MEMFSMT_LOCK

   ulPos = memfsInodeFind( szName, NULL );

   if( uiFlags & FO_CREAT )
   {
      if( uiFlags & FO_EXCL )
      {
         /* create new */
         if( ulPos )
            uiError = 80; 
      }
      else if( uiFlags & FO_TRUNC )
      {
         /* create always */
         if( ulPos && s_fs.pInodes[ ulPos - 1 ]->uiDeny & FOX_DENYWRITE )
            uiError = 32;
      }
      else
      {
         /* open always */
         /* Do nothing */
      }
   }
   else if( uiFlags & FO_TRUNC )
   {
      /* truncate existing */
      if( ulPos )
         if( s_fs.pInodes[ ulPos - 1 ]->uiDeny & FOX_DENYWRITE )
            uiError = 32;
      else
         uiError = 2;
   }
   else
   {
      /* open existing */
      if( ! ulPos )
         uiError = 2;
   }

   if( ! uiError )
   {
      if( ulPos )
      {
         if( uiFlags & FOX_READ && s_fs.pInodes[ ulPos - 1 ]->uiDeny & FOX_DENYREAD ||
             uiFlags & FOX_WRITE && s_fs.pInodes[ ulPos - 1 ]->uiDeny & FOX_DENYWRITE ||
             s_fs.pInodes[ ulPos - 1 ]->uiDeny & uiFlags )
         {
            uiError = 32;
         }
         else 
         {
            pFile = memfsFileAlloc( s_fs.pInodes[ ulPos - 1 ] );
         }
      }
      else
      {
         pFile = memfsFileAlloc( memfsInodeAlloc( szName ) );
      }
   }

   s_error = uiError;
   if( uiError )
   {
      HB_MEMFSMT_UNLOCK
      return FS_ERROR;
   }
   pFile->pInode->uiDeny |= uiFlags & FOX_DENYFLAGS;
   if( uiFlags & FOX_READ )
      pFile->pInode->uiCountRead++;
   if( uiFlags & FOX_WRITE )
      pFile->pInode->uiCountWrite++;
   
   HB_MEMFSMT_UNLOCK

   pFile->uiFlags = uiFlags;
   return ( HB_FHANDLE ) pFile;
}


HB_MEMFS_EXPORT void hb_memfsClose( HB_FHANDLE pFile )
{
   PHB_MEMFS_INODE  pInode = ( ( PHB_MEMFS_FILE ) pFile )->pInode;

   HB_MEMFSMT_LOCK
   if( --pInode->uiCount == 0 )
   {
      memfsInodeFree( pInode );
   }
   else
   {
      if( ( ( PHB_MEMFS_FILE ) pFile )->uiFlags & FOX_READ )
         pInode->uiCountRead--;
      if( ( ( PHB_MEMFS_FILE ) pFile )->uiFlags & FOX_WRITE )
         pInode->uiCountWrite--;
      pInode->uiDeny ^= ( ( PHB_MEMFS_FILE ) pFile )->uiFlags & FOX_DENYFLAGS;
   }
   HB_MEMFSMT_UNLOCK
   hb_xfree( ( void * ) pFile );
}


HB_MEMFS_EXPORT ULONG hb_memfsReadAt( HB_FHANDLE pFile, void * pBuff, ULONG ulCount, HB_FOFFSET llOffset )
{
   PHB_MEMFS_INODE  pInode = ( ( PHB_MEMFS_FILE ) pFile )->pInode;
   ULONG            ulRead;

   if( ( ( ( PHB_MEMFS_FILE ) pFile )->uiFlags & FOX_READ ) == 0 )
      return 0; /* access denied */

   if( llOffset < 0 || pInode->llSize <= llOffset )
      return 0;

   HB_MEMFSMT_LOCK
   if( pInode->llSize >= llOffset + ulCount )
      ulRead = ulCount;
   else
      ulRead = pInode->llSize - llOffset;

   memcpy( pBuff, pInode->pData + ( ULONG ) llOffset, ulRead );
   HB_MEMFSMT_UNLOCK
   ( ( PHB_MEMFS_FILE ) pFile )->llPos = llOffset + ulCount;
   return ulRead;
}


HB_MEMFS_EXPORT ULONG hb_memfsWriteAt( HB_FHANDLE pFile, const void * pBuff, ULONG ulCount, HB_FOFFSET llOffset )
{
   PHB_MEMFS_INODE  pInode = ( ( PHB_MEMFS_FILE ) pFile )->pInode;

   if( ( ( ( PHB_MEMFS_FILE ) pFile )->uiFlags & FOX_WRITE ) == 0 )
      return 0; /* access denied */

   if( llOffset < 0 )
      return 0;

   HB_MEMFSMT_LOCK

   /* Reallocate if neccesary */
   if( pInode->llAlloc < llOffset + ulCount )
   {
      HB_FOFFSET  llNewAlloc = pInode->llAlloc + ( pInode->llAlloc >> 1 );

      if( llNewAlloc < llOffset + ulCount )
         llNewAlloc = llOffset + ulCount;

      pInode->pData = hb_xrealloc( pInode->pData, llNewAlloc );
      memset( pInode->pData + ( ULONG ) pInode->llAlloc, 0, llNewAlloc - pInode->llAlloc );
      pInode->llAlloc = llNewAlloc;
   }
   memcpy( pInode->pData + ( ULONG ) llOffset, pBuff, ulCount );

   if( pInode->llSize < llOffset + ulCount )
      pInode->llSize = llOffset + ulCount;
   
   HB_MEMFSMT_UNLOCK
   ( ( PHB_MEMFS_FILE ) pFile )->llPos = llOffset + ulCount;
   return ulCount;
}

#ifdef HB_MEMFS_PUBLIC_API
HB_MEMFS_EXPORT ULONG hb_memfsRead( HB_FHANDLE pFile, void * pBuff, ULONG ulCount )
{
   return hb_memfsReadAt( pFile, pBuff, ulCount, ( ( PHB_MEMFS_FILE ) pFile )->llPos );
}


HB_MEMFS_EXPORT ULONG hb_memfsWrite( HB_FHANDLE pFile, const void * pBuff, ULONG ulCount )
{
   return hb_memfsWriteAt( pFile, pBuff, ulCount, ( ( PHB_MEMFS_FILE ) pFile )->llPos );
}
#endif


HB_MEMFS_EXPORT BOOL hb_memfsTruncAt( HB_FHANDLE pFile, HB_FOFFSET llOffset )
{
   PHB_MEMFS_INODE  pInode = ( ( PHB_MEMFS_FILE ) pFile )->pInode;
   HB_FOFFSET       llNewAlloc;

   if( ( ( ( PHB_MEMFS_FILE ) pFile )->uiFlags & FOX_WRITE ) == 0 )
      return 0; /* access denied */

   if( llOffset < 0 )
      return 0;

   HB_MEMFSMT_LOCK

   /* Reallocate if neccesary */
   if( pInode->llAlloc < llOffset )
   {
      llNewAlloc = pInode->llAlloc + ( pInode->llAlloc >> 1 );

      if( llNewAlloc < llOffset )
         llNewAlloc = llOffset;

      pInode->pData = hb_xrealloc( pInode->pData, llNewAlloc );
      memset( pInode->pData + ( ULONG ) pInode->llAlloc, 0, llNewAlloc - pInode->llAlloc );
      pInode->llAlloc = llNewAlloc;
   }
   else if( ( pInode->llAlloc >> 2 ) > ( llOffset > HB_MEMFS_INITSIZE ? llOffset : HB_MEMFS_INITSIZE ) )
   {
      llNewAlloc = ( llOffset > HB_MEMFS_INITSIZE ? llOffset : HB_MEMFS_INITSIZE );
      pInode->pData = hb_xrealloc( pInode->pData, llNewAlloc );
   }
   pInode->llSize = llOffset;
   HB_MEMFSMT_UNLOCK
   return 1;
}


HB_MEMFS_EXPORT HB_FOFFSET hb_memfsSeek( HB_FHANDLE pFile, HB_FOFFSET llOffset, USHORT uiFlags )
{
   PHB_MEMFS_INODE  pInode = ( ( PHB_MEMFS_FILE ) pFile )->pInode;
   HB_FOFFSET       llPos;

   HB_MEMFSMT_LOCK
   if( uiFlags & FS_END )
      llPos = pInode->llSize + llOffset;
   else if( uiFlags & FS_RELATIVE )
      llPos = ( ( PHB_MEMFS_FILE ) pFile )->llPos + llOffset;
   else
      llPos = llOffset;

   if( llPos < 0 )
      llPos = 0;

   if( llPos > pInode->llSize )
      llPos = pInode->llSize;

   HB_MEMFSMT_UNLOCK
   ( ( PHB_MEMFS_FILE ) pFile )->llPos = llPos;
   return llPos;
}


HB_MEMFS_EXPORT void hb_memfsCommit( HB_FHANDLE pFile )
{
   HB_SYMBOL_UNUSED( pFile );
   return;
}


HB_MEMFS_EXPORT BOOL hb_memfsLock( HB_FHANDLE pFile, HB_FOFFSET ulStart, HB_FOFFSET ulLength, USHORT uiMode )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( ulStart );
   HB_SYMBOL_UNUSED( ulLength );
   HB_SYMBOL_UNUSED( uiMode );
   return 1;
}

/******************************************************
*
*  I/O Driver for Memory file system
*
*******************************************************/

#define FILE_PREFIX     "mem:"
#define FILE_PREFIX_LEN strlen( FILE_PREFIX )

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   HB_FHANDLE            hFile;
}
HB_FILE;


static PHB_FILE s_fileNew( HB_FHANDLE hFile );


static BOOL s_fileAccept( const char * pFilename )
{
   /* printf("\ns_fileAccept %s\n", pFilename ); */
   return hb_strnicmp( pFilename, FILE_PREFIX, FILE_PREFIX_LEN ) == 0;
}


static BOOL s_fileExists( const char * pFilename, char * pRetPath )
{
   if( hb_memfsFileExists( pFilename + FILE_PREFIX_LEN ) )
   {
      /* Warning: return buffer could be the same memory place as filename parameter! */
      if( pRetPath && pRetPath != pFilename )
         strcpy( pRetPath, pFilename );
      return 1;
   }
   return 0;
}


static BOOL s_fileDelete( const char * pFilename )
{
   return hb_memfsDelete( pFilename + FILE_PREFIX_LEN );
}


static BOOL s_fileRename( const char * szName, const char * szNewName )
{
   szName += FILE_PREFIX_LEN;
   if( s_fileAccept( szNewName ) )
   {
      szNewName += FILE_PREFIX_LEN;
      return hb_memfsRename( szName, szNewName );
   }
   return 0;
}


static PHB_FILE s_fileOpen( const char * szName, const char * szDefExt, USHORT uiExFlags, const char * pPaths, PHB_ITEM pError )
{
   HB_FHANDLE hFile;
   char       szNameNew[ HB_PATH_MAX + 1 ];
   USHORT     uiFlags;
   ULONG      ulLen;

   HB_SYMBOL_UNUSED( pPaths );
   HB_SYMBOL_UNUSED( pError );

   hb_strncpy( szNameNew, szName + FILE_PREFIX_LEN, HB_PATH_MAX );

   ulLen = strlen( szNameNew );
   do {
      if( ulLen == 0 || strchr( HB_OS_PATH_DELIM_CHR_LIST, szNameNew[ ulLen - 1 ] ) )
      {
         hb_strncat( szNameNew, szDefExt, HB_PATH_MAX );
         break;
      }
   }
   while( szNameNew[ --ulLen ] != '.' );

   uiFlags = uiExFlags & 0xff;
   if( uiExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) )
   {
      uiFlags |= FO_CREAT;
      if( uiExFlags & FXO_UNIQUE )
         uiFlags |= FO_EXCL;
      else if( uiExFlags & FXO_TRUNCATE )
         uiFlags |= FO_TRUNC;
   }

   hFile = hb_memfsOpen( szNameNew, uiFlags );

   if( hFile != FS_ERROR )
      return s_fileNew( hFile );
   else
   {
      if( pError )
      {
         hb_errPutFileName( pError, szName );
         hb_errPutOsCode( pError, hb_memfsError() );
         hb_errPutGenCode( pError, ( USHORT ) ( ( uiExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
      return NULL;
   }
}


static void s_fileClose( PHB_FILE pFile )
{
   hb_memfsClose( pFile->hFile );
   hb_xfree( pFile );
}


static BOOL s_fileLock( PHB_FILE pFile, HB_FOFFSET ulStart,
                        HB_FOFFSET ulLen, int iType )
{
   return hb_memfsLock( pFile->hFile, ulStart, ulLen, iType );
}


static ULONG s_fileReadAt( PHB_FILE pFile, void * buffer,
                           ULONG ulSize, HB_FOFFSET llOffset )
{
   return hb_memfsReadAt( pFile->hFile, buffer, ulSize, llOffset );
}


static ULONG s_fileWriteAt( PHB_FILE pFile, const void * buffer,
                               ULONG ulSize, HB_FOFFSET llOffset )
{
   return hb_memfsWriteAt( pFile->hFile, buffer, ulSize, llOffset );
}


static BOOL s_fileTruncAt( PHB_FILE pFile, HB_FOFFSET llOffset )
{
   return hb_memfsTruncAt( pFile->hFile, llOffset );
}


static HB_FOFFSET s_fileSize( PHB_FILE pFile )
{
   return hb_memfsSeek( pFile->hFile, 0, FS_END );
}


static void s_fileCommit( PHB_FILE pFile )
{
   hb_memfsCommit( pFile->hFile );
}


static HB_FHANDLE s_fileHandle( PHB_FILE pFile )
{
   return pFile ? pFile->hFile : FS_ERROR;
}


const HB_FILE_FUNCS s_fileFuncs =
{
   s_fileAccept,
   s_fileExists,
   s_fileDelete,
   s_fileRename,
   s_fileOpen,
   s_fileClose,
   s_fileLock,
   s_fileReadAt,
   s_fileWriteAt,
   s_fileTruncAt,
   s_fileSize,
   s_fileCommit,
   s_fileHandle
};


static PHB_FILE s_fileNew( HB_FHANDLE hFile )
{
   PHB_FILE pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );

   pFile->pFuncs = &s_fileFuncs;
   pFile->hFile = hFile;
   return pFile;
}


HB_FUNC( HB_IODMEM ) {;}

HB_INIT_SYMBOLS_BEGIN( iodmem__InitSymbols )
{ "HB_IODMEM",   {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( HB_IODMEM )}, NULL }
HB_INIT_SYMBOLS_END( iodmem__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_file_io_init_ )
   memfsInit();
   hb_fileRegister( &s_fileFuncs );
HB_CALL_ON_STARTUP_END( _hb_file_io_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup iodmem__InitSymbols
   #pragma startup _hb_file_io_init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_iodmem__InitSymbols = iodmem__InitSymbols;
   static HB_$INITSYM hb_vm_auto_hb_file_io_init_ = _hb_file_io_init_;
   #pragma data_seg()
#endif
