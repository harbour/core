/*
 * Harbour Project source code:
 *   dummy I/O driver initialization
 *
 * Copyright 2014 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#define _HB_FILE_IMPLEMENTATION_

#include "hbapi.h"
#include "hbapifs.h"

#define HB_FILE_ERR_UNSUPPORTED  ( ( HB_ERRCODE ) FS_ERROR )

static HB_BOOL s_fileAccept( const char * pszFileName )
{
   HB_SYMBOL_UNUSED( pszFileName );

   return HB_FALSE;
}

static HB_BOOL s_fileExists( const char * pszFileName, char * pRetPath )
{
   HB_SYMBOL_UNUSED( pszFileName );
   HB_SYMBOL_UNUSED( pRetPath );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileDelete( const char * pszFileName )
{
   HB_SYMBOL_UNUSED( pszFileName );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileRename( const char * pszName, const char * pszNewName )
{
   HB_SYMBOL_UNUSED( pszName );
   HB_SYMBOL_UNUSED( pszNewName );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileCopy( const char * pSrcFile, const char * pszDstFile )
{
   HB_SYMBOL_UNUSED( pSrcFile );
   HB_SYMBOL_UNUSED( pszDstFile );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileDirExists( const char * pszDirName )
{
   HB_SYMBOL_UNUSED( pszDirName );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileDirMake( const char * pszDirName )
{
   HB_SYMBOL_UNUSED( pszDirName );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileDirRemove( const char * pszDirName )
{
   HB_SYMBOL_UNUSED( pszDirName );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static double s_fileDirSpace( const char * pszDirName, HB_USHORT uiType )
{
   HB_SYMBOL_UNUSED( pszDirName );
   HB_SYMBOL_UNUSED( uiType );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return 0.0;
}

static PHB_ITEM s_fileDirectory( const char * pszDirSpec, const char * pszAttr )
{
   HB_SYMBOL_UNUSED( pszDirSpec );
   HB_SYMBOL_UNUSED( pszAttr );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return NULL;
}

static HB_BOOL s_fileTimeGet( const char * pszFileName, long * plJulian, long * plMillisec )
{
   HB_SYMBOL_UNUSED( pszFileName );
   HB_SYMBOL_UNUSED( plJulian );
   HB_SYMBOL_UNUSED( plMillisec );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileTimeSet( const char * pszFileName, long lJulian, long lMillisec )
{
   HB_SYMBOL_UNUSED( pszFileName );
   HB_SYMBOL_UNUSED( lJulian );
   HB_SYMBOL_UNUSED( lMillisec );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileAttrGet( const char * pszFileName, HB_FATTR * pnAttr )
{
   HB_SYMBOL_UNUSED( pszFileName );
   HB_SYMBOL_UNUSED( pnAttr );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileAttrSet( const char * pszFileName, HB_FATTR nAttr )
{
   HB_SYMBOL_UNUSED( pszFileName );
   HB_SYMBOL_UNUSED( nAttr );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileLink( const char * pszExisting, const char * pszNewName )
{
   HB_SYMBOL_UNUSED( pszExisting );
   HB_SYMBOL_UNUSED( pszNewName );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_BOOL s_fileLinkSym( const char * pszTarget, const char * pszNewName )
{
   HB_SYMBOL_UNUSED( pszTarget );
   HB_SYMBOL_UNUSED( pszNewName );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static char * s_fileLinkRead( const char * pszFileName )
{
   HB_SYMBOL_UNUSED( pszFileName );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return NULL;
}

static PHB_FILE s_fileOpen( const char * pszName, const char * pszDefExt, HB_USHORT uiExFlags,
                            const char * pPaths, PHB_ITEM pError )
{
   HB_SYMBOL_UNUSED( pszName );
   HB_SYMBOL_UNUSED( pszDefExt );
   HB_SYMBOL_UNUSED( uiExFlags );
   HB_SYMBOL_UNUSED( pszDefExt );
   HB_SYMBOL_UNUSED( pPaths );
   HB_SYMBOL_UNUSED( pError );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return NULL;
}

static void s_fileClose( PHB_FILE pFile )
{
   HB_SYMBOL_UNUSED( pFile );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );
}

static HB_BOOL s_fileLock( PHB_FILE pFile, HB_FOFFSET nStart,
                           HB_FOFFSET nLen, int iType )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( nStart );
   HB_SYMBOL_UNUSED( nLen );
   HB_SYMBOL_UNUSED( iType );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static int s_fileLockTest( PHB_FILE pFile, HB_FOFFSET nStart,
                           HB_FOFFSET nLen, int iType )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( nStart );
   HB_SYMBOL_UNUSED( nLen );
   HB_SYMBOL_UNUSED( iType );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return -1;
}

static HB_SIZE s_fileRead( PHB_FILE pFile, void * data,
                           HB_SIZE nSize, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( nSize );
   HB_SYMBOL_UNUSED( timeout );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return 0;
}

static HB_SIZE s_fileWrite( PHB_FILE pFile, const void * data,
                            HB_SIZE nSize, HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( nSize );
   HB_SYMBOL_UNUSED( timeout );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return 0;
}

static HB_SIZE s_fileReadAt( PHB_FILE pFile, void * buffer,
                             HB_SIZE nSize, HB_FOFFSET nOffset )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( buffer );
   HB_SYMBOL_UNUSED( nSize );
   HB_SYMBOL_UNUSED( nOffset );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return 0;
}

static HB_SIZE s_fileWriteAt( PHB_FILE pFile, const void * buffer,
                              HB_SIZE nSize, HB_FOFFSET nOffset )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( buffer );
   HB_SYMBOL_UNUSED( nSize );
   HB_SYMBOL_UNUSED( nOffset );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return 0;
}

static HB_BOOL s_fileTruncAt( PHB_FILE pFile, HB_FOFFSET nOffset )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( nOffset );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_FOFFSET s_fileSeek( PHB_FILE pFile, HB_FOFFSET nOffset,
                              HB_USHORT uiFlags )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( nOffset );
   HB_SYMBOL_UNUSED( uiFlags );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return 0;
}

static HB_FOFFSET s_fileSize( PHB_FILE pFile )
{
   HB_SYMBOL_UNUSED( pFile );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return 0;
}

static HB_BOOL s_fileEof( PHB_FILE pFile )
{
   HB_SYMBOL_UNUSED( pFile );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_TRUE;
}

static void s_fileFlush( PHB_FILE pFile, HB_BOOL fDirty )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( fDirty );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );
}

static void s_fileCommit( PHB_FILE pFile )
{
   HB_SYMBOL_UNUSED( pFile );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );
}

static HB_BOOL s_fileConfigure( PHB_FILE pFile, int iIndex, PHB_ITEM pValue )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( iIndex );
   HB_SYMBOL_UNUSED( pValue );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return HB_FALSE;
}

static HB_FHANDLE s_fileHandle( PHB_FILE pFile )
{
   HB_SYMBOL_UNUSED( pFile );

   hb_fsSetError( HB_FILE_ERR_UNSUPPORTED );

   return ( HB_FHANDLE ) FS_ERROR;
}

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

   s_fileOpen,
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

typedef void * ( * HB_FILE_FUNC )( PHB_FILE );
#define HB_FILE_FUNC_COUNT ( sizeof( HB_FILE_FUNCS ) / sizeof( HB_FILE_FUNC ) )

HB_BOOL hb_fileRegisterPart( HB_FILE_FUNCS * pFuncs )
{
   const HB_FILE_FUNC * pDummyFunc;
   HB_FILE_FUNC * pFunction;
   int iCount;

   pDummyFunc = ( const HB_FILE_FUNC * ) &s_fileFuncs;
   pFunction = ( HB_FILE_FUNC * ) pFuncs;

   for( iCount = 0; iCount < ( int ) HB_FILE_FUNC_COUNT;
        iCount++, pDummyFunc++, pFunction++ )
   {
      if( * pFunction == NULL )
         * pFunction = * pDummyFunc;
   }

   return hb_fileRegisterFull( pFuncs );
}
