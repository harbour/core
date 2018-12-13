/*
 * I/O driver for PIPE streams
 *
 * Copyright 2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#define _HB_FILE_IMPLEMENTATION_

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbinit.h"

#define FILE_PREFIX      "PIPE:"
#define FILE_PREFIX_LEN  strlen( FILE_PREFIX )

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   HB_FHANDLE            hProcess;
   HB_FHANDLE            hPipeRD;
   HB_FHANDLE            hPipeWR;
   HB_BOOL               fEof;
   HB_MAXINT             timeout;
}
HB_FILE;

static PHB_FILE hb_fileProcessOpen( const char * pszCommand, HB_FATTR nMode,
                                    HB_MAXINT timeout, HB_BOOL fDetach );

static HB_BOOL s_fileAccept( PHB_FILE_FUNCS pFuncs, const char * pszFileName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return pszFileName[ 0 ] == '|' ||
          hb_strnicmp( pszFileName, FILE_PREFIX, FILE_PREFIX_LEN ) == 0;
}

static PHB_FILE s_fileOpen( PHB_FILE_FUNCS pFuncs, const char * pszName,
                            const char * pszDefExt, HB_FATTR nExFlags,
                            const char * pPaths, PHB_ITEM pError )
{
   const char * pszCommand = pszName;
   HB_MAXINT timeout = -1;
   PHB_FILE pFile;

   HB_SYMBOL_UNUSED( pFuncs );
   HB_SYMBOL_UNUSED( pszDefExt );
   HB_SYMBOL_UNUSED( pPaths );

   if( pszCommand[ 0 ] == '|' )
      pszCommand++;
   else
      pszCommand += FILE_PREFIX_LEN;

   if( HB_ISDIGIT( *pszCommand ) )
   {
      const char * ptr = pszCommand;
      int iValue = 0;

      while( HB_ISDIGIT( * ptr ) )
         iValue = iValue * 10 + ( * ptr++ - '0' );
      if( * ptr == ':' )
      {
         pszCommand = ptr + 1;
         timeout = iValue;
      }
   }

   pFile = hb_fileProcessOpen( pszCommand, nExFlags & ( FO_READ | FO_WRITE | FO_READWRITE ),
                               timeout, HB_FALSE );
   if( pError )
   {
      hb_errPutFileName( pError, pszName );
      if( pFile == NULL )
      {
         hb_errPutOsCode( pError, hb_fsError() );
         hb_errPutGenCode( pError, ( HB_ERRCODE ) EG_OPEN );
      }
   }

   return pFile;
}

static void s_fileClose( PHB_FILE pFile )
{
   if( pFile->hPipeRD != FS_ERROR )
      hb_fsClose( pFile->hPipeRD );
   if( pFile->hPipeWR != FS_ERROR )
      hb_fsClose( pFile->hPipeWR );
   if( pFile->hProcess != FS_ERROR )
      hb_fsProcessValue( pFile->hProcess, HB_TRUE );
   hb_xfree( pFile );
}

static HB_SIZE s_fileRead( PHB_FILE pFile, void * data,
                           HB_SIZE nSize, HB_MAXINT timeout )
{
   HB_SIZE nRead = 0;

   if( pFile->hPipeRD == FS_ERROR )
      hb_fsSetError( 6 );
   else if( ! pFile->fEof )
   {
      if( timeout == -1 )
         timeout = pFile->timeout;

      nRead = hb_fsPipeRead( pFile->hPipeRD, data, nSize, timeout );
      if( nRead == ( HB_SIZE ) -1 )
      {
         pFile->fEof = HB_TRUE;
         nRead = 0;
      }
   }

   return nRead;
}

static HB_SIZE s_fileWrite( PHB_FILE pFile, const void * data,
                            HB_SIZE nSize, HB_MAXINT timeout )
{
   HB_SIZE nWritten = ( HB_SIZE ) -1;

   if( pFile->hPipeWR == FS_ERROR )
      hb_fsSetError( 6 );
   else
   {
      if( timeout == -1 )
         timeout = pFile->timeout;

      nWritten = hb_fsPipeWrite( pFile->hPipeWR, data, nSize, timeout );
   }
   return nWritten;
}

static HB_BOOL s_fileEof( PHB_FILE pFile )
{
   hb_fsSetError( 0 );
   return pFile->fEof;
}

static HB_BOOL s_fileConfigure( PHB_FILE pFile, int iIndex, PHB_ITEM pValue )
{
   switch( iIndex )
   {
      case HB_VF_TIMEOUT:
      {
         HB_MAXINT timeout = pFile->timeout;

         if( HB_IS_NUMERIC( pValue ) )
            pFile->timeout = hb_itemGetNInt( pValue );
         hb_itemPutNInt( pValue, timeout );
         return HB_TRUE;
      }
      case HB_VF_SHUTDOWN:
      {
         int iMode = pFile->hPipeRD != FS_ERROR ?
                     ( pFile->hPipeWR != FS_ERROR ? FO_READWRITE : FO_READ ) :
                     ( pFile->hPipeWR != FS_ERROR ? FO_WRITE : -1 );

         if( HB_IS_NUMERIC( pValue ) )
         {
            switch( hb_itemGetNI( pValue ) )
            {
               case FO_READ:
                  if( pFile->hPipeRD != FS_ERROR )
                     hb_fsClose( pFile->hPipeRD );
                  break;
               case FO_WRITE:
                  if( pFile->hPipeWR != FS_ERROR )
                     hb_fsClose( pFile->hPipeWR );
                  break;
               case FO_READWRITE:
                  if( pFile->hPipeRD != FS_ERROR )
                     hb_fsClose( pFile->hPipeRD );
                  if( pFile->hPipeWR != FS_ERROR )
                     hb_fsClose( pFile->hPipeWR );
                  break;
            }
         }
         hb_itemPutNI( pValue, iMode );
         return HB_TRUE;
      }
      case HB_VF_RDHANDLE:
         hb_itemPutNInt( pValue, ( HB_NHANDLE ) pFile->hPipeRD );
         return HB_TRUE;

      case HB_VF_WRHANDLE:
         hb_itemPutNInt( pValue, ( HB_NHANDLE ) pFile->hPipeWR );
         return HB_TRUE;

      case HB_VF_IONAME:
         hb_itemPutC( pValue, FILE_PREFIX );
         return HB_TRUE;
   }

   return HB_FALSE;
}

static HB_FHANDLE s_fileHandle( PHB_FILE pFile )
{
   return pFile ? ( pFile->hPipeRD != FS_ERROR ?
                    pFile->hPipeRD : pFile->hPipeWR ) : FS_ERROR;
}

static HB_FILE_FUNCS s_fileFuncs =
{
   s_fileAccept,

   NULL, /* s_fileExists */
   NULL, /* s_fileDelete */
   NULL, /* s_fileRename */
   NULL, /* s_fileCopy */

   NULL, /* s_fileDirExists */
   NULL, /* s_fileDirMake */
   NULL, /* s_fileDirRemove */
   NULL, /* s_fileDirSpace */
   NULL, /* s_fileDirectory */

   NULL, /* s_fileTimeGet */
   NULL, /* s_fileTimeSet */
   NULL, /* s_fileAttrGet */
   NULL, /* s_fileAttrSet */

   NULL, /* s_fileLink */
   NULL, /* s_fileLinkSym */
   NULL, /* s_fileLinkRead */

   s_fileOpen,
   s_fileClose,
   NULL, /* s_fileLock */
   NULL, /* s_fileLockTest */
   s_fileRead,
   s_fileWrite,
   NULL, /* s_fileReadAt */
   NULL, /* s_fileWriteAt */
   NULL, /* s_fileTruncAt */
   NULL, /* s_fileSeek */
   NULL, /* s_fileSize */
   s_fileEof,
   NULL, /* s_fileFlush */
   NULL, /* s_fileCommit */
   s_fileConfigure,
   s_fileHandle
};

static PHB_FILE s_fileNew( HB_FHANDLE hProcess, HB_FHANDLE hPipeRD,
                           HB_FHANDLE hPipeWR, HB_MAXINT timeout )
{
   PHB_FILE pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );

   pFile->pFuncs  = &s_fileFuncs;
   pFile->hProcess = hProcess;
   pFile->hPipeRD = hPipeRD;
   pFile->hPipeWR = hPipeWR;
   pFile->fEof    = HB_FALSE;
   pFile->timeout = timeout;

   return pFile;
}

static PHB_FILE hb_fileProcessOpen( const char * pszCommand, HB_FATTR nMode,
                                    HB_MAXINT timeout, HB_BOOL fDetach )
{
   HB_FHANDLE hProcess, hPipeRD = FS_ERROR, hPipeWR = FS_ERROR;
   HB_FHANDLE * phStdIn = NULL, * phStdOut = NULL;

   if( pszCommand == NULL || *pszCommand == '\0' )
      return NULL;

   switch( nMode )
   {
      case FO_READ:
         phStdOut = &hPipeRD;
         break;
      case FO_WRITE:
         phStdIn = &hPipeWR;
         break;
      case FO_READWRITE:
         phStdOut = &hPipeRD;
         phStdIn = &hPipeWR;
         break;
      default:
         return NULL;
   }

   hProcess = hb_fsProcessOpen( pszCommand, phStdIn, phStdOut, NULL,
                                fDetach, NULL );
   return hProcess != FS_ERROR ? s_fileNew( hProcess, hPipeRD, hPipeWR, timeout ) : NULL;
}

static PHB_FILE hb_fileFromPipeHandle( HB_FHANDLE hProcess, HB_FHANDLE hPipeRD, HB_FHANDLE hPipeWR, HB_MAXINT timeout )
{
   return hPipeRD != FS_ERROR || hPipeWR != FS_ERROR ?
          s_fileNew( hProcess, hPipeRD, hPipeWR, timeout ) : NULL;
}

/* hb_vfFromPipes( [<hReads>], [<hWrite>], [<hProcess>], [<nTimeout>] )
         --> <pHandle> | NIL */
HB_FUNC( HB_VFFROMPIPES )
{
   HB_FHANDLE hPipeRD = hb_numToHandle( hb_parnintdef( 1, FS_ERROR ) );
   HB_FHANDLE hPipeWR = hb_numToHandle( hb_parnintdef( 2, FS_ERROR ) );
   HB_FHANDLE hProcess = hb_numToHandle( hb_parnintdef( 3, FS_ERROR ) );
   PHB_FILE pFile = hb_fileFromPipeHandle( hProcess, hPipeRD, hPipeWR,
                                           hb_parnintdef( 4, -1 ) );
   if( pFile )
      hb_fileItemPut( hb_param( -1, HB_IT_ANY ), pFile );
}

/* hb_vfOpenProcess( <cCommand>, [<nMode>=FO_READ], [<nTimeout>], [<lDetach>] )
         --> <pHandle> | NIL */
HB_FUNC( HB_VFOPENPROCESS )
{
   const char * pszCommand = hb_parc( 1 );
   HB_FATTR nMode = hb_parnldef( 2, FO_READ );
   HB_MAXINT timeout = hb_parnintdef( 3, -1 );
   HB_BOOL fDetach = hb_parl( 4 );
   PHB_FILE pFile;

   nMode &= FO_READ | FO_WRITE | FO_READWRITE;
   pFile = hb_fileProcessOpen( pszCommand, nMode, timeout, fDetach );

   if( pFile )
      hb_fileItemPut( hb_param( -1, HB_IT_ANY ), pFile );
}

HB_FUNC( HB_PIPEIO ) { ; }

HB_CALL_ON_STARTUP_BEGIN( _hb_file_pipeio_init_ )
   hb_fileRegisterPart( &s_fileFuncs );
HB_CALL_ON_STARTUP_END( _hb_file_pipeio_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_file_pipeio_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _hb_file_pipeio_init_ )
   #include "hbiniseg.h"
#endif
