/*
 * Harbour Project source code:
 * PRG level interface to Harbour FILE IO API
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

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbdate.h"

/* extended FILE IO handle destructor */
static HB_GARBAGE_FUNC( hb_file_Destructor )
{
   PHB_FILE * fileHolder = ( PHB_FILE * ) Cargo;

   if( * fileHolder )
   {
      PHB_FILE pFile = * fileHolder;
      * fileHolder = NULL;
      hb_fileClose( pFile );
   }
}

static const HB_GC_FUNCS s_gcFileFuncs =
{
   hb_file_Destructor,
   hb_gcDummyMark
};

static PHB_FILE hb_fileParam( int iParam )
{
   PHB_FILE * fileHolder = ( PHB_FILE * ) hb_parptrGC( &s_gcFileFuncs, iParam );

   if( fileHolder && * fileHolder )
      return * fileHolder;

   hb_errRT_BASE_SubstR( EG_ARG, 2021, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static PHB_FILE * hb_fileParamPtr( int iParam )
{
   PHB_FILE * fileHolder = ( PHB_FILE * ) hb_parptrGC( &s_gcFileFuncs, iParam );

   if( fileHolder && * fileHolder )
      return fileHolder;

   hb_errRT_BASE_SubstR( EG_ARG, 2021, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static void hb_fileReturn( PHB_FILE pFile )
{
   if( pFile )
   {
      PHB_FILE * fileHolder = ( PHB_FILE * ) hb_gcAllocate( sizeof( PHB_FILE ),
                                                            &s_gcFileFuncs );
      * fileHolder = pFile;
      hb_retptrGC( fileHolder );
   }
   else
      hb_ret();
}

/* hb_VFExists( <cFileName>, [ @<cDestFileName> ] ) -> <lOK> */
HB_FUNC( HB_VFEXISTS )
{
   const char * pszFileName = hb_parc( 1 );
   HB_BOOL fResult = HB_FALSE;
   HB_ERRCODE uiError = 2;

   if( pszFileName )
   {
      if( HB_ISBYREF( 2 ) )
      {
         char szName[ HB_PATH_MAX ];

         szName[ 0 ] = '\0';
         fResult = hb_fileExists( pszFileName, szName );
         hb_storc( szName, 2 );
      }
      else
         fResult = hb_fileExists( pszFileName, NULL );

      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retl( fResult );
}

/* hb_VFErase( <cFileName> ) -> <nResult> */
HB_FUNC( HB_VFERASE )
{
   const char * pszFile = hb_parc( 1 );
   HB_ERRCODE uiError = 3;
   int iResult = F_ERROR;

   if( pszFile )
   {
      if( hb_fileDelete( pszFile ) )
         iResult = 0;
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retni( iResult );
}

/* hb_VFRename( <cFileSrc>, <cFileDst> ) -> <nResult> */
HB_FUNC( HB_VFRENAME )
{
   const char * szFileOld = hb_parc( 1 ),
              * szFileNew = hb_parc( 2 );
   HB_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( szFileOld && szFileNew )
   {
      if( hb_fileRename( szFileOld, szFileNew ) )
         iResult = 0;
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retni( iResult );
}

/* hb_VFCopyFile( <cFileSrc>, <cFileDst> ) -> <nResult> */
HB_FUNC( HB_VFCOPYFILE )
{
   const char * pszSource = hb_parc( 1 ),
              * pszDestin = hb_parc( 2 );
   HB_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszSource && pszDestin )
   {
      if( hb_fileCopy( pszSource, pszDestin ) )
         iResult = 0;
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retni( iResult );
}

/* hb_VFDirExists( <cDirName> ) -> <lExists> */
HB_FUNC( HB_VFDIREXISTS )
{
   const char * pszDirName = hb_parc( 1 );
   HB_BOOL fResult = HB_FALSE;
   HB_ERRCODE uiError = 2;

   if( pszDirName )
   {
      fResult = hb_fileDirExists( pszDirName );
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retl( fResult );
}

/* hb_VFDirMake( <cDirName> ) -> <nSuccess> */
HB_FUNC( HB_VFDIRMAKE )
{
   const char * pszDirName = hb_parc( 1 );
   HB_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszDirName )
   {
      if( hb_fileDirMake( pszDirName ) )
         iResult = 0;
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retni( iResult );
}

/* hb_VFDirRemove( <cDirName> ) -> <nSuccess> */
HB_FUNC( HB_VFDIRREMOVE )
{
   const char * pszDirName = hb_parc( 1 );
   HB_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszDirName )
   {
      if( hb_fileDirRemove( pszDirName ) )
         iResult = 0;
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retni( iResult );
}

/* hb_VFDirectory( [ <cDirSpec> ], [ <cAttr> ] ) -> <aDirectory> */
HB_FUNC( HB_VFDIRECTORY )
{
   hb_itemReturnRelease( hb_fileDirectory( hb_parc( 1 ), hb_parc( 2 ) ) );
   hb_fsSetFError( hb_fsError() );
}

/* hb_VFDirSpace( <cDirName>, [ <nInfoType> ] ) -> <nFreeSpace> */
HB_FUNC( HB_VFDIRSPACE )
{
   HB_USHORT uiType = ( HB_USHORT ) hb_parnidef( 2, HB_DISK_AVAIL );

   hb_retnlen( hb_fileDirSpace( hb_parc( 1 ), uiType ), -1, 0 );
   hb_fsSetFError( hb_fsError() );
}

/* hb_VFAttrGet( <cFileName>, @<nAttr> ) -> <lOK> */
HB_FUNC( HB_VFATTRGET )
{
   HB_FATTR nAttr = 0;

   hb_retl( hb_fileAttrGet( hb_parcx( 1 ), &nAttr ) );
   hb_fsSetFError( hb_fsError() );

   hb_stornl( nAttr, 2 );
}

/* hb_VFAttrSet( <cFileName>,  <nAttr> ) -> <lOK> */
HB_FUNC( HB_VFATTRSET )
{
   hb_retl( hb_fileAttrSet( hb_parcx( 1 ), ( HB_FATTR ) hb_parnl( 2 ) ) );
   hb_fsSetFError( hb_fsError() );
}

/* hb_VFTimeGet( <cFileName>, @<tsDateTime> ) -> <lOK> */
HB_FUNC( HB_VFTIMEGET )
{
   long lJulian, lMillisec;
   HB_BOOL fOK;

   fOK = hb_fileTimeGet( hb_parcx( 1 ), &lJulian, &lMillisec );
   hb_fsSetFError( hb_fsError() );

   if( fOK )
   {
      if( HB_ISBYREF( 3 ) )
      {
         char buf[ 13 ];
         hb_timeStr( buf, lMillisec );
         if( lMillisec % 1000 == 0 )
            buf[ 8 ] = '\0';
         hb_storc( buf, 3 );
         hb_stordl( lJulian, 2 );
      }
      else
         hb_stortdt( lJulian, lMillisec, 2 );

      hb_retl( HB_TRUE );
   }
   else
   {
      if( HB_ISBYREF( 3 ) )
      {
         hb_storc( NULL, 3 );
         hb_stordl( 0, 2 );
      }
      else
         hb_stortdt( 0, 0, 2 );

      hb_retl( HB_FALSE );
   }
}

/* hb_VFTimeSet( <cFileName>,  <tsDateTime> ) -> <lOK> */
HB_FUNC( HB_VFTIMESET )
{
   long lDate = -1, lTime = -1;

   if( HB_ISTIMESTAMP( 2 ) )
      hb_partdt( &lDate, &lTime, 2 );
   else
   {
      if( HB_ISDATE( 2 ) )
         lDate = hb_pardl( 2 );
      if( HB_ISCHAR( 3 ) )
      {
         int iHour, iMinutes, iSeconds, iMSec;
         if( hb_timeStrGet( hb_parc( 3 ), &iHour, &iMinutes, &iSeconds, &iMSec ) )
            lTime = hb_timeEncode( iHour, iMinutes, iSeconds, iMSec );
      }
   }

   hb_retl( hb_fileTimeSet( hb_parcx( 1 ), lDate, lTime ) );
   hb_fsSetFError( hb_fsError() );
}

/* hb_VFLink( <cExistingFileName>, <cNewFileName> ) -> <nSuccess> */
HB_FUNC( HB_VFLINK )
{
   const char * pszExisting = hb_parc( 1 ), * pszNewFile = hb_parc( 2 );
   HB_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszExisting && pszNewFile )
   {
      if( hb_fileLink( pszExisting, pszNewFile ) )
         iResult = 0;
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retni( iResult );
}

/* hb_VFLinkSym( <cTargetFileName>, <cNewFileName> ) -> <nSuccess> */
HB_FUNC( HB_VFLINKSYM )
{
   const char * pszTarget = hb_parc( 1 ), * pszNewFile = hb_parc( 2 );
   HB_ERRCODE uiError = 2;
   int iResult = F_ERROR;

   if( pszTarget && pszNewFile )
   {
      if( hb_fileLinkSym( pszTarget, pszNewFile ) )
         iResult = 0;
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retni( iResult );
}

/* hb_VFLinkRead( <cFileName> ) -> <cDestFileName> | "" */
HB_FUNC( HB_VFLINKREAD )
{
   const char * pszFile = hb_parc( 1 );
   char * pszResult = NULL;
   HB_ERRCODE uiError = 2;

   if( pszFile )
   {
      pszResult = hb_fileLinkRead( pszFile );
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
   hb_retc_buffer( pszResult );
}

/* hb_VFOpen( [@]<cFileName>, [ <nModeAttr> ] ) -> <pHandle> | NIL */
HB_FUNC( HB_VFOPEN )
{
   const char * pszFile = hb_parc( 1 );

   if( pszFile )
   {
      PHB_FILE pFile;

      if( HB_ISBYREF( 1 ) )
      {
         char szName[ HB_PATH_MAX ];

         hb_strncpy( szName, pszFile, sizeof( szName ) - 1 );
         pFile = hb_fileExtOpen( szName, NULL /* pDefExt */,
                                 ( HB_USHORT ) ( hb_parnidef( 2, FO_READ | FO_COMPAT ) | FXO_COPYNAME ),
                                 NULL /* pPaths */, NULL /* pError */ );
         hb_storc( szName, 1 );
      }
      else
         pFile = hb_fileExtOpen( pszFile, NULL /* pDefExt */,
                                 ( HB_USHORT ) hb_parnidef( 2, FO_READ | FO_COMPAT ),
                                 NULL /* pPaths */, NULL /* pError */ );

      hb_fsSetFError( hb_fsError() );
      hb_fileReturn( pFile );
   }
   else
   {
      hb_fsSetFError( 0 );
      hb_errRT_BASE( EG_ARG, 2021, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* hb_VFClose( <pHandle> ) -> <lOK> */
HB_FUNC( HB_VFCLOSE )
{
   PHB_FILE * pFilePtr = hb_fileParamPtr( 1 );

   if( pFilePtr )
   {
      PHB_FILE pFile = * pFilePtr;
      * pFilePtr = NULL;
      hb_fileClose( pFile );
      hb_fsSetFError( hb_fsError() );
      hb_retl( HB_TRUE );
   }
}

/* hb_VFLock( <pHandle>, <nStart>, <nLen>, [ <nType> ] ) -> <lOK> */
HB_FUNC( HB_VFLOCK )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      HB_ERRCODE uiError = 0;
      HB_BOOL fResult = HB_FALSE;

      if( HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
      {
         fResult = hb_fileLock( pFile,
                                ( HB_FOFFSET ) hb_parnint( 2 ),
                                ( HB_FOFFSET ) hb_parnint( 3 ),
                                FL_LOCK | ( hb_parni( 4 ) & ~FL_MASK ) );
         uiError = hb_fsError();
      }
      hb_fsSetFError( uiError );
      hb_retl( fResult );
   }
}

/* hb_VFUnlock( <pHandle>, <nStart>, <nLen> ) -> <lOK> */
HB_FUNC( HB_VFUNLOCK )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      HB_ERRCODE uiError = 0;
      HB_BOOL fResult = HB_FALSE;

      if( HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
      {
         fResult = hb_fileLock( pFile,
                                ( HB_FOFFSET ) hb_parnint( 2 ),
                                ( HB_FOFFSET ) hb_parnint( 3 ),
                                FL_UNLOCK );
         uiError = hb_fsError();
      }
      hb_fsSetFError( uiError );
      hb_retl( fResult );
   }
}

/* hb_VFLockTest( <pHandle>, <nStart>, <nLen>, [ <nType> ] ) -> <nPID> | 0 (nolock) | -1 (err) */
HB_FUNC( HB_VFLOCKTEST )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      HB_ERRCODE uiError = 0;
      int iResult = -1;

      if( HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
      {
         iResult = hb_fileLockTest( pFile,
                                    ( HB_FOFFSET ) hb_parnint( 2 ),
                                    ( HB_FOFFSET ) hb_parnint( 3 ),
                                    FL_LOCK | ( hb_parni( 4 ) & ~FL_MASK ) );
         uiError = hb_fsError();
      }
      hb_fsSetFError( uiError );
      hb_retni( iResult );
   }
}

/* hb_VFRead( <pHandle>, @<cBuff>, [ <nToRead> ], [ <nTimeOut> ] ) -> <nRead> */
HB_FUNC( HB_VFREAD )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
      HB_ERRCODE uiError = 0;
      HB_SIZE nRead = 0;
      HB_SIZE nSize;
      char * buffer;

      if( pBuffer && HB_ISBYREF( 2 ) &&
          hb_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
      {
         if( HB_ISNUM( 3 ) )
         {
            nRead = hb_parns( 3 );
            if( nRead < nSize )
               nSize = nRead;
         }
         nRead = hb_fileRead( pFile, buffer, nSize, hb_parnintdef( 4, -1 ) );
         uiError = hb_fsError();
      }

      hb_retns( nRead );
      hb_fsSetFError( uiError );
   }
}

/* hb_VFReadLen( <pHandle>, <nToRead>, [ <nTimeOut> ] ) -> <cBuffer> */
HB_FUNC( HB_VFREADLEN )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile && HB_ISNUM( 2 ) )
   {
      HB_ERRCODE uiError = 0;
      HB_SIZE nToRead = hb_parns( 2 );

      if( nToRead > 0 )
      {
         char * buffer = ( char * ) hb_xgrab( nToRead + 1 );
         HB_SIZE nRead;

         nRead = hb_fileRead( pFile, buffer, nToRead, hb_parnintdef( 3, -1 ) );
         uiError = hb_fsError();
         buffer[ nRead ] = '\0';

         hb_retclen_buffer( buffer, nRead );
      }
      else
         hb_retc_null();

      hb_fsSetFError( uiError );
   }
}

/* hb_VFWrite( <pHandle>, <cBuff>, [ <nToWrite> ], [ <nTimeOut> ] ) -> <nWritten> */
HB_FUNC( HB_VFWRITE )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      HB_ERRCODE uiError = 0;

      if( HB_ISCHAR( 2 ) )
      {
         HB_SIZE nLen = hb_parclen( 2 );

         if( HB_ISNUM( 3 ) )
         {
            HB_SIZE nWrite = hb_parns( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         hb_retns( hb_fileWrite( pFile, hb_parc( 2 ), nLen,
                                 hb_parnintdef( 4, -1 ) ) );
         uiError = hb_fsError();
      }
      else
         hb_retni( 0 );

      hb_fsSetFError( uiError );
   }
}

/* hb_VFReadAt( <pHandle>, @<cBuff>, [ <nToRead> ], [ <nAtOffset> ] ) -> <nRead> */
HB_FUNC( HB_VFREADAT )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
      HB_ERRCODE uiError = 0;
      HB_SIZE nRead = 0;
      HB_SIZE nSize;
      char * buffer;

      if( pBuffer && HB_ISBYREF( 2 ) &&
          hb_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
      {
         if( HB_ISNUM( 3 ) )
         {
            nRead = hb_parns( 3 );
            if( nRead < nSize )
               nSize = nRead;
         }
         nRead = hb_fileReadAt( pFile, buffer, nSize,
                                ( HB_FOFFSET ) hb_parnintdef( 4, -1 ) );
         uiError = hb_fsError();
      }

      hb_retns( nRead );
      hb_fsSetFError( uiError );
   }
}

/* hb_VFWriteAt( <pHandle>, <cBuff>, [ <nToWrite> ], [ <nAtOffset> ] ) -> <nWritten> */
HB_FUNC( HB_VFWRITEAT )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      HB_ERRCODE uiError = 0;
      const char * pszData = hb_parc( 2 );

      if( pszData )
      {
         HB_SIZE nLen = hb_parclen( 2 );

         if( HB_ISNUM( 3 ) )
         {
            HB_SIZE nWrite = hb_parns( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         hb_retns( hb_fileWriteAt( pFile, pszData, nLen,
                                   ( HB_FOFFSET ) hb_parnintdef( 4, -1 ) ) );
         uiError = hb_fsError();
      }
      else
         hb_retni( 0 );

      hb_fsSetFError( uiError );
   }
}

/* hb_VFSeek( <pHandle>, <nOffset>, [ <nWhence> ] ) -> <nOffset> */
HB_FUNC( HB_VFSEEK )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      HB_ERRCODE uiError = 0;

      if( HB_ISNUM( 2 ) )
      {
         hb_retnint( hb_fileSeek( pFile, ( HB_FOFFSET ) hb_parnint( 2 ),
                                  ( HB_USHORT ) hb_parnidef( 3, FS_SET ) ) );
         uiError = hb_fsError();
      }
      else
         hb_retni( 0 );

      hb_fsSetFError( uiError );
   }
}

/* hb_VFTrunc( <pHandle>, [ <nAtOffset> ] ) -> <lOK> */
HB_FUNC( HB_VFTRUNC )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      hb_retl( hb_fileTruncAt( pFile, ( HB_FOFFSET ) hb_parnint( 2 ) ) );
      hb_fsSetFError( hb_fsError() );
   }
}

/* hb_VFSize( <pHandle> ) -> <nSize> */
HB_FUNC( HB_VFSIZE )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      hb_retnint( hb_fileSize( pFile ) );
      hb_fsSetFError( hb_fsError() );
   }
}

/* hb_VFEof( <pHandle> ) -> <lEOF> */
HB_FUNC( HB_VFEOF )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      hb_retl( hb_fileEof( pFile ) );
      hb_fsSetFError( hb_fsError() );
   }
}

/* hb_VFFlush( <pHandle>, [ <lDirtyOnly> ] ) -> NIL */
HB_FUNC( HB_VFFLUSH )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      hb_fileFlush( pFile, hb_parl( 2 ) );
      hb_fsSetFError( hb_fsError() );
   }
}

/* hb_VFCommit( <pHandle> ) -> NIL */
HB_FUNC( HB_VFCOMMIT )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      hb_fileCommit( pFile );
      hb_fsSetFError( hb_fsError() );
   }
}

/* hb_VFConfig( <pHandle>, <nSet>, [ <nParam> ] ) -> <nResult> */
HB_FUNC( HB_VFCONFIG )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
   {
      if( HB_ISNUM( 2 ) )
      {
         PHB_ITEM pValue = hb_itemNew( hb_param( 3, HB_IT_ANY ) );

         hb_fileConfigure( pFile, hb_parni( 2 ), pValue );
         hb_fsSetFError( hb_fsError() );
         hb_itemReturnRelease( pValue );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 2021, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* hb_VFHandle( <pHandle> ) -> <nOsHandle> */
HB_FUNC( HB_VFHANDLE )
{
   PHB_FILE pFile = hb_fileParam( 1 );

   if( pFile )
      hb_retnint( ( HB_NHANDLE ) hb_fileHandle( pFile ) );
}

/* hb_VFTempFile( @<cFileName>, [ <cDir> ], [ <cPrefix> ], [ <cExt> ], [ <nAttr> ] ) -> <pHandle> | NIL */
HB_FUNC( HB_VFTEMPFILE )
{
   char szName[ HB_PATH_MAX ];

   hb_fileReturn( hb_fileCreateTempEx( szName,
                                       hb_parc( 2 ), /* pszDir */
                                       hb_parc( 3 ), /* pszPrefix */
                                       hb_parc( 4 ), /* pszExt */
                                       ( HB_FATTR ) hb_parnldef( 5, FC_NORMAL ) ) );
   hb_fsSetFError( hb_fsError() );
   hb_storc( szName, 1 );
}
