/*
 * Harbour Project source code:
 *   I/O driver for TCP streams
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
#include "hbapierr.h"
#include "hbinit.h"

#include "hbsocket.h"

#define FILE_PREFIX      "tcp:"
#define FILE_PREFIX_LEN  strlen( FILE_PREFIX )

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   HB_SOCKET             sd;
   HB_BOOL               fEof;
   int                   timeout;
} HB_FILE;

static PHB_FILE s_fileNew( HB_SOCKET sd, int timeout );

static HB_BOOL s_fileAccept( const char * pszFileName )
{
   return hb_strnicmp( pszFileName, FILE_PREFIX, FILE_PREFIX_LEN ) == 0;
}

static PHB_FILE s_fileOpen( const char * pszName, const char * pszDefExt,
                            HB_USHORT uiExFlags,
                            const char * pPaths, PHB_ITEM pError )
{
   const char * pszHost = pszName + FILE_PREFIX_LEN, * ptr;
   PHB_FILE pFile = NULL;
   HB_ERRCODE errcode = 0;
   HB_SIZE nLen = 0;
   int iPort = 0;
   int timeout = -1;

   HB_SYMBOL_UNUSED( pszDefExt );
   HB_SYMBOL_UNUSED( pPaths );

   if( ( ptr = strchr( pszHost, ':' ) ) != NULL && ptr != pszHost )
   {
      nLen = ptr - pszHost;
      ++ptr;
      while( HB_ISDIGIT( * ptr ) )
         iPort = iPort * 10 + ( * ptr++ - '0' );

      if( * ptr == ':' )
      {
         ++ptr;
         while( HB_ISDIGIT( * ptr ) )
            timeout = HB_MAX( timeout, 0 ) * 10 + ( * ptr++ - '0' );
      }

      if( * ptr != 0 && * ptr != ':' )
         iPort = 0;
   }

   if( iPort > 0 )
   {
      char * pszAddr, * pszIpAddr;

      pszAddr = hb_strndup( pszHost, nLen );
      pszIpAddr = hb_socketResolveAddr( pszAddr, HB_SOCKET_AF_INET );
      hb_xfree( pszAddr );

      if( pszIpAddr )
      {
         HB_SOCKET sd = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0 );
         if( sd != HB_NO_SOCKET )
         {
            void * pSockAddr;
            unsigned uiLen;

            if( hb_socketInetAddr( &pSockAddr, &uiLen, pszIpAddr, iPort ) )
            {
               hb_socketSetKeepAlive( sd, HB_TRUE );
               if( hb_socketConnect( sd, pSockAddr, uiLen, timeout ) == 0 )
               {
                  switch( uiExFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) )
                  {
                     case FO_READ:
                        hb_socketShutdown( sd, HB_SOCKET_SHUT_WR );
                        break;
                     case FO_WRITE:
                        hb_socketShutdown( sd, HB_SOCKET_SHUT_RD );
                        break;
                  }
                  pFile = s_fileNew( sd, timeout );
                  sd = HB_NO_SOCKET;
               }
               hb_xfree( pSockAddr );
            }
            if( sd != HB_NO_SOCKET )
            {
               errcode = hb_socketGetError();
               hb_socketClose( sd );
            }
         }
         hb_xfree( pszIpAddr );
      }
      if( errcode == 0 && pFile == NULL )
         errcode = hb_socketGetError();
   }
   else
      errcode = HB_SOCKET_ERR_WRONGADDR;

   hb_fsSetError( errcode );

   if( pError )
   {
      hb_errPutFileName( pError, pszName );
      if( pFile == NULL )
      {
         hb_errPutOsCode( pError, errcode );
         hb_errPutGenCode( pError, ( HB_ERRCODE ) EG_OPEN );
      }
   }

   return pFile;
}

static void s_fileClose( PHB_FILE pFile )
{
   hb_socketClose( pFile->sd );
   hb_fsSetError( hb_socketGetError() );
   hb_xfree( pFile );
}

static HB_SIZE s_fileRead( PHB_FILE pFile, void * data,
                           HB_SIZE nSize, HB_MAXINT timeout )
{
   HB_ERRCODE errcode = 0;
   long lRead = 0;

   if( ! pFile->fEof )
   {
      lRead = nSize > LONG_MAX ? LONG_MAX : ( long ) nSize;
      lRead = hb_socketRecv( pFile->sd, data, lRead, 0, timeout );

      errcode = hb_socketGetError();

      if( lRead <= 0 )
      {
         switch( errcode )
         {
            case HB_SOCKET_ERR_TIMEOUT:
            case HB_SOCKET_ERR_AGAIN:
            case HB_SOCKET_ERR_TRYAGAIN:
               break;
            default:
               pFile->fEof = HB_TRUE;
               break;
         }
         lRead = 0;
      }
   }
   hb_fsSetError( errcode );

   return lRead;
}

static HB_SIZE s_fileWrite( PHB_FILE pFile, const void * data,
                            HB_SIZE nSize, HB_MAXINT timeout )
{
   long lSend = nSize > LONG_MAX ? LONG_MAX : ( long ) nSize;

   lSend = hb_socketSend( pFile->sd, data, lSend, 0, timeout );
   hb_fsSetError( hb_socketGetError() );

   if( lSend < 0 )
      lSend = 0;

   return lSend;
}

static HB_BOOL s_fileEof( PHB_FILE pFile )
{
   hb_fsSetError( 0 );
   return pFile->fEof;
}

static HB_BOOL s_fileConfigure( PHB_FILE pFile, int iIndex, PHB_ITEM pValue )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( iIndex );
   HB_SYMBOL_UNUSED( pValue );

   return HB_FALSE;
}

static HB_FHANDLE s_fileHandle( PHB_FILE pFile )
{
   return ( HB_FHANDLE ) ( pFile ? pFile->sd : HB_NO_SOCKET );
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

static PHB_FILE s_fileNew( HB_SOCKET sd, int timeout )
{
   PHB_FILE pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );

   pFile->pFuncs  = &s_fileFuncs;
   pFile->sd      = sd;
   pFile->fEof    = HB_FALSE;
   pFile->timeout = timeout;

   return pFile;
}

HB_FUNC( HB_TCPIO ) { ; }


HB_CALL_ON_STARTUP_BEGIN( _hb_file_tcpio_init_ )
   hb_fileRegisterPart( &s_fileFuncs );
HB_CALL_ON_STARTUP_END( _hb_file_tcpio_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_file_tcpio_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _hb_file_tcpio_init_ )
   #include "hbiniseg.h"
#endif
