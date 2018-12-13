/*
 * I/O driver for TCP streams
 *
 * Copyright 2014 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbinit.h"

#include "hbsocket.h"

#define FILE_PREFIX      "TCP:"
#define FILE_PREFIX_LEN  strlen( FILE_PREFIX )

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   PHB_SOCKEX            sock;
   HB_BOOL               fEof;
   HB_MAXINT             timeout;
}
HB_FILE;

static PHB_FILE s_fileNew( PHB_SOCKEX sock, HB_MAXINT timeout );

static HB_BOOL s_fileAccept( PHB_FILE_FUNCS pFuncs, const char * pszFileName )
{
   HB_SYMBOL_UNUSED( pFuncs );

   return hb_strnicmp( pszFileName, FILE_PREFIX, FILE_PREFIX_LEN ) == 0;
}

static PHB_FILE s_fileOpen( PHB_FILE_FUNCS pFuncs, const char * pszName,
                            const char * pszDefExt, HB_FATTR nExFlags,
                            const char * pPaths, PHB_ITEM pError )
{
   const char * pszHost = pszName + FILE_PREFIX_LEN, * ptr;
   PHB_FILE pFile = NULL;
   HB_ERRCODE errcode = 0;
   HB_SIZE nLen = 0;
   int iPort = 0;
   HB_MAXINT timeout = -1;

   HB_SYMBOL_UNUSED( pFuncs );
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

      hb_socketAutoInit();

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
                  PHB_SOCKEX sock;

                  switch( nExFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) )
                  {
                     case FO_READ:
                        hb_socketShutdown( sd, HB_SOCKET_SHUT_WR );
                        break;
                     case FO_WRITE:
                        hb_socketShutdown( sd, HB_SOCKET_SHUT_RD );
                        break;
                  }
                  sock = hb_sockexNew( sd, NULL, NULL );
                  if( sock )
                  {
                     hb_sockexSetShutDown( sock, HB_TRUE );
                     hb_sockexSetAutoFlush( sock, HB_TRUE );
                     pFile = s_fileNew( sock, timeout );
                     sd = HB_NO_SOCKET;
                  }
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
   hb_sockexClose( pFile->sock, HB_TRUE );
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
      if( timeout == -1 )
         timeout = pFile->timeout;
      lRead = hb_sockexRead( pFile->sock, data, lRead, timeout );

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
   long lSent = nSize > LONG_MAX ? LONG_MAX : ( long ) nSize;
   HB_ERRCODE errcode;

   if( timeout == -1 )
      timeout = pFile->timeout;
   lSent = hb_sockexWrite( pFile->sock, data, lSent, timeout );
   errcode = hb_socketGetError();
   hb_fsSetError( errcode );

   if( lSent < 0 )
   {
      switch( errcode )
      {
         case HB_SOCKET_ERR_TIMEOUT:
         case HB_SOCKET_ERR_AGAIN:
         case HB_SOCKET_ERR_TRYAGAIN:
            lSent = 0;
            break;
      }
   }

   return lSent;
}

static HB_BOOL s_fileEof( PHB_FILE pFile )
{
   hb_fsSetError( 0 );
   return pFile->fEof;
}

static void s_fileFlush( PHB_FILE pFile, HB_BOOL fDirty )
{
   HB_SYMBOL_UNUSED( fDirty );

   hb_sockexFlush( pFile->sock, pFile->timeout, HB_FALSE );
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
         HB_SOCKET sd = hb_sockexGetHandle( pFile->sock );

         if( HB_IS_NUMERIC( pValue ) && sd != HB_NO_SOCKET )
         {
            switch( hb_itemGetNI( pValue ) )
            {
               case FO_READ:
                  hb_socketShutdown( sd, HB_SOCKET_SHUT_RD );
                  break;
               case FO_WRITE:
                  hb_socketShutdown( sd, HB_SOCKET_SHUT_WR );
                  break;
               case FO_READWRITE:
                  hb_socketShutdown( sd, HB_SOCKET_SHUT_RDWR );
                  break;
            }
         }
         hb_itemClear( pValue );
         return HB_TRUE;
      }
      case HB_VF_RDHANDLE:
      case HB_VF_WRHANDLE:
         hb_itemPutNInt( pValue, ( HB_NHANDLE ) hb_sockexGetHandle( pFile->sock ) );
         return HB_TRUE;

      case HB_VF_IONAME:
         hb_itemPutC( pValue, FILE_PREFIX );
         return HB_TRUE;
   }

   return HB_FALSE;
}

static HB_FHANDLE s_fileHandle( PHB_FILE pFile )
{
   return ( HB_FHANDLE ) ( pFile ? hb_sockexGetHandle( pFile->sock ) : HB_NO_SOCKET );
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
   s_fileFlush,
   NULL, /* s_fileCommit */
   s_fileConfigure,
   s_fileHandle
};

static PHB_FILE s_fileNew( PHB_SOCKEX sock, HB_MAXINT timeout )
{
   PHB_FILE pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );

   pFile->pFuncs  = &s_fileFuncs;
   pFile->sock    = sock;
   pFile->fEof    = HB_FALSE;
   pFile->timeout = timeout;

   return pFile;
}

static PHB_FILE hb_fileFromSocket( PHB_SOCKEX sock, HB_MAXINT timeout )
{
   return sock && hb_sockexGetHandle( sock ) != HB_NO_SOCKET ? s_fileNew( sock, timeout ) : NULL;
}

HB_FUNC( HB_VFFROMSOCKET )
{
   PHB_SOCKEX sock = hb_sockexParam( 1 );
   PHB_FILE pFile = hb_fileFromSocket( sock, hb_parnintdef( 2, -1 ) );

   if( pFile )
   {
      hb_sockexItemClear( hb_param( 1, HB_IT_POINTER ) );
      hb_fileItemPut( hb_param( -1, HB_IT_ANY ), pFile );
   }
}

HB_FUNC( HB_TCPIO ) {}


HB_CALL_ON_STARTUP_BEGIN( _hb_file_tcpio_init_ )
   hb_fileRegisterPart( &s_fileFuncs );
HB_CALL_ON_STARTUP_END( _hb_file_tcpio_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_file_tcpio_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _hb_file_tcpio_init_ )
   #include "hbiniseg.h"
#endif
