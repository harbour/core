/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server.
 *    All files which names starts 'net:' are redirected to this API.
 *    This is server code giving the following .prg functions:
 *       NETIO_LISTEN( [<nPort>], [<cAddress>], [<cRootDir>] )
 *                                              -> <pListenSocket> | NIL
 *       NETIO_ACCEPT( <pListenSocket> [, <nTimeOut>] )
 *                                              -> <pConnectionSocket> | NIL
 *       NETIO_SERVER( <pConnectionSocket> ) -> NIL
 *       NETIO_SERVERSTOP( <pListenSocket> | <pConnectionSocket>, <lStop> )
 *                                              -> NIL
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbsocket.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbthread.h"
#include "netio.h"


/*
 * server code
 */

typedef struct _HB_CONSRV
{
   HB_SOCKET      sd;
   PHB_FILE       fileTable[ NETIO_FILES_MAX ];
   int            filesCount;
   int            firstFree;
   BOOL           stop;
   int            rootPathLen;
   char           rootPath[ HB_PATH_MAX ];
}
HB_CONSRV, * PHB_CONSRV;

typedef struct _HB_LISTENSD
{
   HB_SOCKET      sd;
   BOOL           stop;
   char           rootPath[ HB_PATH_MAX ];
}
HB_LISTENSD, * PHB_LISTENSD;

static BOOL s_isDirSep( char c )
{
   /* intentionally used explicit values instead of harbour macros
    * because client can use different OS
    */
   return c == '/' || c == '\\';
}

static const char * s_consrvFilePath( char * pszFileName,
                                      PHB_CONSRV conn )
{
   int iPos = 0, iLevel = 0;
   char ch = HB_OS_PATH_DELIM_CHR;

   if( conn->rootPathLen )
   {
      while( s_isDirSep( pszFileName[ 0 ] ) )
         ++pszFileName;
   }

   while( pszFileName[ iPos ] && iLevel >= 0 )
   {
      if( conn->rootPathLen && s_isDirSep( ch ) )
      {
         if( pszFileName[ iPos ] == '.' &&
             pszFileName[ iPos + 1 ] == '.' &&
             ( pszFileName[ iPos + 2 ] == '\0' ||
               s_isDirSep( pszFileName[ iPos + 2 ] ) ) )
            --iLevel;
         else if( pszFileName[ iPos ] &&
                  !s_isDirSep( pszFileName[ iPos ] ) &&
                  !( pszFileName[ iPos ] == '.' &&
                     s_isDirSep( pszFileName[ iPos + 1 ] ) ) )
            ++iLevel;
      }
      ch = pszFileName[ iPos ];
      if( s_isDirSep( ch ) )
         pszFileName[ iPos ] = HB_OS_PATH_DELIM_CHR;
      ++iPos;
   }

   if( iLevel < 0 )
      pszFileName = NULL;
   else if( conn->rootPathLen )
   {
      memmove( pszFileName + conn->rootPathLen, pszFileName,
               iPos + 1 );
      memcpy( pszFileName, conn->rootPath, conn->rootPathLen );
   }

   return pszFileName;
}

static int s_srvFileNew( PHB_CONSRV conn, PHB_FILE pFile )
{
   if( conn->filesCount < NETIO_FILES_MAX )
   {
      while( conn->firstFree < NETIO_FILES_MAX )
      {
         if( conn->fileTable[ conn->firstFree ] == NULL )
         {
            conn->filesCount++;
            conn->fileTable[ conn->firstFree ] = pFile;
            return conn->firstFree;
         }
         conn->firstFree++;
      }
   }
   return -1;
}

static PHB_FILE s_srvFileFree( PHB_CONSRV conn, int iFile )
{
   if( iFile < NETIO_FILES_MAX && conn->filesCount > 0 )
   {
      PHB_FILE pFile = conn->fileTable[ iFile ];
      if( pFile )
      {
         conn->fileTable[ iFile ] = NULL;
         conn->filesCount--;
         if( conn->firstFree > iFile )
            conn->firstFree = iFile;
         return pFile;
      }
   }

   return NULL;
}

static PHB_FILE s_srvFileGet( PHB_CONSRV conn, int iFile )
{
   if( iFile < NETIO_FILES_MAX && conn->filesCount > 0 )
      return conn->fileTable[ iFile ];
   else
      return NULL;
}

static void s_consrv_close( PHB_CONSRV conn )
{
   int i = 0;

   if( conn->sd != HB_NO_SOCKET )
      hb_socketClose( conn->sd );

   while( conn->filesCount > 0 )
   {
      if( i >= NETIO_FILES_MAX )
         break;   /* internal error, it should not happen */

      if( conn->fileTable[ i ] )
      {
         hb_fileClose( conn->fileTable[ i ] );
         conn->filesCount--;
      }
      ++i;
   }
   conn->firstFree = 0;
   hb_xfree( conn );
}

static HB_GARBAGE_FUNC( s_consrv_destructor )
{
   PHB_CONSRV * conn_ptr = ( PHB_CONSRV * ) Cargo;

   if( *conn_ptr )
   {
      PHB_CONSRV conn = *conn_ptr;
      *conn_ptr = NULL;
      s_consrv_close( conn );
   }
}

static PHB_CONSRV s_consrvParam( int iParam )
{
   PHB_CONSRV * conn_ptr = ( PHB_CONSRV * ) hb_parptrGC( s_consrv_destructor,
                                                         iParam );

   if( conn_ptr && *conn_ptr )
      return *conn_ptr;

   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static void s_consrvRet( PHB_CONSRV conn )
{
   if( conn )
   {
      PHB_CONSRV * conn_ptr = ( PHB_CONSRV * ) hb_gcAlloc( sizeof( PHB_CONSRV ),
                                                           s_consrv_destructor );
      *conn_ptr = conn;
      hb_retptrGC( conn_ptr );
   }
   else
      hb_ret();
}

static PHB_CONSRV s_consrvNew( HB_SOCKET connsd, const char * szRootPath )
{
   PHB_CONSRV conn = ( PHB_CONSRV ) memset( hb_xgrab( sizeof( HB_CONSRV ) ),
                                            0, sizeof( HB_CONSRV ) );
   conn->sd = connsd;
   if( szRootPath )
   {
      hb_strncpy( conn->rootPath, szRootPath, sizeof( conn->rootPath ) - 1 );
      conn->rootPathLen = ( int ) strlen( conn->rootPath );
   }

   return conn;
}

static long s_srvRecvAll( PHB_CONSRV conn, void * buffer, long len )
{
   BYTE * ptr = ( BYTE * ) buffer;
   long lRead = 0, l;

   while( lRead < len && !conn->stop )
   {
      l = hb_socketRecv( conn->sd, ptr + lRead, len - lRead, 0, 1000 );
      if( l <= 0 )
      {
         if( hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT ||
             hb_vmRequestQuery() != 0 )
            break;
      }
      else
         lRead += l;
   }

   return lRead;
}

static long s_srvSendAll( PHB_CONSRV conn, void * buffer, long len )
{
   BYTE * ptr = ( BYTE * ) buffer;
   long lSent = 0, l;

   while( lSent < len && !conn->stop )
   {
      l = hb_socketSend( conn->sd, ptr + lSent, len - lSent, 0, 1000 );
      if( l <= 0 )
      {
         if( hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT ||
             hb_vmRequestQuery() != 0 )
            break;
      }
      else
         lSent += l;
   }

   return lSent;
}

static HB_GARBAGE_FUNC( s_listensd_destructor )
{
   PHB_LISTENSD * lsd_ptr = ( PHB_LISTENSD * ) Cargo;

   if( *lsd_ptr )
   {
      PHB_LISTENSD lsd = *lsd_ptr;
      *lsd_ptr = NULL;
      if( lsd->sd != HB_NO_SOCKET )
      {
         hb_socketClose( lsd->sd );
         lsd->sd = HB_NO_SOCKET;
      }
      hb_xfree( lsd );
   }
}

static PHB_LISTENSD s_listenParam( int iParam, BOOL fError )
{
   PHB_LISTENSD * lsd_ptr = ( PHB_LISTENSD * )
                            hb_parptrGC( s_listensd_destructor, iParam );

   if( lsd_ptr && *lsd_ptr )
      return *lsd_ptr;

   if( fError )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static void s_listenRet( HB_SOCKET sd, const char * szRootPath )
{
   if( sd != HB_NO_SOCKET )
   {
      PHB_LISTENSD lsd, * lsd_ptr;
      int iLen;

      lsd = ( PHB_LISTENSD ) memset( hb_xgrab( sizeof( HB_LISTENSD ) ),
                                            0, sizeof( HB_LISTENSD ) );
      lsd->sd = sd;
      if( szRootPath )
         hb_strncpy( lsd->rootPath, szRootPath, sizeof( lsd->rootPath ) - 1 );
      else
         hb_fsBaseDirBuff( lsd->rootPath );
      iLen = ( int ) strlen( lsd->rootPath );
      if( iLen > 0 )
      {
         if( !s_isDirSep( lsd->rootPath[ iLen - 1 ] ) )
         {
            if( iLen == sizeof( lsd->rootPath ) - 1 )
               --iLen;
            lsd->rootPath[ iLen ] = HB_OS_PATH_DELIM_CHR;
         }
      }
      lsd_ptr = ( PHB_LISTENSD * ) hb_gcAlloc( sizeof( PHB_LISTENSD ),
                                               s_listensd_destructor );
      *lsd_ptr = lsd;
      hb_retptrGC( lsd_ptr );
   }
   else
      hb_ret();
}


HB_FUNC( NETIO_SERVERSTOP )
{
   PHB_LISTENSD lsd = s_listenParam( 1, FALSE );
   BOOL fStop = !HB_ISLOG( 2 ) || hb_parl( 2 );

   if( lsd )
      lsd->stop = fStop;
   else
   {
      PHB_CONSRV conn = s_consrvParam( 1 );
      if( conn )
         lsd->stop = fStop;
   }
}

HB_FUNC( NETIO_LISTEN )
{
   static BOOL s_fInit = TRUE;

   int iPort = hb_parnidef( 1, NETIO_DEFAULT_PORT );
   const char * szAddress = hb_parc( 2 );
   const char * szRootPath = hb_parc( 3 );
   void * pSockAddr;
   unsigned uiLen;
   HB_SOCKET sd = HB_NO_SOCKET;

   if( s_fInit )
   {
      hb_socketInit();
      s_fInit = FALSE;
   }

   if( hb_socketInetAddr( &pSockAddr, &uiLen, szAddress, iPort ) )
   {
      sd = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0 );
      if( sd != HB_NO_SOCKET )
      {
         if( hb_socketBind( sd, pSockAddr, uiLen ) != 0 ||
             hb_socketListen( sd, 10 ) != 0 )
         {
            hb_socketClose( sd );
            sd = HB_NO_SOCKET;
         }
      }
      hb_xfree( pSockAddr );
   }

   s_listenRet( sd, szRootPath );
}

HB_FUNC( NETIO_ACCEPT )
{
   PHB_LISTENSD lsd = s_listenParam( 1, TRUE );
   PHB_CONSRV conn = NULL;

   if( lsd && lsd->sd != HB_NO_SOCKET && !lsd->stop )
   {
      HB_LONG timeout = HB_ISNUM( 2 ) ? hb_parnint( 2 ) : -1;
      HB_SOCKET connsd;

      do
         connsd = hb_socketAccept( lsd->sd, NULL, NULL, timeout < 0 ? 1000 : timeout );
      while( connsd == HB_NO_SOCKET && !lsd->stop && timeout < 0 &&
             hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT &&
             hb_vmRequestQuery() == 0 );

      if( connsd != HB_NO_SOCKET )
      {
         BOOL fOK = FALSE;
         BYTE msgbuf[ 64 ];

         conn = s_consrvNew( connsd, lsd->rootPath );

         if( s_srvRecvAll( conn, msgbuf, NETIO_MSGLEN ) == NETIO_MSGLEN &&
             HB_GET_LE_INT32( msgbuf ) == NETIO_LOGIN )
         {
            long len = HB_GET_LE_INT16( &msgbuf[ 4 ] );

            if( len < ( long ) sizeof( msgbuf ) &&
                len == ( long ) strlen( NETIO_LOGINSTRID ) &&
                s_srvRecvAll( conn, msgbuf, len ) == len )
            {
               if( memcmp( NETIO_LOGINSTRID, msgbuf, len ) == 0 )
               {
                  HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_LOGIN );
                  HB_PUT_LE_UINT32( &msgbuf[ 4 ], NETIO_CONNECTED );
                  memset( msgbuf + 8, '\0', NETIO_MSGLEN - 8 );
                  if( s_srvSendAll( conn, msgbuf, NETIO_MSGLEN ) == NETIO_MSGLEN )
                     fOK = TRUE;
               }
            }
         }

         if( !fOK )
         {
            s_consrv_close( conn );
            conn = NULL;
         }
      }
   }

   s_consrvRet( conn );
}

HB_FUNC( NETIO_SERVER )
{
   PHB_CONSRV conn = s_consrvParam( 1 );

   if( conn && conn->sd != HB_NO_SOCKET && !conn->stop )
   {
      for( ;; )
      {
         BYTE msgbuf[ NETIO_MSGLEN ], buffer[ 2048 ], * ptr = NULL, * msg;
         BOOL fNoAnswer = FALSE;
         long len = 0, size;
         int iError = 0, iFileNo;
         USHORT uiFalgs, uiFsError;
         char * szExt;
         PHB_FILE pFile;
         HB_FOFFSET llOffset, llSize;

         msg = buffer;

         if( s_srvRecvAll( conn, msgbuf, NETIO_MSGLEN ) != NETIO_MSGLEN )
            break;

         switch( HB_GET_LE_UINT32( msgbuf ) )
         {
            case NETIO_EXISTS:
               size = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               if( size <= 0 )
                  iError = NETIO_ERR_WRONG_PARAM;
               else
               {
                  if( size + conn->rootPathLen >= ( long ) sizeof( buffer ) )
                     ptr = msg = ( BYTE * ) hb_xgrab( size + conn->rootPathLen + 1 );
                  msg[ size ] = '\0';
                  if( s_srvRecvAll( conn, msg, size ) != size )
                     iError = NETIO_ERR_READ;
                  else
                  {
                     const char * szFile = s_consrvFilePath( ( char * ) msg, conn );

                     if( !szFile )
                        iError = NETIO_ERR_WRONG_FILE_PATH;
                     else if( !hb_fileExists( ( const char * ) msg, NULL ) )
                        iError = hb_fsError();
                     else
                     {
                        HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_EXISTS );
                        memset( msg + 4, '\0', NETIO_MSGLEN - 4 );
                     }
                  }
               }
               break;

            case NETIO_DELETE:
               size = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               if( size <= 0 )
                  iError = NETIO_ERR_WRONG_PARAM;
               else
               {
                  if( size + conn->rootPathLen >= ( long ) sizeof( buffer ) )
                     ptr = msg = ( BYTE * ) hb_xgrab( size + conn->rootPathLen + 1 );
                  msg[ size ] = '\0';
                  if( s_srvRecvAll( conn, msg, size ) != size )
                     iError = NETIO_ERR_READ;
                  else
                  {
                     const char * szFile = s_consrvFilePath( ( char * ) msg, conn );

                     if( !szFile )
                        iError = NETIO_ERR_WRONG_FILE_PATH;
                     else if( !hb_fileDelete( ( const char * ) msg ) )
                        iError = hb_fsError();
                     else
                     {
                        HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_DELETE );
                        memset( msg + 4, '\0', NETIO_MSGLEN - 4 );
                     }
                  }
               }
               break;

            case NETIO_OPEN:
               uiFalgs = HB_GET_LE_UINT16( &msgbuf[ 6 ] );
               szExt = msgbuf[ 8 ] ? hb_strndup( ( const char * ) &msgbuf[ 8 ],
                                                 NETIO_MSGLEN - 8 ) : NULL;
               size = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               if( size <= 0 )
                  iError = NETIO_ERR_WRONG_PARAM;
               else
               {
                  if( size + conn->rootPathLen >= ( long ) sizeof( buffer ) )
                     ptr = msg = ( BYTE * ) hb_xgrab( size + conn->rootPathLen + 1 );
                  msg[ size ] = '\0';
                  if( s_srvRecvAll( conn, msg, size ) != size )
                     iError = NETIO_ERR_READ;
                  else if( conn->filesCount >= NETIO_FILES_MAX )
                     iError = NETIO_ERR_FILES_MAX;
                  else
                  {
                     const char * szFile = s_consrvFilePath( ( char * ) msg, conn );

                     if( !szFile )
                        iError = NETIO_ERR_WRONG_FILE_PATH;
                     else
                     {
                        pFile = hb_fileExtOpen( szFile, szExt, uiFalgs, NULL, NULL );
                        if( !pFile )
                           iError = hb_fsError();
                        else
                        {
                           iFileNo = s_srvFileNew( conn, pFile );
                           if( iFileNo < 0 )
                           {
                              iError = NETIO_ERR_FILES_MAX;
                              hb_fileClose( pFile );
                           }
                           else
                           {
                              HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_OPEN );
                              HB_PUT_LE_UINT16( &msg[ 4 ], iFileNo );
                              memset( msg + 6, '\0', NETIO_MSGLEN - 6 );
                           }
                        }
                     }
                  }
               }
               if( szExt )
                  hb_xfree( szExt );
               break;

            case NETIO_READ:
               iFileNo = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               size = HB_GET_LE_UINT32( &msgbuf[ 6 ] );
               llOffset = HB_GET_LE_INT64( &msgbuf[ 10 ] );
               if( size < 0 )
                  iError = NETIO_ERR_WRONG_PARAM;
               else
               {
                  if( size >= ( long ) ( sizeof( buffer ) - NETIO_MSGLEN ) )
                     ptr = msg = ( BYTE * ) hb_xgrab( size + NETIO_MSGLEN );
                  pFile = s_srvFileGet( conn, iFileNo );
                  if( pFile == NULL )
                     iError = NETIO_ERR_WRONG_FILE_HANDLE;
                  else
                  {
                     len = hb_fileReadAt( pFile, msg + NETIO_MSGLEN, size, llOffset );
                     uiFsError = hb_fsError();
                     HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_READ );
                     HB_PUT_LE_UINT32( &msg[ 4 ], len );
                     HB_PUT_LE_UINT32( &msg[ 8 ], uiFsError );
                     memset( msg + 10, '\0', NETIO_MSGLEN - 10 );
                  }
               }
               break;

            case NETIO_WRITE:
               iFileNo = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               size = HB_GET_LE_UINT32( &msgbuf[ 6 ] );
               llOffset = HB_GET_LE_INT64( &msgbuf[ 10 ] );
               if( size < 0 )
                  iError = NETIO_ERR_WRONG_PARAM;
               else
               {
                  if( size >= ( long ) sizeof( buffer ) )
                     ptr = msg = ( BYTE * ) hb_xgrab( size );
                  if( s_srvRecvAll( conn, msg, size ) != size )
                     iError = NETIO_ERR_READ;
                  else
                  {
                     pFile = s_srvFileGet( conn, iFileNo );
                     if( pFile == NULL )
                        iError = NETIO_ERR_WRONG_FILE_HANDLE;
                     else
                     {
                        size = hb_fileWriteAt( pFile, msg, size, llOffset );
                        uiFsError = hb_fsError();
                        HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_WRITE );
                        HB_PUT_LE_UINT32( &msg[ 4 ], size );
                        HB_PUT_LE_UINT32( &msg[ 8 ], uiFsError );
                        memset( msg + 10, '\0', NETIO_MSGLEN - 10 );
                     }
                  }
               }
               break;

            case NETIO_LOCK:
               iFileNo = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               llOffset = HB_GET_LE_INT64( &msgbuf[ 6 ] );
               llSize = HB_GET_LE_INT64( &msgbuf[ 14 ] );
               uiFalgs = HB_GET_LE_UINT16( &msgbuf[ 22 ] );
               pFile = s_srvFileGet( conn, iFileNo );
               if( pFile == NULL )
                  iError = NETIO_ERR_WRONG_FILE_HANDLE;
               else if( !hb_fileLock( pFile, llOffset, llSize, uiFalgs ) )
                  iError = hb_fsError();
               else
               {
                  HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_LOCK );
                  memset( msg + 4, '\0', NETIO_MSGLEN - 4 );
               }
               break;

            case NETIO_TRUNC:
               iFileNo = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               llOffset = HB_GET_LE_INT64( &msgbuf[ 6 ] );
               pFile = s_srvFileGet( conn, iFileNo );
               if( pFile == NULL )
                  iError = NETIO_ERR_WRONG_FILE_HANDLE;
               else if( !hb_fileTruncAt( pFile, llOffset ) )
                  iError = hb_fsError();
               else
               {
                  HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_TRUNC );
                  memset( msg + 4, '\0', NETIO_MSGLEN - 4 );
               }
               break;

            case NETIO_SIZE:
               iFileNo = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               pFile = s_srvFileGet( conn, iFileNo );
               if( pFile == NULL )
                  iError = NETIO_ERR_WRONG_FILE_HANDLE;
               else
               {
                  llOffset = hb_fileSize( pFile );
                  uiFsError = hb_fsError();
                  HB_PUT_LE_UINT32( &msg[  0 ], NETIO_SIZE );
                  HB_PUT_LE_UINT64( &msg[  4 ], llOffset );
                  HB_PUT_LE_UINT32( &msg[ 12 ], uiFsError );
                  memset( msg + 14, '\0', NETIO_MSGLEN - 14 );
               }
               break;

            case NETIO_CLOSE:
               iFileNo = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               pFile = s_srvFileFree( conn, iFileNo );
               if( pFile == NULL )
                  iError = NETIO_ERR_WRONG_FILE_HANDLE;
               else
               {
                  hb_fileClose( pFile );
                  HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_CLOSE );
                  memset( msg + 4, '\0', NETIO_MSGLEN - 4 );
               }
               break;

            case NETIO_COMMIT:
               iFileNo = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
               pFile = s_srvFileGet( conn, iFileNo );
               if( pFile )
                  hb_fileCommit( pFile );
               fNoAnswer = TRUE;
               break;

            default: /* unkown message */
               iError = NETIO_ERR_UNKNOWN_COMMAND;
               break;
         }

         if( !fNoAnswer )
         {
            if( iError != 0 )
            {
               HB_PUT_LE_UINT32( &msg[ 0 ], NETIO_ERROR );
               HB_PUT_LE_UINT16( &msg[ 4 ], iError );
               memset( msg + 6, '\0', NETIO_MSGLEN - 6 );
               len = NETIO_MSGLEN;
            }
            else
               len += NETIO_MSGLEN;

            if( s_srvSendAll( conn, msg, len ) != len )
               break;
         }

         if( ptr )
            hb_xfree( ptr );
      }
   }
}
