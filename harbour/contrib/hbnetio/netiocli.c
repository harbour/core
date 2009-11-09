/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server.
 *    All files which names starts 'net:' are redirected to this API.
 *    This is client code with
 *       NETIO_CONNECT( [<cServer>], [<cPort>], [<nTimeOut>] ) -> <lOK>
 *    function which register alternative RDD IO API, sets server
 *    address and port and connection timeout parameter.
 *    Then it tries to connect to the server and returns .T. on success.
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

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_IMPLEMENTATION_

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbsocket.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbthread.h"
#include "netio.h"

/*
 * client code
 */

typedef struct _HB_CONCLI
{
   HB_COUNTER     used;
   PHB_ITEM       mutex;
   int            timeout;
   int            port;
   HB_SOCKET      sd;
   struct _HB_CONCLI * next;
   char           server[ 1 ];
}
HB_CONCLI, * PHB_CONCLI;

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   PHB_CONCLI     conn;
   USHORT         fd;
}
HB_FILE;

typedef struct
{
   int      timeout;
   int      port;
   char     server[ NETIO_SERVERNAME_MAX ];
} HB_CONDATA, * PHB_CONDATA;

/* MT macros */
#define HB_NETIO_LOCK         hb_threadEnterCriticalSection( &s_netioMtx );
#define HB_NETIO_UNLOCK       hb_threadLeaveCriticalSection( &s_netioMtx );
static HB_CRITICAL_NEW( s_netioMtx );

static HB_TSD_NEW( s_conData, sizeof( HB_CONDATA ), NULL, NULL );

static PHB_CONCLI s_connections = NULL;

static BOOL s_defaultInit = TRUE;
static char s_defaultServer[ NETIO_SERVERNAME_MAX ] = NETIO_DEFAULT_SERVER;
static int  s_defaultPort = NETIO_DEFAULT_PORT;
static int  s_defaultTimeOut = NETIO_DEFAULT_TIMEOUT;

static BOOL s_fInit = TRUE;


static const HB_FILE_FUNCS * s_fileMethods( void );

#define NETIO_TIMEOUT   -1

static long s_fileRecvAll( PHB_CONCLI conn, void * buffer, long len )
{
   BYTE * ptr = ( BYTE * ) buffer;
   long lRead = 0, l;

   while( lRead < len )
   {
      l = hb_socketRecv( conn->sd, ptr + lRead, len - lRead, 0, NETIO_TIMEOUT );
      if( l <= 0 )
         break;
      lRead += l;
   }
   return lRead;
}

static BOOL s_fileSendMsg( PHB_CONCLI conn, BYTE * msgbuf,
                           const void * data, long len, BOOL fWait )
{
   BYTE buffer[ 2048 ];
   BYTE * msg, * ptr = NULL;
   LONG lSent = 0, l;
   BOOL fResult = FALSE;

   if( len == 0 )
   {
      msg = msgbuf;
      len = NETIO_MSGLEN;
   }
   else
   {
      len += NETIO_MSGLEN;
      if( len > ( long ) sizeof( buffer ) )
         msg = ptr = ( BYTE * ) hb_xgrab( len );
      else
         msg = buffer;
      memcpy( msg, msgbuf, NETIO_MSGLEN );
      memcpy( msg + NETIO_MSGLEN, data, len - NETIO_MSGLEN );
   }

   while( lSent < len )
   {
      l = hb_socketSend( conn->sd, msg + lSent, len - lSent, 0, NETIO_TIMEOUT );
      if( l <= 0 )
         break;
      lSent += l;
   }

   if( ptr )
      hb_xfree( ptr );

   if( lSent == len )
   {
      if( fWait )
      {
         int iMsg = HB_GET_LE_INT32( msgbuf );

         while( s_fileRecvAll( conn, msgbuf, NETIO_MSGLEN ) == NETIO_MSGLEN )
         {
            int iResult = HB_GET_LE_INT32( msgbuf );

            if( iResult != NETIO_SYNC )
            {
               if( iResult == NETIO_ERROR )
                  hb_fsSetError( HB_GET_LE_UINT16( &msgbuf[ 4 ] ) );
               else if( iResult == iMsg )
                  fResult = TRUE;
               break;
            }
         }
      }
      else
         fResult = TRUE;
   }

   return fResult;
}

static PHB_CONCLI s_fileConNew( HB_SOCKET sd, const char * pszServer,
                                int iPort, int iTimeOut )
{
   PHB_CONCLI conn;
   int iLen;

   iLen = ( int ) strlen( pszServer );
   conn = ( PHB_CONCLI ) hb_xgrab( sizeof( HB_CONCLI ) + iLen );
   hb_atomic_set( &conn->used, 1 );
   conn->mutex = hb_threadMutexCreate();
   conn->sd = sd;
   conn->next = NULL;
   conn->timeout = iTimeOut;
   conn->port = iPort;
   memcpy( conn->server, pszServer, iLen + 1 );

   return conn;
}

static void s_fileConFree( PHB_CONCLI conn )
{
   hb_socketShutdown( conn->sd, HB_SOCKET_SHUT_RDWR );
   hb_socketClose( conn->sd );
   if( conn->mutex )
      hb_itemRelease( conn->mutex );
   hb_xfree( conn );
}

static void s_fileConRegister( PHB_CONCLI conn )
{
   PHB_CONCLI * connPtr;

   HB_NETIO_LOCK
   connPtr = &s_connections;
   while( *connPtr )
      connPtr = &( *connPtr )->next;
   *connPtr = conn;
   HB_NETIO_UNLOCK
}

static void s_fileConClose( PHB_CONCLI conn )
{
   if( hb_atomic_dec( &conn->used ) )
   {
      HB_NETIO_LOCK
      if( hb_atomic_get( &conn->used ) == 0 )
      {
         PHB_CONCLI * connPtr = &s_connections;
         while( *connPtr )
         {
            if( *connPtr == conn )
            {
               *connPtr = conn->next;
               break;
            }
            connPtr = &( *connPtr )->next;
         }
      }
      else
         conn = NULL;   /* reused by other thread */
      HB_NETIO_UNLOCK

      if( conn )
         s_fileConFree( conn );
   }
}

static PHB_CONCLI s_fileConFind( const char * pszServer, int iPort )
{
   PHB_CONCLI conn;

   HB_NETIO_LOCK
   conn = s_connections;
   while( conn )
   {
      if( conn->port == iPort && hb_stricmp( conn->server, pszServer ) == 0 )
      {
         hb_atomic_inc( &conn->used );
         break;
      }
      conn = conn->next;
   }
   HB_NETIO_UNLOCK

   return conn;
}

static BOOL s_fileConLock( PHB_CONCLI conn )
{
   return !conn->mutex || hb_threadMutexLock( conn->mutex );
}

static void s_fileConUnlock( PHB_CONCLI conn )
{
   if( conn->mutex )
      hb_threadMutexUnlock( conn->mutex );
}

static PHB_CONCLI s_fileConnect( const char ** pszFilename,
                                 const char * pszServer,
                                 int iPort, int iTimeOut )
{
   PHB_CONCLI conn;
   HB_SOCKET sd;
   PHB_CONDATA pConData = ( PHB_CONDATA ) hb_stackGetTSD( &s_conData );
   char server[ NETIO_SERVERNAME_MAX ];
   char * pszIpAddres;

   if( pConData->port )
   {
      pszServer = pConData->server;
      iPort = pConData->port;
      iTimeOut = pConData->timeout;
   }
   else
   {
      if( !pszServer )
         pszServer = s_defaultServer;
      if( !iPort )
         iPort = s_defaultPort;
      if( iTimeOut == 0 || iTimeOut < -1 )
         iTimeOut = s_defaultTimeOut;
   }

   if( pszFilename )
   {
      /* decode server address and port if given as part of file name
       * in format like:
       *          "192.168.0.1:2941:path/to/file"
       */
      const char * psz = strchr( *pszFilename, ':' );
      if( psz )
      {
         int iLen = ( int ) ( psz - *pszFilename );

         if( iLen == 0 || iLen > 1 )
         {
            char port_buf[ 10 ];
            if( iLen >= ( int ) sizeof( server ) )
               iLen = ( int ) sizeof( server ) - 1;
            if( iLen > 0 )
            {
               hb_strncpy( server, *pszFilename, iLen );
               pszServer = server;
            }
            *pszFilename = psz + 1;
            iLen = 0;
            while( HB_ISDIGIT( ( *pszFilename )[ iLen ] ) &&
                   iLen < ( int ) sizeof( port_buf ) - 1 )
            {
               port_buf[ iLen ] = ( *pszFilename )[ iLen ];
               ++iLen;
            }
            if( ( *pszFilename )[ iLen ] == ':' )
            {
               if( iLen == 0 )
                  ++( *pszFilename );
               else if( iLen > 0 )
               {
                  int iOverflow;
                  HB_LONG llPort;

                  port_buf[ iLen ] = '\0';
                  llPort = hb_strValInt( port_buf, &iOverflow );

                  if( !iOverflow && llPort > 0 && llPort < 0x10000 )
                  {
                     *pszFilename += iLen + 1;
                     iPort = ( int ) llPort;
                  }
               }
            }
         }
      }
   }

   pszIpAddres = hb_socketResolveAddr( pszServer, HB_SOCKET_AF_INET );
   if( pszIpAddres == NULL )
      return NULL;

   conn = s_fileConFind( pszIpAddres, iPort );
   if( conn == NULL )
   {
      sd = hb_socketOpen( HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0 );
      if( sd != HB_NO_SOCKET )
      {
         void * pSockAddr;
         unsigned uiLen;

         if( hb_socketInetAddr( &pSockAddr, &uiLen, pszIpAddres, iPort ) )
         {
            hb_socketSetKeepAlive( sd, TRUE );
            if( hb_socketConnect( sd, pSockAddr, uiLen, iTimeOut ) == 0 )
            {
               BYTE msgbuf[ NETIO_MSGLEN ];
               UINT16 len = ( UINT16 ) strlen( NETIO_LOGINSTRID );

               HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_LOGIN );
               HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
               memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );

               hb_socketSetNoDelay( sd, TRUE );
               conn = s_fileConNew( sd, pszIpAddres, iPort, iTimeOut );
               sd = HB_NO_SOCKET;

               if( !s_fileSendMsg( conn, msgbuf, NETIO_LOGINSTRID, len, TRUE ) ||
                   HB_GET_LE_UINT32( &msgbuf[ 4 ] ) != NETIO_CONNECTED )
               {
                  s_fileConFree( conn );
                  conn = NULL;
               }
               else
                  s_fileConRegister( conn );
            }
            hb_xfree( pSockAddr );
         }
         if( !conn && sd != HB_NO_SOCKET )
            hb_socketClose( sd );
      }
   }

   if( conn != NULL && s_defaultInit )
   {
      HB_NETIO_LOCK
      if( s_defaultInit )
      {
         hb_strncpy( s_defaultServer, pszIpAddres, sizeof( s_defaultServer ) - 1 );
         s_defaultPort = iPort;
         s_defaultTimeOut = iTimeOut;
         s_defaultInit = FALSE;
      }
      HB_NETIO_UNLOCK
   }

   hb_xfree( pszIpAddres );

   return conn;
}

static void s_netio_exit( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   while( s_connections )
   {
      PHB_CONCLI conn = s_connections;
      s_connections = conn->next;
      s_fileConFree( conn );
   }

   if( !s_fInit )
   {
      hb_socketCleanup();
      s_fInit = TRUE;
   }
}

static void s_netio_init( void )
{
   if( s_fInit )
   {
      hb_socketInit();
      hb_fileRegister( s_fileMethods() );
      hb_vmAtQuit( s_netio_exit, NULL );
      s_fInit = FALSE;
   }
}

HB_FUNC( NETIO_CONNECT )
{
   const char * pszServer = hb_parc( 1 );
   int iPort = hb_parni( 2 ), iTimeOut = hb_parni( 3 );
   PHB_CONCLI conn;

   s_netio_init();

   conn = s_fileConnect( NULL, pszServer, iPort, iTimeOut );
   if( conn )
   {
      PHB_CONDATA pConData = ( PHB_CONDATA ) hb_stackGetTSD( &s_conData );

      pConData->timeout = conn->timeout;
      pConData->port = conn->port;
      hb_strncpy( pConData->server, conn->server, sizeof( pConData->server ) - 1 );

      s_fileConClose( conn );
   }

   hb_retl( conn != NULL );
}

/* Client methods
 */
static BOOL s_fileAccept( const char * pFilename )
{
   return hb_strnicmp( pFilename, NETIO_FILE_PREFIX, NETIO_FILE_PREFIX_LEN ) == 0;
}

static BOOL s_fileExists( const char * pFilename, char * pRetPath )
{
   BOOL fResult = FALSE;
   PHB_CONCLI conn;

   if( pRetPath )
      hb_strncpy( pRetPath, pFilename, HB_PATH_MAX - 1 );

   pFilename += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pFilename, NULL, 0, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         BYTE msgbuf[ NETIO_MSGLEN ];
         UINT16 len = ( UINT16 ) strlen( pFilename );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_EXISTS );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pFilename, len, TRUE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static BOOL s_fileDelete( const char * pFilename )
{
   BOOL fResult = FALSE;
   PHB_CONCLI conn;

   pFilename += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pFilename, NULL, 0, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         BYTE msgbuf[ NETIO_MSGLEN ];
         UINT16 len = ( UINT16 ) strlen( pFilename );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_DELETE );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pFilename, len, TRUE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static BOOL s_fileRename( const char * pszFileName, const char * pszNewName )
{
   BOOL fResult = FALSE;
   PHB_CONCLI conn;

   pszFileName += NETIO_FILE_PREFIX_LEN;
   if( s_fileAccept( pszNewName ) )
      pszNewName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszFileName, NULL, 0, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         BYTE msgbuf[ NETIO_MSGLEN ];
         UINT16 len1 = ( UINT16 ) strlen( pszFileName );
         UINT16 len2 = ( UINT16 ) strlen( pszNewName );
         BYTE * pBuffer = ( BYTE * ) hb_xgrab( len1 + len2 );

         memcpy( pBuffer, pszFileName, len1 );
         memcpy( pBuffer + len1, pszNewName, len2 );
         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_RENAME );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len1 );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], len2 );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         fResult = s_fileSendMsg( conn, msgbuf, pBuffer, len1 + len2, TRUE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static PHB_FILE s_fileOpen( const char * pFilename, const char * pDefExt,
                            USHORT uiExFlags, const char * pPaths,
                            PHB_ITEM pError )
{
   PHB_FILE pFile = NULL;
   PHB_CONCLI conn;
   const char * pszFile = pFilename + NETIO_FILE_PREFIX_LEN;

   HB_SYMBOL_UNUSED( pPaths );

   conn = s_fileConnect( &pszFile, NULL, 0, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         BYTE msgbuf[ NETIO_MSGLEN ];
         UINT16 len = ( UINT16 ) strlen( pszFile );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_OPEN );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], uiExFlags );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         if( pDefExt )
            hb_strncpy( ( char * ) &msgbuf[ 8 ],
                        ( const char * ) pDefExt, sizeof( msgbuf ) - 9 );

         if( s_fileSendMsg( conn, msgbuf, pszFile, len, TRUE ) )
         {
            pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );
            pFile->pFuncs = s_fileMethods();
            pFile->conn = conn;
            pFile->fd = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
         }
         s_fileConUnlock( conn );
      }

      if( !pFile )
         s_fileConClose( conn );
   }

   if( pError )
   {
      hb_errPutFileName( pError, pFilename );
      if( pFile == NULL )
      {
         hb_errPutOsCode( pError, hb_fsError() );
         hb_errPutGenCode( pError, ( USHORT ) ( ( uiExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
   }

   return pFile;
}

static void s_fileClose( PHB_FILE pFile )
{
   if( s_fileConLock( pFile->conn ) )
   {
      BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_CLOSE );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
      s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, TRUE );
      s_fileConUnlock( pFile->conn );
   }
   s_fileConClose( pFile->conn );
   hb_xfree( pFile );
}

static BOOL s_fileLock( PHB_FILE pFile, HB_FOFFSET ulStart, HB_FOFFSET ulLen,
                        int iType )
{
   BOOL fResult = FALSE;

   if( s_fileConLock( pFile->conn ) )
   {
      BYTE msgbuf[ NETIO_MSGLEN ];
      BOOL fUnLock = ( iType & FL_MASK ) == FL_UNLOCK;

      HB_PUT_LE_UINT32( &msgbuf[  0 ], fUnLock ? NETIO_UNLOCK : NETIO_LOCK );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT64( &msgbuf[  6 ], ulStart );
      HB_PUT_LE_UINT64( &msgbuf[ 14 ], ulLen );
      HB_PUT_LE_UINT16( &msgbuf[ 22 ], ( USHORT ) iType );
      memset( msgbuf + 24, '\0', sizeof( msgbuf ) - 24 );

      fResult = s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, !fUnLock );
      s_fileConUnlock( pFile->conn );
   }

   return fResult;
}

static ULONG s_fileReadAt( PHB_FILE pFile, void * data, ULONG ulSize,
                           HB_FOFFSET llOffset )
{
   ULONG ulResult = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_READ );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[  6 ], ulSize );
      HB_PUT_LE_UINT64( &msgbuf[ 10 ], llOffset );
      memset( msgbuf + 18, '\0', sizeof( msgbuf ) - 18 );

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, TRUE ) )
      {
         ulResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
         if( ulResult > 0 )
         {
            if( ulResult > ulSize ) /* error, it should not happen, enemy attack? */
               ulResult = ulSize;
            ulResult = s_fileRecvAll( pFile->conn, data, ulResult );
         }
         hb_fsSetError( HB_GET_LE_UINT16( &msgbuf[ 8 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return ulResult;
}

static ULONG s_fileWriteAt( PHB_FILE pFile, const void * data, ULONG ulSize,
                            HB_FOFFSET llOffset )
{
   ULONG ulResult = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_WRITE );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[  6 ], ulSize );
      HB_PUT_LE_UINT64( &msgbuf[ 10 ], llOffset );
      memset( msgbuf + 18, '\0', sizeof( msgbuf ) - 18 );

      if( s_fileSendMsg( pFile->conn, msgbuf, data, ulSize, TRUE ) )
      {
         ulResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
         hb_fsSetError( HB_GET_LE_UINT16( &msgbuf[ 8 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return ulResult;
}

static BOOL s_fileTruncAt( PHB_FILE pFile, HB_FOFFSET llOffset )
{
   BOOL fResult = FALSE;

   if( s_fileConLock( pFile->conn ) )
   {
      BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_TRUNC );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      HB_PUT_LE_UINT64( &msgbuf[ 6 ], llOffset );
      memset( msgbuf + 14, '\0', sizeof( msgbuf ) - 14 );

      fResult = s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, TRUE );
      s_fileConUnlock( pFile->conn );
   }

   return fResult;
}

static HB_FOFFSET s_fileSize( PHB_FILE pFile )
{
   HB_FOFFSET llOffset = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( msgbuf, NETIO_SIZE );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, TRUE ) )
      {
         llOffset = HB_GET_LE_UINT64( &msgbuf[ 4 ] );
         hb_fsSetError( HB_GET_LE_UINT16( &msgbuf[ 12 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return llOffset;
}

static void s_fileCommit( PHB_FILE pFile )
{
   if( s_fileConLock( pFile->conn ) )
   {
      BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( msgbuf, NETIO_COMMIT );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );

      s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, FALSE );
      s_fileConUnlock( pFile->conn );
   }
}

static HB_FHANDLE s_fileHandle( PHB_FILE pFile )
{
   return pFile ? pFile->conn->sd : HB_NO_SOCKET;
}

static const HB_FILE_FUNCS * s_fileMethods( void )
{
   static const HB_FILE_FUNCS s_fileFuncs =
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

   return &s_fileFuncs;
}

/* set HB_NETIO_STARTUP_INIT macro if you want to register NETIO client
 * automatically at application startup
 */
#if defined( HB_NETIO_STARTUP_INIT )

HB_CALL_ON_STARTUP_BEGIN( _hb_file_io_init_ )
   s_netio_init();
HB_CALL_ON_STARTUP_END( _hb_file_io_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_file_io_init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_hb_file_io_init_ = _hb_file_io_init_;
   #pragma data_seg()
#endif

#endif /* HB_NETIO_STARTUP_INIT */
