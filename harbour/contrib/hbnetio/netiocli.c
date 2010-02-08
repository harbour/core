/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server with RPC support
 *    All files which names starts 'net:' are redirected to this API.
 *    This is client code with
 *       NETIO_CONNECT( [<cServer>], [<nPort>], [<nTimeOut>],
 *                      [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
 *             -> <lOK>
 *    function which register alternative RDD IO API, sets server
 *    address and port and connection timeout parameter.
 *    Then it tries to connect to the server and returns .T. on success.
 *    This code also provides the following .prg functions:
 *       NETIO_DISCONNECT( [<cServer>], [<nPort>] ) -> <lOK>
 *       NETIO_DECODE( [@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>],
 *                     [@<cPasswd>], [@<nCompressionLevel>], [@<nStrategy>] )
 *             -> <lDecoded>
 *       NETIO_PROCEXISTS( <cProcName> ) -> <lExists>
 *       NETIO_PROCEXEC( <cProcName> [, <params,...>] ) -> <lSent>
 *       NETIO_PROCEXECW( <cProcName> [, <params,...>] ) -> <lExecuted>
 *       NETIO_FUNCEXEC( <cFuncName> [, <params,...>] ) -> <xFuncRetVal>
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
#include "hbznet.h"
#include "hbzlib.ch"
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
   HB_COUNTER     usrcount;
   PHB_ITEM       mutex;
   int            timeout;
   int            port;
   HB_SOCKET      sd;
   PHB_ZNETSTREAM zstream;
   struct _HB_CONCLI * next;
   int            level;
   int            strategy;
   int            passlen;
   char           passwd[ NETIO_PASSWD_MAX ];
   char           server[ 1 ];
}
HB_CONCLI, * PHB_CONCLI;

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   PHB_CONCLI     conn;
   HB_USHORT      fd;
}
HB_FILE;

typedef struct
{
   int      timeout;
   int      port;
   int      level;
   int      strategy;
   int      passlen;
   char     server[ NETIO_SERVERNAME_MAX ];
   char     passwd[ NETIO_PASSWD_MAX ];
} HB_CONDATA, * PHB_CONDATA;

/* MT macros */
#define HB_NETIO_LOCK         hb_threadEnterCriticalSection( &s_netioMtx );
#define HB_NETIO_UNLOCK       hb_threadLeaveCriticalSection( &s_netioMtx );
static HB_CRITICAL_NEW( s_netioMtx );

static HB_TSD_NEW( s_conData, sizeof( HB_CONDATA ), NULL, NULL );

static PHB_CONCLI s_connections = NULL;

static HB_BOOL s_defaultInit = HB_TRUE;
static HB_CONDATA s_defaultConn = {
   NETIO_DEFAULT_TIMEOUT,
   NETIO_DEFAULT_PORT,
   HB_ZLIB_COMPRESSION_DISABLE,
   HB_ZLIB_STRATEGY_DEFAULT,
   0,
   NETIO_DEFAULT_SERVER,
   ""
};

static HB_BOOL s_fInit = HB_TRUE;

static const HB_FILE_FUNCS * s_fileMethods( void );

#define NETIO_TIMEOUT   -1

static long s_fileRecvAll( PHB_CONCLI conn, void * buffer, long len )
{
   HB_BYTE * ptr = ( HB_BYTE * ) buffer;
   long lRead = 0, l;

   while( lRead < len )
   {
      if( conn->zstream )
         l = hb_znetRead( conn->zstream, conn->sd, ptr + lRead, len - lRead, NETIO_TIMEOUT );
      else
         l = hb_socketRecv( conn->sd, ptr + lRead, len - lRead, 0, NETIO_TIMEOUT );
      if( l <= 0 )
         break;
      lRead += l;
   }
   return lRead;
}

static HB_BOOL s_fileSendMsg( PHB_CONCLI conn, HB_BYTE * msgbuf,
                              const void * data, long len, HB_BOOL fWait )
{
   HB_BYTE buffer[ 2048 ];
   HB_BYTE * msg, * ptr = NULL;
   HB_LONG lSent = 0, lLast = 1, l;
   HB_BOOL fResult = HB_FALSE;

   if( len == 0 )
   {
      msg = msgbuf;
      len = NETIO_MSGLEN;
   }
   else
   {
      len += NETIO_MSGLEN;
      if( len > ( long ) sizeof( buffer ) )
         msg = ptr = ( HB_BYTE * ) hb_xgrab( len );
      else
         msg = buffer;
      memcpy( msg, msgbuf, NETIO_MSGLEN );
      memcpy( msg + NETIO_MSGLEN, data, len - NETIO_MSGLEN );
   }

   while( lSent < len )
   {
      if( conn->zstream )
         l = hb_znetWrite( conn->zstream, conn->sd, msg + lSent, len - lSent, NETIO_TIMEOUT, &lLast );
      else
         l = lLast = hb_socketSend( conn->sd, msg + lSent, len - lSent, 0, NETIO_TIMEOUT );
      if( l > 0 )
         lSent += l;
      if( lLast <= 0 )
         break;
   }

   if( ptr )
      hb_xfree( ptr );

   if( lSent == len )
   {
      if( conn->zstream )
         hb_znetFlush( conn->zstream, conn->sd, NETIO_TIMEOUT );

      if( fWait )
      {
         int iMsg = HB_GET_LE_INT32( msgbuf );

         while( s_fileRecvAll( conn, msgbuf, NETIO_MSGLEN ) == NETIO_MSGLEN )
         {
            int iResult = HB_GET_LE_INT32( msgbuf );

            if( iResult != NETIO_SYNC )
            {
               if( iResult == NETIO_ERROR )
                  hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 4 ] ) );
               else if( iResult == iMsg )
                  fResult = HB_TRUE;
               break;
            }
         }
      }
      else
         fResult = HB_TRUE;
   }

   return fResult;
}

static PHB_CONCLI s_fileConNew( HB_SOCKET sd, const char * pszServer,
                                int iPort, int iTimeOut,
                                const char * pszPasswd, int iPassLen,
                                int iLevel, int iStrategy )
{
   PHB_CONCLI conn;
   int iLen;

   iLen = ( int ) strlen( pszServer );
   conn = ( PHB_CONCLI ) hb_xgrab( sizeof( HB_CONCLI ) + iLen );
   hb_atomic_set( &conn->used, 1 );
   hb_atomic_set( &conn->usrcount, 0 );
   conn->mutex = hb_threadMutexCreate();
   conn->sd = sd;
   conn->zstream = NULL;
   conn->next = NULL;
   conn->timeout = iTimeOut;
   conn->port = iPort;
   memcpy( conn->server, pszServer, iLen + 1 );
   conn->level = iLevel;
   conn->strategy = iStrategy;
   conn->passlen = iPassLen;
   if( iPassLen )
      memcpy( conn->passwd, pszPasswd, iPassLen );

   return conn;
}

static void s_fileConFree( PHB_CONCLI conn )
{
   hb_socketShutdown( conn->sd, HB_SOCKET_SHUT_RDWR );
   hb_socketClose( conn->sd );
   if( conn->zstream )
      hb_znetClose( conn->zstream );
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

static HB_BOOL s_fileUsrDisconnect( const char * pszServer, int iPort )
{
   PHB_CONCLI conn, connClose = NULL;

   HB_NETIO_LOCK
   conn = s_connections;
   while( conn )
   {
      if( conn->port == iPort &&
          hb_stricmp( conn->server, pszServer ) == 0 )
      {
         if( hb_atomic_get( &conn->usrcount ) )
         {
            if( hb_atomic_dec( &conn->usrcount ) )
               connClose = conn;
         }
         break;
      }
      conn = conn->next;
   }
   HB_NETIO_UNLOCK

   if( connClose )
      s_fileConClose( connClose );

   return conn != NULL;
}

static void s_fileUsrConnect( PHB_CONCLI conn )
{
   hb_atomic_inc( &conn->usrcount );
}

static HB_BOOL s_fileConLock( PHB_CONCLI conn )
{
   return !conn->mutex || hb_threadMutexLock( conn->mutex );
}

static void s_fileConUnlock( PHB_CONCLI conn )
{
   if( conn->mutex )
      hb_threadMutexUnlock( conn->mutex );
}

static void s_fileGetConnParam( const char ** pszServer, int * piPort, int * piTimeOut,
                                const char ** pszPasswd, int * piPassLen )
{
   PHB_CONDATA pConData = ( PHB_CONDATA ) hb_stackTestTSD( &s_conData );


   if( pConData == NULL )
      pConData = &s_defaultConn;

   if( *pszServer == NULL )
      *pszServer = pConData->server;
   if( *piPort == 0 )
      *piPort = pConData->port;
   if( piTimeOut && ( *piTimeOut == 0 || *piTimeOut < -1 ) )
      *piTimeOut = pConData->timeout;
   if( piPassLen && *piPassLen == 0 && pConData->passlen != 0 )
   {
      *piPassLen = pConData->passlen;
      *pszPasswd = pConData->passwd;
   }
}

static const char * s_fileDecode( const char * pszFilename,
                                  char * buffer, const char ** pServer,
                                  int * piPort, int * piTimeOut,
                                  const char ** pPasswd, int * piPassLen,
                                  int * piLevel, int * piStrategy )
{
   HB_SYMBOL_UNUSED( piTimeOut );
   HB_SYMBOL_UNUSED( piLevel );
   HB_SYMBOL_UNUSED( piStrategy );

   if( pszFilename )
   {
      /* decode server address and port if given as part of file name
       * in format like:
       *          "192.168.0.1:2941:path/to/file"
       * or:
       *          "192.168.0.1:2941:passwd:path/to/file"
       * or:
       *          "//192.168.0.1:2941/path/to/file"
       */
      const char * psz, * pth = NULL;

      if( ( pszFilename[ 0 ] == '/' || pszFilename[ 0 ] == '\\' ) &&
          pszFilename[ 0 ] == pszFilename[ 1 ] )
      {
         pszFilename += 2;
         pth = strchr( pszFilename, '/' );
         psz = strchr( pszFilename, '\\' );
         if( !pth || ( psz && psz < pth ) )
         {
            pth = psz;
            if( !pth )
               pth = pszFilename + strlen( pszFilename );
         }
      }

      psz = strchr( pszFilename, ':' );
      if( pth && ( !psz || pth < psz ) )
         psz = pth;

      if( psz )
      {
         int iLen = ( int ) ( psz - pszFilename );

         if( pth || iLen == 0 || iLen > 1 )
         {
            char port_buf[ 10 ], c;

            if( iLen >= NETIO_SERVERNAME_MAX )
               iLen = NETIO_SERVERNAME_MAX - 1;
            if( iLen > 0 )
            {
               hb_strncpy( buffer, pszFilename, iLen );
               *pServer = buffer;
            }
            pszFilename = psz + 1;
            if( !pth || psz < pth )
            {
               iLen = 0;
               while( HB_ISDIGIT( pszFilename[ iLen ] ) &&
                      iLen < ( int ) sizeof( port_buf ) - 1 )
               {
                  port_buf[ iLen ] = pszFilename[ iLen ];
                  ++iLen;
               }
               c = pszFilename[ iLen ];
               if( c == ':' || c == '/' || c == '\\' )
               {
                  if( iLen > 0 )
                  {
                     int iOverflow;
                     HB_MAXINT llPort;

                     port_buf[ iLen ] = '\0';
                     llPort = hb_strValInt( port_buf, &iOverflow );

                     if( !iOverflow && llPort > 0 && llPort < 0x10000 )
                     {
                        pszFilename += iLen;
                        *piPort = ( int ) llPort;
                     }
                  }
                  if( c == ':' )
                  {
                     ++pszFilename;
                     iLen = 0;
                     while( pszFilename[ iLen ] &&
                            pszFilename[ iLen ] != ':' )
                        ++iLen;
                     if( pszFilename[ iLen ] == ':' )
                     {
                        *pPasswd = pszFilename;
                        pszFilename += iLen + 1;
                        if( iLen > NETIO_PASSWD_MAX )
                           iLen = NETIO_PASSWD_MAX;
                        *piPassLen = iLen;
                     }
                  }
               }
            }
         }
      }
   }

   return pszFilename;
}

static PHB_CONCLI s_fileConnect( const char ** pFilename,
                                 const char * pszServer,
                                 int iPort, int iTimeOut,
                                 const char * pszPasswd, int iPassLen,
                                 int iLevel, int iStrategy )
{
   PHB_CONCLI conn;
   HB_SOCKET sd;
   char server[ NETIO_SERVERNAME_MAX ];
   char * pszIpAddres;

   s_fileGetConnParam( &pszServer, &iPort, &iTimeOut, &pszPasswd, &iPassLen );

   if( pFilename )
      *pFilename = s_fileDecode( *pFilename, server,
                                 &pszServer, &iPort, &iTimeOut,
                                 &pszPasswd, &iPassLen, &iLevel, &iStrategy );

   if( iLevel == HB_ZLIB_COMPRESSION_DISABLE && iPassLen )
      iLevel = HB_ZLIB_COMPRESSION_DEFAULT;

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
            hb_socketSetKeepAlive( sd, HB_TRUE );
            if( hb_socketConnect( sd, pSockAddr, uiLen, iTimeOut ) == 0 )
            {
               HB_BYTE msgbuf[ NETIO_MSGLEN ];
               HB_U16 len = ( HB_U16 ) strlen( NETIO_LOGINSTRID );

               HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_LOGIN );
               HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
               memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );

               hb_socketSetNoDelay( sd, HB_TRUE );
               conn = s_fileConNew( sd, pszIpAddres, iPort, iTimeOut,
                                    pszPasswd, iPassLen, iLevel, iStrategy );
               sd = HB_NO_SOCKET;

               if( iLevel != HB_ZLIB_COMPRESSION_DISABLE )
               {
                  conn->zstream = hb_znetOpen( iLevel, iStrategy );
                  if( conn->zstream != NULL )
                  {
                     if( iPassLen )
                        hb_znetEncryptKey( conn->zstream, pszPasswd, iPassLen );
                  }
                  else
                  {
                     s_fileConFree( conn );
                     conn = NULL;
                  }
               }

               if( conn )
               {
                  if( !s_fileSendMsg( conn, msgbuf, NETIO_LOGINSTRID, len, HB_TRUE ) ||
                      HB_GET_LE_UINT32( &msgbuf[ 4 ] ) != NETIO_CONNECTED )
                  {
                     s_fileConFree( conn );
                     conn = NULL;
                  }
                  else
                     s_fileConRegister( conn );
               }
            }
            hb_xfree( pSockAddr );
         }
         if( sd != HB_NO_SOCKET )
            hb_socketClose( sd );
      }
   }

   if( conn != NULL && s_defaultInit )
   {
      HB_NETIO_LOCK
      if( s_defaultInit )
      {
         hb_strncpy( s_defaultConn.server, pszIpAddres,
                     sizeof( s_defaultConn.server ) - 1 );
         s_defaultConn.port = iPort;
         s_defaultConn.timeout = iTimeOut;
         s_defaultConn.level = iLevel;
         s_defaultConn.strategy = iStrategy;
         s_defaultConn.passlen = iPassLen;
         if( iPassLen )
            memcpy( s_defaultConn.passwd, pszPasswd, iPassLen );
         s_defaultInit = HB_FALSE;
      }
      HB_NETIO_UNLOCK
   }

   hb_xfree( pszIpAddres );

   return conn;
}

static void s_netio_exit( void * cargo )
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
      s_fInit = HB_TRUE;
   }
}

static void s_netio_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_fInit )
   {
      hb_socketInit();
      hb_fileRegister( s_fileMethods() );
      hb_vmAtQuit( s_netio_exit, NULL );
      s_fInit = HB_FALSE;
   }
}

/* NETIO_DECODE( [@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>], ;
 *               [@<cPasswd>], [@<nCompressionLevel>], [@<nStrategy>] ) -> <lOK>
 */
HB_FUNC( NETIO_DECODE )
{
   char server[ NETIO_SERVERNAME_MAX ];
   const char * pszFullName = hb_parc( 1 );
   const char * pszServer, * pszPasswd, * pszFile;
   int iPort, iTimeOut, iPassLen, iLevel, iStrategy;

   pszServer = hb_parc( 2 );
   iPort = hb_parni( 3 );
   iTimeOut = hb_parni( 4 );
   pszPasswd = hb_parc( 5 );
   iPassLen = ( int ) hb_parclen( 5 );
   iLevel = hb_parnidef( 6, HB_ZLIB_COMPRESSION_DISABLE );
   iStrategy = hb_parnidef( 7, HB_ZLIB_STRATEGY_DEFAULT );

   s_fileGetConnParam( &pszServer, &iPort, &iTimeOut, &pszPasswd, &iPassLen );

   pszFile = s_fileDecode( pszFullName, server,
                           &pszServer, &iPort, &iTimeOut,
                           &pszPasswd, &iPassLen, &iLevel, &iStrategy );

   if( iLevel == HB_ZLIB_COMPRESSION_DISABLE && iPassLen )
      iLevel = HB_ZLIB_COMPRESSION_DEFAULT;

   hb_storc( pszServer, 2 );
   hb_storni( iPort, 3 );
   hb_storni( iTimeOut, 4 );
   hb_storclen( pszPasswd, iPassLen, 5 );
   hb_storni( iLevel, 6 );
   hb_storni( iStrategy, 7 );
   if( pszFile != pszFullName )
      /* the order is important and 1-st parameter
       * should be assigned at the end
       */
      hb_storc( pszFile, 1 );

   hb_retl( pszFile != pszFullName );
}

/* NETIO_CONNECT( [<cServer>], [<nPort>], [<nTimeOut>], ;
 *                [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] ) -> <lOK>
 */
HB_FUNC( NETIO_CONNECT )
{
   const char * pszServer = hb_parc( 1 ),
              * pszPasswd = hb_parc( 4 );
   int iPort = hb_parni( 2 ),
       iTimeOut = hb_parni( 3 ),
       iPassLen = ( int ) hb_parclen( 4 ),
       iLevel = hb_parnidef( 5, HB_ZLIB_COMPRESSION_DISABLE ),
       iStrategy = hb_parnidef( 6, HB_ZLIB_STRATEGY_DEFAULT );
   PHB_CONCLI conn;

   if( iPassLen > NETIO_PASSWD_MAX )
      iPassLen = NETIO_PASSWD_MAX;

   s_netio_init( NULL );

   conn = s_fileConnect( NULL, pszServer, iPort, iTimeOut, pszPasswd, iPassLen,
                         iLevel, iStrategy );
   if( conn )
   {
      PHB_CONDATA pConData = ( PHB_CONDATA ) hb_stackGetTSD( &s_conData );

      pConData->timeout = conn->timeout;
      pConData->port = conn->port;
      hb_strncpy( pConData->server, conn->server, sizeof( pConData->server ) - 1 );
      pConData->level = conn->level;
      pConData->strategy = conn->strategy;
      pConData->passlen = conn->passlen;
      if( conn->passlen )
         memcpy( pConData->passwd, conn->passwd, conn->passlen );

      s_fileUsrConnect( conn );
   }

   hb_retl( conn != NULL );
}

/* NETIO_DISCONNECT( [<cServer>], [<nPort>] ) -> <lOK>
 */
HB_FUNC( NETIO_DISCONNECT )
{
   const char * pszServer = hb_parc( 1 );
   char * pszIpAddres;
   int iPort = hb_parni( 2 );
   HB_BOOL fDisconnected = HB_FALSE;

   s_fileGetConnParam( &pszServer, &iPort, NULL, NULL, NULL );
   pszIpAddres = hb_socketResolveAddr( pszServer, HB_SOCKET_AF_INET );
   if( pszIpAddres != NULL )
   {
      fDisconnected = s_fileUsrDisconnect( pszIpAddres, iPort );
      hb_xfree( pszIpAddres );
   }
   hb_retl( fDisconnected );
}

static const char * s_netio_params( int iMsg, const char * pszName, HB_U32 * pSize, char ** pFree )
{
   int iPCount = iMsg == NETIO_PROCIS ? 0 : hb_pcount(), i;
   char * data = NULL, * itmData;
   ULONG size, itmSize;

   size = ( ULONG ) strlen( pszName ) + 1;

   for( i = 2; i <= iPCount; ++i )
   {
      itmData = hb_itemSerialize( hb_param( i, HB_IT_ANY ), HB_TRUE, &itmSize );
      if( data == NULL )
         data = ( char * ) memcpy( hb_xgrab( size + itmSize ), pszName, size );
      else
         data = ( char * ) hb_xrealloc( data, size + itmSize );
      memcpy( data + size, itmData, itmSize );
      size += itmSize;
      hb_xfree( itmData );
   }

   *pFree = data;
   *pSize = ( HB_U32 ) size;

   return data ? data : pszName;
}

static HB_BOOL s_netio_procexec( const char * pszProcName, int iMsg )
{
   HB_BOOL fResult = HB_FALSE;

   if( pszProcName )
   {
      PHB_CONCLI conn = s_fileConnect( &pszProcName, NULL, 0, 0, NULL, 0,
                                       HB_ZLIB_COMPRESSION_DISABLE, 0 );
      if( conn )
      {
         if( s_fileConLock( conn ) )
         {
            HB_BYTE msgbuf[ NETIO_MSGLEN ];
            const char * data;
            char * buffer;
            HB_U32 size;

            data = s_netio_params( iMsg, pszProcName, &size, &buffer );
            HB_PUT_LE_UINT32( &msgbuf[ 0 ], iMsg );
            HB_PUT_LE_UINT32( &msgbuf[ 4 ], size );
            memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
            fResult = s_fileSendMsg( conn, msgbuf, data, size, iMsg != NETIO_PROC );
            if( fResult && iMsg == NETIO_FUNC )
            {
               ULONG ulResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );

               if( ulResult > 0 )
               {
                  PHB_ITEM pItem;

                  if( ulResult > size && buffer )
                  {
                     hb_xfree( buffer );
                     buffer = NULL;
                  }
                  if( buffer == NULL )
                     buffer = ( char * ) hb_xgrab( ulResult );
                  ulResult = s_fileRecvAll( conn, buffer, ulResult );
                  data = buffer;
                  pItem = hb_itemDeserialize( &data, &ulResult );
                  if( pItem )
                     hb_itemReturnRelease( pItem );
                  /* else TODO: RTE */
               }
            }
            if( buffer )
               hb_xfree( buffer );
            s_fileConUnlock( conn );
         }
         s_fileConClose( conn );
      }
   }

   return fResult;
}

/* check if function/procedure exists on the server side:
 *
 * NETIO_PROCEXISTS( <cProcName> ) -> <lExists>
 */
HB_FUNC( NETIO_PROCEXISTS )
{
   const char * pszProcName = hb_parc( 1 );

   hb_retl( s_netio_procexec( pszProcName, NETIO_PROCIS ) );
}

/* execute function/procedure on server the side,
 * do not wait for confirmation:
 *
 * NETIO_PROCEXEC( <cProcName> [, <params,...>] ) -> <lSent>
 */
HB_FUNC( NETIO_PROCEXEC )
{
   const char * pszProcName = hb_parc( 1 );

   hb_retl( s_netio_procexec( pszProcName, NETIO_PROC ) );
}

/* execute function/procedure on the server side and wait for
 * confirmation:
 *
 * NETIO_PROCEXECW( <cProcName> [, <params,...>] ) -> <lExecuted>
 */
HB_FUNC( NETIO_PROCEXECW )
{
   const char * pszProcName = hb_parc( 1 );

   hb_retl( s_netio_procexec( pszProcName, NETIO_PROCW ) );
}

/* execute function on the server side and wait for its return value:
 *
 * NETIO_FUNCEXEC( <cFuncName> [, <params,...>] ) -> <xFuncRetVal>
 */
HB_FUNC( NETIO_FUNCEXEC )
{
   const char * pszProcName = hb_parc( 1 );

   s_netio_procexec( pszProcName, NETIO_FUNC );
}

/* Client methods
 */
static HB_BOOL s_fileAccept( const char * pFilename )
{
   return hb_strnicmp( pFilename, NETIO_FILE_PREFIX, NETIO_FILE_PREFIX_LEN ) == 0;
}

static HB_BOOL s_fileExists( const char * pFilename, char * pRetPath )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   if( pRetPath )
      hb_strncpy( pRetPath, pFilename, HB_PATH_MAX - 1 );

   pFilename += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pFilename, NULL, 0, 0, NULL, 0,
                         HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pFilename );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_EXISTS );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pFilename, len, HB_TRUE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileDelete( const char * pFilename )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pFilename += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pFilename, NULL, 0, 0, NULL, 0,
                         HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pFilename );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_DELETE );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pFilename, len, HB_TRUE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileRename( const char * pszFileName, const char * pszNewName )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszFileName += NETIO_FILE_PREFIX_LEN;
   if( s_fileAccept( pszNewName ) )
      pszNewName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszFileName, NULL, 0, 0, NULL, 0,
                         HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len1 = ( HB_U16 ) strlen( pszFileName );
         HB_U16 len2 = ( HB_U16 ) strlen( pszNewName );
         HB_BYTE * pBuffer = ( HB_BYTE * ) hb_xgrab( len1 + len2 );

         memcpy( pBuffer, pszFileName, len1 );
         memcpy( pBuffer + len1, pszNewName, len2 );
         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_RENAME );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len1 );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], len2 );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         fResult = s_fileSendMsg( conn, msgbuf, pBuffer, len1 + len2, HB_TRUE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static PHB_FILE s_fileOpen( const char * pFilename, const char * pDefExt,
                            HB_USHORT uiExFlags, const char * pPaths,
                            PHB_ITEM pError )
{
   PHB_FILE pFile = NULL;
   PHB_CONCLI conn;
   const char * pszFile = pFilename + NETIO_FILE_PREFIX_LEN;

   HB_SYMBOL_UNUSED( pPaths );

   conn = s_fileConnect( &pszFile, NULL, 0, 0, NULL, 0,
                         HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszFile );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_OPEN );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], uiExFlags );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         if( pDefExt )
            hb_strncpy( ( char * ) &msgbuf[ 8 ],
                        ( const char * ) pDefExt, sizeof( msgbuf ) - 9 );

         if( s_fileSendMsg( conn, msgbuf, pszFile, len, HB_TRUE ) )
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
         hb_errPutGenCode( pError, ( HB_ERRCODE ) ( ( uiExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
   }

   return pFile;
}

static void s_fileClose( PHB_FILE pFile )
{
   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_CLOSE );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
      s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE );
      s_fileConUnlock( pFile->conn );
   }
   s_fileConClose( pFile->conn );
   hb_xfree( pFile );
}

static HB_BOOL s_fileLock( PHB_FILE pFile, HB_FOFFSET ulStart, HB_FOFFSET ulLen,
                           int iType )
{
   HB_BOOL fResult = HB_FALSE;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];
      HB_BOOL fUnLock = ( iType & FL_MASK ) == FL_UNLOCK;

      HB_PUT_LE_UINT32( &msgbuf[  0 ], fUnLock ? NETIO_UNLOCK : NETIO_LOCK );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT64( &msgbuf[  6 ], ulStart );
      HB_PUT_LE_UINT64( &msgbuf[ 14 ], ulLen );
      HB_PUT_LE_UINT16( &msgbuf[ 22 ], ( HB_USHORT ) iType );
#if NETIO_MSGLEN > 24
      memset( msgbuf + 24, '\0', sizeof( msgbuf ) - 24 );
#endif

      fResult = s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, !fUnLock );
      s_fileConUnlock( pFile->conn );
   }

   return fResult;
}

static HB_SIZE s_fileReadAt( PHB_FILE pFile, void * data, HB_SIZE ulSize,
                             HB_FOFFSET llOffset )
{
   HB_SIZE ulResult = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_READ );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[  6 ], ulSize );
      HB_PUT_LE_UINT64( &msgbuf[ 10 ], llOffset );
      memset( msgbuf + 18, '\0', sizeof( msgbuf ) - 18 );

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE ) )
      {
         ulResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
         if( ulResult > 0 )
         {
            if( ulResult > ulSize ) /* error, it should not happen, enemy attack? */
               ulResult = ulSize;
            ulResult = s_fileRecvAll( pFile->conn, data, ulResult );
         }
         hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 8 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return ulResult;
}

static HB_SIZE s_fileWriteAt( PHB_FILE pFile, const void * data, HB_SIZE ulSize,
                              HB_FOFFSET llOffset )
{
   HB_SIZE ulResult = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_WRITE );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[  6 ], ulSize );
      HB_PUT_LE_UINT64( &msgbuf[ 10 ], llOffset );
      memset( msgbuf + 18, '\0', sizeof( msgbuf ) - 18 );

      if( s_fileSendMsg( pFile->conn, msgbuf, data, ulSize, HB_TRUE ) )
      {
         ulResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
         hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 8 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return ulResult;
}

static HB_BOOL s_fileTruncAt( PHB_FILE pFile, HB_FOFFSET llOffset )
{
   HB_BOOL fResult = HB_FALSE;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_TRUNC );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      HB_PUT_LE_UINT64( &msgbuf[ 6 ], llOffset );
      memset( msgbuf + 14, '\0', sizeof( msgbuf ) - 14 );

      fResult = s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE );
      s_fileConUnlock( pFile->conn );
   }

   return fResult;
}

static HB_FOFFSET s_fileSize( PHB_FILE pFile )
{
   HB_FOFFSET llOffset = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( msgbuf, NETIO_SIZE );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE ) )
      {
         llOffset = HB_GET_LE_UINT64( &msgbuf[ 4 ] );
         hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 12 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return llOffset;
}

void s_fileFlush( PHB_FILE pFile, HB_BOOL fDirty )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( fDirty );
}

static void s_fileCommit( PHB_FILE pFile )
{
   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( msgbuf, NETIO_COMMIT );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );

      s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_FALSE );
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
      s_fileFlush,
      s_fileCommit,
      s_fileHandle
   };

   return &s_fileFuncs;
}

/* set HB_NETIO_STARTUP_INIT macro if you want to register NETIO client
 * automatically at application startup
 */
#if defined( HB_NETIO_STARTUP_INIT )

HB_CALL_ON_STARTUP_BEGIN( _hb_file_netio_init_ )
   hb_vmAtInit( s_netio_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_file_netio_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_file_netio_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_file_netio_init_ )
   #include "hbiniseg.h"
#endif

#endif /* HB_NETIO_STARTUP_INIT */
