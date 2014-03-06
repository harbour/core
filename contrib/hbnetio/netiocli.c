/*
 * Harbour Project source code:
 *    demonstration code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server with RPC support
 *    All files which names starts 'net:' are redirected to this API.
 *    This is client code with
 *       netio_Connect( [<cServer>], [<nPort>], [<nTimeOut>],
 *                      [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
 *             -> <lOK>
 *    function which register alternative RDD IO API, sets server
 *    address and port and connection timeout parameter.
 *    Then it tries to connect to the server and returns .T. on success.
 *    This code also provides the following .prg functions:
 *       netio_Disconnect( [<cServer>], [<nPort>] ) -> <lOK>
 *       netio_Decode( [@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>],
 *                     [@<cPasswd>], [@<nCompressionLevel>], [@<nStrategy>] )
 *             -> <lDecoded>
 *       netio_ProcExists( <cProcName> ) -> <lExists>
 *       netio_ProcExec( <cProcName> [, <params,...>] ) -> <lSent>
 *       netio_ProcExecW( <cProcName> [, <params,...>] ) -> <lExecuted>
 *       netio_FuncExec( <cFuncName> [, <params,...>] ) -> <xFuncRetVal>
 *
 *       netio_OpenDataStream( <cStreamFuncName> [, <params,...>] )
 *             -> <nStreamID>
 *       netio_OpenItemStream( <cStreamFuncName> [, <params,...>] )
 *             -> <nStreamID>
 *       netio_CloseStream( <nStreamID>, [<cServer>], [<nPort>] )
 *             -> <lOK>
 *       netio_GetData( <nStreamID>, [<cServer>], [<nPort>] )
 *             -> <aData> | <cData> | NIL
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbserial.ch"

/*
 * client code
 */

typedef struct _HB_SRVDATA
{
   int      id;
   int      type;
   PHB_ITEM array;
   char *   data;
   HB_SIZE  size;
   HB_SIZE  bufsize;
   HB_SIZE  maxsize;
   struct _HB_SRVDATA * next;
}
HB_SRVDATA, * PHB_SRVDATA;

typedef struct _HB_CONCLI
{
   HB_COUNTER          used;
   HB_COUNTER          usrcount;
   PHB_ITEM            mutex;
   HB_ERRCODE          errcode;
   int                 timeout;
   int                 port;
   HB_SOCKET           sd;
   PHB_ZNETSTREAM      zstream;
   PHB_SRVDATA         srvdata;
   struct _HB_CONCLI * next;
   int                 level;
   int                 strategy;
   int                 passlen;
   char                passwd[ NETIO_PASSWD_MAX ];
   char                server[ 1 ];
}
HB_CONCLI, * PHB_CONCLI;

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   PHB_CONCLI conn;
   HB_USHORT  fd;
}
HB_FILE;

typedef struct
{
   int  timeout;
   int  port;
   int  level;
   int  strategy;
   int  passlen;
   char server[ NETIO_SERVERNAME_MAX ];
   char passwd[ NETIO_PASSWD_MAX ];
} HB_CONDATA, * PHB_CONDATA;

/* MT macros */
#define HB_NETIO_LOCK()    hb_threadEnterCriticalSection( &s_netioMtx )
#define HB_NETIO_UNLOCK()  hb_threadLeaveCriticalSection( &s_netioMtx )
static HB_CRITICAL_NEW( s_netioMtx );

static HB_TSD_NEW( s_conData, sizeof( HB_CONDATA ), NULL, NULL );

static PHB_CONCLI s_connections = NULL;

static HB_BOOL    s_defaultInit = HB_TRUE;
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

#define NETIO_TIMEOUT  -1

static void hb_errRT_NETIO( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode,
                            HB_ERRCODE errOsCode, const char * szDescription,
                            const char * szOperation )
{
   PHB_ITEM pError;

   pError = hb_errRT_New( ES_ERROR, "NETIO", errGenCode, errSubCode,
                          szDescription, szOperation, errOsCode, EF_NONE );
   hb_errLaunch( pError );
   hb_itemRelease( pError );
}

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

static long s_fileRecvTest( PHB_CONCLI conn, void * buffer, long len )
{
   HB_BYTE * ptr = ( HB_BYTE * ) buffer;
   long lRead = 0, l;
   HB_MAXINT timeout = 0;

   while( lRead < len )
   {
      if( conn->zstream )
         l = hb_znetRead( conn->zstream, conn->sd, ptr + lRead, len - lRead, timeout );
      else
         l = hb_socketRecv( conn->sd, ptr + lRead, len - lRead, 0, timeout );
      if( l <= 0 )
         break;
      lRead += l;
      timeout = NETIO_TIMEOUT;
   }
   return lRead;
}

static int s_fileGenSrvDataID( PHB_CONCLI conn )
{
   PHB_SRVDATA pSrvData = conn->srvdata;
   static int s_iStreamID = 0;

   if( ++s_iStreamID <= 0 )
      s_iStreamID = 1;

   while( pSrvData )
   {
      if( pSrvData->id == s_iStreamID )
      {
         if( ++s_iStreamID <= 0 )
            s_iStreamID = 1;
         pSrvData = conn->srvdata;
      }
      else
         pSrvData = pSrvData->next;
   }

   return s_iStreamID;
}

static PHB_SRVDATA s_fileFindSrvData( PHB_CONCLI conn, int iStreamID, int iType )
{
   PHB_SRVDATA pSrvData = conn->srvdata;

   while( pSrvData )
   {
      if( pSrvData->id == iStreamID )
         return ( iType == 0 || pSrvData->type == iType ) ? pSrvData : NULL;
      pSrvData = pSrvData->next;
   }

   return NULL;
}

static HB_BOOL s_fileCloseSrvData( PHB_CONCLI conn, int iStreamID )
{
   PHB_SRVDATA * pSrvDataPtr = &conn->srvdata;

   while( *pSrvDataPtr )
   {
      if( ( *pSrvDataPtr )->id == iStreamID )
      {
         PHB_SRVDATA pSrvData = *pSrvDataPtr;
         *pSrvDataPtr = pSrvData->next;
         if( pSrvData->array )
            hb_itemRelease( pSrvData->array );
         if( pSrvData->data )
            hb_xfree( pSrvData->data );
         hb_xfree( pSrvData );
         if( ! conn->srvdata )
            hb_atomic_dec( &conn->used );
         return HB_TRUE;
      }
      pSrvDataPtr = &( *pSrvDataPtr )->next;
   }

   return HB_FALSE;
}

static void s_fileNewSrvData( PHB_CONCLI conn, int iStreamID, int iType )
{
   PHB_SRVDATA pSrvData = s_fileFindSrvData( conn, iStreamID, 0 );

   if( ! pSrvData )
   {
      pSrvData = ( PHB_SRVDATA ) memset( hb_xgrab( sizeof( HB_SRVDATA ) ),
                                         0, sizeof( HB_SRVDATA ) );
      pSrvData->id = iStreamID;
      pSrvData->type = iType;
      if( iType == NETIO_SRVITEM )
         pSrvData->maxsize = 4096;
      else if( iType == NETIO_SRVDATA )
         pSrvData->maxsize = 0x10000;
      pSrvData->next = conn->srvdata;
      if( ! conn->srvdata )
         hb_atomic_inc( &conn->used );
      conn->srvdata = pSrvData;
   }
}

static HB_BOOL s_fileRecvSrvData( PHB_CONCLI conn, long len, int iStreamID, int iType )
{
   char * buffer = ( char * ) hb_xgrab( len );
   HB_BOOL fResult = HB_FALSE;

   if( s_fileRecvAll( conn, buffer, len ) == len )
   {
      PHB_SRVDATA pSrvData = s_fileFindSrvData( conn, iStreamID, iType );

      if( pSrvData )
      {
         if( pSrvData->size < pSrvData->maxsize )
         {
            if( iType == NETIO_SRVITEM )
            {
               HB_SIZE nSize = len;
               const char * data = buffer;
               PHB_ITEM pItem = hb_itemDeserialize( &data, &nSize );

               if( pItem )
               {
                  if( nSize == 0 )
                  {
                     if( pSrvData->array == NULL )
                        pSrvData->array = hb_itemArrayNew( 0 );
                     if( hb_arrayLen( pSrvData->array ) < pSrvData->maxsize )
                        hb_arrayAddForward( pSrvData->array, pItem );
                  }
                  hb_itemRelease( pItem );
               }
            }
            else if( iType == NETIO_SRVDATA )
            {
               long lmax = ( long ) ( pSrvData->maxsize - pSrvData->size );

               if( len > lmax )
                  len = lmax;
               if( pSrvData->size + len > pSrvData->bufsize )
               {
                  pSrvData->bufsize = ( pSrvData->size + len ) << 1;
                  if( pSrvData->bufsize > pSrvData->maxsize )
                     pSrvData->bufsize = pSrvData->maxsize;
                  pSrvData->data = ( char * ) hb_xrealloc( pSrvData->data,
                                                           pSrvData->bufsize );
               }
               memcpy( pSrvData->data + pSrvData->size, buffer, len );
               pSrvData->size += len;
            }
         }
      }
      fResult = HB_TRUE;
   }
   else
   {
      conn->errcode = hb_socketGetError();
      hb_errRT_NETIO( EG_READ, 1001, conn->errcode, NULL, HB_ERR_FUNCNAME );
   }
   hb_xfree( buffer );

   return fResult;
}

static HB_BOOL s_fileSendMsg( PHB_CONCLI conn, HB_BYTE * msgbuf,
                              const void * data, long len,
                              HB_BOOL fWait, HB_BOOL fNoError )
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
      if( conn->zstream &&
          hb_znetFlush( conn->zstream, conn->sd, NETIO_TIMEOUT ) != 0 )
      {
         conn->errcode = hb_socketGetError();
         if( ! fNoError )
            hb_errRT_NETIO( EG_WRITE, 1002, conn->errcode, NULL, HB_ERR_FUNCNAME );
      }
      else if( fWait )
      {
         int iMsg = HB_GET_LE_INT32( msgbuf ), iResult;

         for( ;; )
         {
            if( s_fileRecvAll( conn, msgbuf, NETIO_MSGLEN ) != NETIO_MSGLEN )
            {
               conn->errcode = hb_socketGetError();
               if( ! fNoError )
                  hb_errRT_NETIO( EG_READ, 1003, conn->errcode, NULL, HB_ERR_FUNCNAME );
               break;
            }

            iResult = HB_GET_LE_INT32( msgbuf );

            if( iResult == NETIO_SRVITEM || iResult == NETIO_SRVDATA )
            {
               int iStreamID = HB_GET_LE_UINT32( &msgbuf[ 4 ] );

               len = HB_GET_LE_INT32( &msgbuf[ 8 ] );
               if( len > 0 )
               {
                  if( ! s_fileRecvSrvData( conn, len, iStreamID, iResult ) )
                     break;
               }
            }
            else if( iResult != NETIO_SYNC )
            {
               if( iResult == NETIO_ERROR )
               {
                  conn->errcode = ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 4 ] );
                  hb_fsSetError( conn->errcode );
               }
               else if( iResult != iMsg )
               {
                  conn->errcode = NETIO_ERR_UNKNOWN_COMMAND;
                  if( ! fNoError )
                     hb_errRT_NETIO( EG_UNSUPPORTED, 1004, 0, NULL, HB_ERR_FUNCNAME );
               }
               else
                  fResult = HB_TRUE;
               break;
            }
         }
      }
      else
         fResult = HB_TRUE;
   }
   else
   {
      conn->errcode = hb_socketGetError();
      if( ! fNoError )
         hb_errRT_NETIO( EG_WRITE, 1005, conn->errcode, NULL, HB_ERR_FUNCNAME );
   }

   return fResult;
}

static HB_BOOL s_fileProcessData( PHB_CONCLI conn )
{
   HB_BYTE msgbuf[ NETIO_MSGLEN ];
   HB_BOOL fResult = HB_TRUE;
   int iMsg, iStreamID;
   long len;

   for( ;; )
   {
      len = s_fileRecvTest( conn, msgbuf, NETIO_MSGLEN );
      if( len == NETIO_MSGLEN )
      {
         iMsg = HB_GET_LE_INT32( msgbuf );
         if( iMsg == NETIO_SRVITEM || iMsg == NETIO_SRVDATA )
         {
            iStreamID = HB_GET_LE_INT32( &msgbuf[ 4 ] );
            len = HB_GET_LE_INT32( &msgbuf[ 8 ] );
            if( len > 0 )
            {
               if( ! s_fileRecvSrvData( conn, len, iStreamID, iMsg ) )
               {
                  fResult = HB_FALSE;
                  break;
               }
            }
         }
         else if( iMsg == NETIO_ERROR )
         {
            conn->errcode = ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 4 ] );
            hb_fsSetError( conn->errcode );
         }
         else if( iMsg != NETIO_SYNC )
         {
            fResult = HB_FALSE;
            conn->errcode = NETIO_ERR_UNKNOWN_COMMAND;
            hb_errRT_NETIO( EG_UNSUPPORTED, 1006, 0, NULL, HB_ERR_FUNCNAME );
            break;
         }
      }
      else
      {
         if( len != 0 )
         {
            fResult = HB_FALSE;
            conn->errcode = hb_socketGetError();
            hb_errRT_NETIO( EG_READ, 1007, conn->errcode, NULL, HB_ERR_FUNCNAME );
         }
         break;
      }
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
   conn->errcode = 0;
   conn->sd = sd;
   conn->zstream = NULL;
   conn->srvdata = NULL;
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
   while( conn->srvdata )
   {
      PHB_SRVDATA pSrvData = conn->srvdata;
      conn->srvdata = pSrvData->next;
      if( pSrvData->array )
         hb_itemRelease( pSrvData->array );
      if( pSrvData->data )
         hb_xfree( pSrvData->data );
      hb_xfree( pSrvData );
   }
   if( conn->zstream )
      hb_znetClose( conn->zstream );
   if( conn->mutex )
      hb_itemRelease( conn->mutex );
   hb_xfree( conn );
}

static void s_fileConRegister( PHB_CONCLI conn )
{
   PHB_CONCLI * connPtr;

   HB_NETIO_LOCK();
   connPtr = &s_connections;
   while( *connPtr )
      connPtr = &( *connPtr )->next;
   *connPtr = conn;
   HB_NETIO_UNLOCK();
}

static void s_fileConClose( PHB_CONCLI conn )
{
   if( hb_atomic_dec( &conn->used ) )
   {
      HB_NETIO_LOCK();
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
      HB_NETIO_UNLOCK();

      if( conn )
         s_fileConFree( conn );
   }
}

static PHB_CONCLI s_fileConFind( const char * pszServer, int iPort )
{
   PHB_CONCLI conn;

   HB_NETIO_LOCK();
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
   HB_NETIO_UNLOCK();

   return conn;
}

static HB_BOOL s_fileUsrDisconnect( const char * pszServer, int iPort )
{
   PHB_CONCLI conn, connClose = NULL;

   HB_NETIO_LOCK();
   conn = s_connections;
   while( conn )
   {
      if( conn->port == iPort && hb_stricmp( conn->server, pszServer ) == 0 )
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
   HB_NETIO_UNLOCK();

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
   return ! conn->mutex || hb_threadMutexLock( conn->mutex );
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

static const char * s_fileDecode( const char * pszFileName,
                                  char * buffer, const char ** pServer,
                                  int * piPort, int * piTimeOut,
                                  const char ** pPasswd, int * piPassLen,
                                  int * piLevel, int * piStrategy )
{
   HB_SYMBOL_UNUSED( piTimeOut );
   HB_SYMBOL_UNUSED( piLevel );
   HB_SYMBOL_UNUSED( piStrategy );

   if( pszFileName )
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

      if( ( pszFileName[ 0 ] == '/' || pszFileName[ 0 ] == '\\' ) &&
          pszFileName[ 0 ] == pszFileName[ 1 ] )
      {
         pszFileName += 2;
         pth = strchr( pszFileName, '/' );
         psz = strchr( pszFileName, '\\' );
         if( ! pth || ( psz && psz < pth ) )
         {
            pth = psz;
            if( ! pth )
               pth = pszFileName + strlen( pszFileName );
         }
      }

      psz = strchr( pszFileName, ':' );
      if( pth && ( ! psz || pth < psz ) )
         psz = pth;

      if( psz )
      {
         int iLen = ( int ) ( psz - pszFileName );

         if( pth || iLen == 0 || iLen > 1 )
         {
            char port_buf[ 10 ], c;

            if( iLen >= NETIO_SERVERNAME_MAX )
               iLen = NETIO_SERVERNAME_MAX - 1;
            if( iLen > 0 )
            {
               hb_strncpy( buffer, pszFileName, iLen );
               *pServer = buffer;
            }
            pszFileName = psz + 1;
            if( ! pth || psz < pth )
            {
               iLen = 0;
               while( HB_ISDIGIT( pszFileName[ iLen ] ) &&
                      iLen < ( int ) sizeof( port_buf ) - 1 )
               {
                  port_buf[ iLen ] = pszFileName[ iLen ];
                  ++iLen;
               }
               c = pszFileName[ iLen ];
               if( c == ':' || c == '/' || c == '\\' )
               {
                  if( iLen > 0 )
                  {
                     int iOverflow;
                     HB_MAXINT llPort;

                     port_buf[ iLen ] = '\0';
                     llPort = hb_strValInt( port_buf, &iOverflow );

                     if( ! iOverflow && llPort > 0 && llPort < 0x10000 )
                     {
                        pszFileName += iLen;
                        *piPort = ( int ) llPort;
                     }
                  }
                  if( c == ':' )
                  {
                     ++pszFileName;
                     iLen = 0;
                     while( pszFileName[ iLen ] &&
                            pszFileName[ iLen ] != ':' )
                        ++iLen;
                     if( pszFileName[ iLen ] == ':' )
                     {
                        if( pPasswd )
                           *pPasswd = pszFileName;
                        pszFileName += iLen + 1;
                        if( iLen > NETIO_PASSWD_MAX )
                           iLen = NETIO_PASSWD_MAX;
                        if( piPassLen )
                           *piPassLen = iLen;
                     }
                  }
               }
            }
         }
      }
   }

   return pszFileName;
}

static PHB_CONCLI s_fileConnCheck( PHB_CONCLI conn, const char ** pFileName )
{
   if( conn )
   {
      char server[ NETIO_SERVERNAME_MAX ];
      const char * pszServer = NULL;
      char * pszIpAddres;
      int iPort = 0;

      s_fileGetConnParam( &pszServer, &iPort, NULL, NULL, NULL );
      *pFileName = s_fileDecode( *pFileName, server,
                                 &pszServer, &iPort, NULL,
                                 NULL, NULL, NULL, NULL );

      pszIpAddres = hb_socketResolveAddr( pszServer, HB_SOCKET_AF_INET );
      if( pszIpAddres == NULL || s_fileConFind( pszIpAddres, iPort ) != conn )
         conn = NULL;
      if( pszIpAddres )
         hb_xfree( pszIpAddres );
   }

   return conn;
}

static PHB_CONCLI s_fileConnect( const char ** pFileName,
                                 const char * pszServer,
                                 int iPort, int iTimeOut, HB_BOOL fNoError,
                                 const char * pszPasswd, int iPassLen,
                                 int iLevel, int iStrategy )
{
   PHB_CONCLI conn;
   HB_SOCKET sd;
   char server[ NETIO_SERVERNAME_MAX ];
   char * pszIpAddres;

   s_fileGetConnParam( &pszServer, &iPort, &iTimeOut, &pszPasswd, &iPassLen );

   if( pFileName )
      *pFileName = s_fileDecode( *pFileName, server,
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
                  if( ! s_fileSendMsg( conn, msgbuf, NETIO_LOGINSTRID, len,
                                       HB_TRUE, fNoError ) ||
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
      HB_NETIO_LOCK();
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
      HB_NETIO_UNLOCK();
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

   if( ! s_fInit )
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
      hb_fileRegisterFull( s_fileMethods() );
      hb_vmAtQuit( s_netio_exit, NULL );
      s_fInit = HB_FALSE;
   }
}

/* netio_Decode( [@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>], ;
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

/* netio_Connect( [<cServer>], [<nPort>], [<nTimeOut>], ;
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

   conn = s_fileConnect( NULL, pszServer, iPort, iTimeOut, HB_TRUE,
                         pszPasswd, iPassLen, iLevel, iStrategy );
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

static HB_GARBAGE_FUNC( s_concli_destructor )
{
   PHB_CONCLI * conn_ptr = ( PHB_CONCLI * ) Cargo;

   if( *conn_ptr )
   {
      s_fileConClose( *conn_ptr );
      *conn_ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcConCliFuncs =
{
   s_concli_destructor,
   hb_gcDummyMark
};

static PHB_CONCLI s_connParam( int iParam )
{
   PHB_CONCLI * conn_ptr = ( PHB_CONCLI * )
                           hb_parptrGC( &s_gcConCliFuncs, iParam );

   if( conn_ptr )
   {
      PHB_CONCLI conn = *conn_ptr;
      if( conn )
      {
         hb_atomic_inc( &conn->used );
         return conn;
      }
   }
   return NULL;
}

/* netio_GetConnection( [<cServer>], [<nPort>], [<nTimeOut>], ;
 *                      [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
 *       -> <pConnection> | NIL
 */
HB_FUNC( NETIO_GETCONNECTION )
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

   conn = s_fileConnect( NULL, pszServer, iPort, iTimeOut, HB_TRUE,
                         pszPasswd, iPassLen, iLevel, iStrategy );
   if( conn )
   {
      PHB_CONCLI * conn_ptr = ( PHB_CONCLI * ) hb_gcAllocate( sizeof( PHB_CONCLI ),
                                                              &s_gcConCliFuncs );
      *conn_ptr = conn;
      hb_retptrGC( conn_ptr );
   }
}

/* netio_Disconnect( [<cServer>], [<nPort>] ) -> <lOK>
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

static const char * s_netio_params( int iParam, int iMsg, const char * pszName, HB_U32 * pSize, char ** pFree )
{
   int iPCount = iMsg == NETIO_PROCIS ? 0 : hb_pcount();
   char * data = NULL, * itmData;
   HB_SIZE size, itmSize;

   size = strlen( pszName ) + 1;

   while( ++iParam <= iPCount )
   {
      itmData = hb_itemSerialize( hb_param( iParam, HB_IT_ANY ), HB_SERIALIZE_NUMSIZE, &itmSize );
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

static HB_BOOL s_netio_procexec( int iMsg, int iType )
{
   HB_BOOL fResult = HB_FALSE;
   const char * pszProcName;
   PHB_CONCLI conn;
   int iParam = 1;

   conn = s_connParam( 1 );
   if( conn )
      ++iParam;
   pszProcName = hb_parc( iParam );
   if( pszProcName )
   {
      if( ! conn )
         conn = s_fileConnect( &pszProcName, NULL, 0, 0, HB_FALSE,
                               NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
      if( conn )
      {
         if( s_fileConLock( conn ) )
         {
            HB_BYTE msgbuf[ NETIO_MSGLEN ];
            const char * data;
            char * buffer;
            HB_U32 size;
            int iStreamID = 0;

            data = s_netio_params( iParam, iMsg, pszProcName, &size, &buffer );
            HB_PUT_LE_UINT32( &msgbuf[ 0 ], iMsg );
            HB_PUT_LE_UINT32( &msgbuf[ 4 ], size );
            if( iMsg == NETIO_FUNCCTRL )
            {
               iStreamID = s_fileGenSrvDataID( conn );
               HB_PUT_LE_UINT32( &msgbuf[ 8 ], iStreamID );
               HB_PUT_LE_UINT32( &msgbuf[ 12 ], iType );
               memset( msgbuf + 16, '\0', sizeof( msgbuf ) - 16 );
            }
            else
               memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
            fResult = s_fileSendMsg( conn, msgbuf, data, size,
                                     iMsg != NETIO_PROC, HB_FALSE );
            if( fResult && ( iMsg == NETIO_FUNC || iMsg == NETIO_FUNCCTRL ) )
            {
               HB_SIZE nResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );

               if( nResult > 0 )
               {
                  PHB_ITEM pItem;

                  if( nResult > size && buffer )
                  {
                     hb_xfree( buffer );
                     buffer = NULL;
                  }
                  if( buffer == NULL )
                     buffer = ( char * ) hb_xgrab( nResult );
                  nResult = s_fileRecvAll( conn, buffer, ( long ) nResult );
                  data = buffer;
                  pItem = hb_itemDeserialize( &data, &nResult );
                  if( pItem )
                  {
                     if( iMsg == NETIO_FUNCCTRL )
                     {
                        if( iStreamID == hb_itemGetNI( pItem ) )
                           s_fileNewSrvData( conn, iStreamID, iType );
                        else
                           hb_itemPutNI( pItem, -1 );
                     }
                     hb_itemReturnRelease( pItem );
                  }
                  else
                  {
                     conn->errcode = NETIO_ERR_WRONG_PARAM;
                     hb_errRT_NETIO( EG_CORRUPTION, 1008, 0, NULL, HB_ERR_FUNCNAME );
                  }
               }
            }
            if( buffer )
               hb_xfree( buffer );
            s_fileConUnlock( conn );
         }
      }
   }

   if( conn )
      s_fileConClose( conn );

   return fResult;
}

/* check if function/procedure exists on the server side:
 *
 * netio_ProcExists( <cProcName> ) -> <lExists>
 */
HB_FUNC( NETIO_PROCEXISTS )
{
   hb_retl( s_netio_procexec( NETIO_PROCIS, 0 ) );
}

/* execute function/procedure on server the side,
 * do not wait for confirmation:
 *
 * netio_ProcExec( <cProcName> [, <params,...>] ) -> <lSent>
 */
HB_FUNC( NETIO_PROCEXEC )
{
   hb_retl( s_netio_procexec( NETIO_PROC, 0 ) );
}

/* execute function/procedure on the server side and wait for
 * confirmation:
 *
 * netio_ProcExecW( <cProcName> [, <params,...>] ) -> <lExecuted>
 */
HB_FUNC( NETIO_PROCEXECW )
{
   hb_retl( s_netio_procexec( NETIO_PROCW, 0 ) );
}

/* execute function on the server side and wait for its return value:
 *
 * netio_FuncExec( <cFuncName> [, <params,...>] ) -> <xFuncRetVal>
 */
HB_FUNC( NETIO_FUNCEXEC )
{
   s_netio_procexec( NETIO_FUNC, 0 );
}

/* open communication stream/channel which allow to send data
 * asynchronously from server to client:
 *
 * netio_OpenDataStream( <cStreamFuncName> [, <params,...>] ) -> <nStreamID>
 *
 * it executes on the server side:
 *    <cStreamFuncName>( <pConnSock>, <nStreamID> [, <params,...>] )
 * and then check value returned by above function. If it's equal to
 * <nStreamID> then the communication stream is opened and <nStreamID>
 * is returned to the client.
 * The function returns new stream ID or -1 if the communication stream
 * cannot be set.
 */
HB_FUNC( NETIO_OPENDATASTREAM )
{
   s_netio_procexec( NETIO_FUNCCTRL, NETIO_SRVDATA );
}

/* open communication stream/channel which allow to send data
 * asynchronously from server to client:
 *
 * netio_OpenItemStream( <cStreamFuncName> [, <params,...>] ) -> <nStreamID>
 *
 * it executes on the server side:
 *    <cStreamFuncName>( <pConnSock>, <nStreamID> [, <params,...>] )
 * and then check value returned by above function. If it's equal to
 * <nStreamID> then the communication stream is opened and <nStreamID>
 * is returned to the client.
 * The function returns new stream ID or -1 if the communication stream
 * cannot be set.
 */
HB_FUNC( NETIO_OPENITEMSTREAM )
{
   s_netio_procexec( NETIO_FUNCCTRL, NETIO_SRVITEM );
}

static PHB_CONCLI s_netio_getConn( void )
{
   PHB_CONCLI conn = s_connParam( 2 );

   if( ! conn )
   {
      const char * pszServer = hb_parc( 2 );
      char * pszIpAddres;
      int iPort = hb_parni( 3 );

      s_fileGetConnParam( &pszServer, &iPort, NULL, NULL, NULL );
      pszIpAddres = hb_socketResolveAddr( pszServer, HB_SOCKET_AF_INET );
      if( pszIpAddres != NULL )
      {
         conn = s_fileConFind( pszIpAddres, iPort );
         hb_xfree( pszIpAddres );
      }
   }

   return conn;
}

/* close communication stream/channel:
 *
 * netio_CloseStream( <nStreamID>, [<pConnection>] | [[<cServer>], [<nPort>]] )
 *    -> <lOK>
 */
HB_FUNC( NETIO_CLOSESTREAM )
{
   int iStreamID = hb_parni( 1 );
   HB_BOOL fResult = HB_FALSE;

   if( iStreamID )
   {
      PHB_CONCLI conn = s_netio_getConn();

      if( conn )
      {
         if( s_fileConLock( conn ) )
         {
            fResult = s_fileCloseSrvData( conn, iStreamID );
            if( fResult )
            {
               HB_BYTE msgbuf[ NETIO_MSGLEN ];

               HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_SRVCLOSE );
               HB_PUT_LE_UINT32( &msgbuf[ 4 ], iStreamID );
               memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
               s_fileSendMsg( conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE );
            }
            s_fileConUnlock( conn );
         }
         s_fileConClose( conn );
      }
   }
   hb_retl( fResult );
}

/* retrieve data sent from the server by cominication stream
 *
 * netio_GetData( <nStreamID>, [<pConnection>] | [[<cServer>], [<nPort>]] )
 *    -> <aData> | <cData> | NIL
 */
HB_FUNC( NETIO_GETDATA )
{
   int iStreamID = hb_parni( 1 );

   if( iStreamID )
   {
      PHB_CONCLI conn = s_netio_getConn();

      if( conn )
      {
         if( s_fileConLock( conn ) )
         {
            if( s_fileProcessData( conn ) )
            {
               PHB_SRVDATA pSrvData = s_fileFindSrvData( conn, iStreamID, 0 );
               if( pSrvData )
               {
                  if( pSrvData->type == NETIO_SRVITEM )
                  {
                     if( pSrvData->array )
                     {
                        hb_itemReturnForward( pSrvData->array );
                        hb_arrayNew( pSrvData->array, 0 );
                     }
                     else
                        hb_reta( 0 );
                  }
                  else if( pSrvData->type == NETIO_SRVDATA )
                  {
                     hb_retclen( pSrvData->data, pSrvData->size );
                     pSrvData->size = 0;
                  }
               }
            }
            s_fileConUnlock( conn );
         }
         s_fileConClose( conn );
      }
   }
}

/* Client methods
 */
static HB_BOOL s_fileAccept( const char * pszFileName )
{
   return hb_strnicmp( pszFileName, NETIO_FILE_PREFIX, NETIO_FILE_PREFIX_LEN ) == 0;
}

static HB_BOOL s_fileDirExists( const char * pszDirName )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszDirName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszDirName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszDirName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_DIREXISTS );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pszDirName, len, HB_TRUE, HB_FALSE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileDirMake( const char * pszDirName )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszDirName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszDirName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszDirName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_DIRMAKE );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pszDirName, len, HB_TRUE, HB_FALSE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileDirRemove( const char * pszDirName )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszDirName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszDirName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszDirName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_DIRREMOVE );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pszDirName, len, HB_TRUE, HB_FALSE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static double s_fileDirSpace( const char * pszDirName, HB_USHORT uiType )
{
   double dResult = 0.0;
   PHB_CONCLI conn;

   pszDirName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszDirName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszDirName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_DIRSPACE );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], uiType );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         if( s_fileSendMsg( conn, msgbuf, pszDirName, len, HB_TRUE, HB_FALSE ) )
         {
            dResult = ( double ) HB_GET_LE_UINT64( &msgbuf[ 4 ] );
            hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 12 ] ) );
         }
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return dResult;
}

static PHB_ITEM s_fileDirectory( const char * pszDirSpec, const char * pszAttr )
{
   PHB_ITEM pDirArray = NULL;
   PHB_CONCLI conn;

   pszDirSpec += NETIO_FILE_PREFIX_LEN;
   conn = s_fileConnect( &pszDirSpec, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len1 = ( HB_U16 ) ( pszDirSpec ? strlen( pszDirSpec ) : 0 );
         HB_U16 len2 = ( HB_U16 ) ( pszAttr ? strlen( pszAttr ) : 0 );
         HB_BYTE * pBuffer = NULL;

         if( len1 + len2 > 0 )
         {
            pBuffer = ( HB_BYTE * ) hb_xgrab( len1 + len2 );
            if( len1 )
               memcpy( pBuffer, pszDirSpec, len1 );
            if( len2 )
               memcpy( pBuffer + len1, pszAttr, len2 );
         }
         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_DIRECTORY );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len1 );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], len2 );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         if( s_fileSendMsg( conn, msgbuf, pBuffer, len1 + len2, HB_TRUE, HB_FALSE ) )
         {
            HB_ERRCODE errCode = ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 8 ] );
            HB_SIZE nResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] ), nRecv = 0;

            if( nResult > 0 )
            {
               char * buffer = ( char * ) hb_xgrab( nResult );
               const char * data = buffer;

               nRecv = s_fileRecvAll( conn, buffer, ( long ) nResult );
               if( nRecv == nResult )
                  pDirArray = hb_itemDeserialize( &data, &nResult );
               hb_xfree( buffer );
            }
            hb_fsSetError( errCode );
            if( pDirArray == NULL )
            {
               if( nRecv != nResult )
               {
                  conn->errcode = hb_socketGetError();
                  hb_errRT_NETIO( EG_CORRUPTION, 1013, 0, NULL, HB_ERR_FUNCNAME );
               }
               else
               {
                  conn->errcode = NETIO_ERR_WRONG_PARAM;
                  hb_errRT_NETIO( EG_CORRUPTION, 1013, 0, NULL, HB_ERR_FUNCNAME );
               }
            }
         }
         if( pBuffer )
            hb_xfree( pBuffer );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   if( pDirArray == NULL )
      pDirArray = hb_itemArrayNew( 0 );

   return pDirArray;
}

static HB_BOOL s_fileExists( const char * pszFileName, char * pRetPath )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   if( pRetPath )
      hb_strncpy( pRetPath, pszFileName, HB_PATH_MAX - 1 );

   pszFileName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszFileName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszFileName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_EXISTS );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pszFileName, len, HB_TRUE, HB_FALSE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileDelete( const char * pszFileName )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszFileName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszFileName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszFileName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_DELETE );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         fResult = s_fileSendMsg( conn, msgbuf, pszFileName, len, HB_TRUE, HB_FALSE );
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

   conn = s_fileConnect( &pszFileName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
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
         fResult = s_fileSendMsg( conn, msgbuf, pBuffer, len1 + len2,
                                  HB_TRUE, HB_FALSE );
         hb_xfree( pBuffer );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileCopy( const char * pszSrcFile, const char * pszDstFile )
{
   HB_BOOL fResult = HB_FALSE;
   const char * pszSource = pszSrcFile + NETIO_FILE_PREFIX_LEN;
   const char * pszDestin = pszDstFile + NETIO_FILE_PREFIX_LEN;
   PHB_CONCLI conn;

   if( ! s_fileAccept( pszDstFile ) )
      return hb_fsCopy( pszSrcFile, pszDstFile );

   conn = s_fileConnect( &pszSource, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConnCheck( conn, &pszDestin ) == NULL )
         return hb_fsCopy( pszSrcFile, pszDstFile );
      else if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len1 = ( HB_U16 ) strlen( pszSource );
         HB_U16 len2 = ( HB_U16 ) strlen( pszDestin );
         HB_BYTE * pBuffer = ( HB_BYTE * ) hb_xgrab( len1 + len2 );

         memcpy( pBuffer, pszSource, len1 );
         memcpy( pBuffer + len1, pszDestin, len2 );
         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_COPY );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len1 );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], len2 );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         fResult = s_fileSendMsg( conn, msgbuf, pBuffer, len1 + len2,
                                  HB_TRUE, HB_FALSE );
         hb_xfree( pBuffer );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileAttrGet( const char * pszFileName, HB_FATTR * pulAttr )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszFileName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszFileName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszFileName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_ATTRGET );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         if( s_fileSendMsg( conn, msgbuf, pszFileName, len, HB_TRUE, HB_FALSE ) )
         {
            * pulAttr = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
            fResult = HB_TRUE;
         }
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileAttrSet( const char * pszFileName, HB_FATTR ulAttr )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszFileName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszFileName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszFileName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_ATTRSET );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         HB_PUT_LE_UINT32( &msgbuf[ 6 ], ulAttr );
         memset( msgbuf + 10, '\0', sizeof( msgbuf ) - 10 );
         fResult = s_fileSendMsg( conn, msgbuf, pszFileName, len, HB_TRUE, HB_FALSE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileTimeGet( const char * pszFileName, long * plJulian, long * plMillisec )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszFileName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszFileName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszFileName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_FTIMEGET );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         if( s_fileSendMsg( conn, msgbuf, pszFileName, len, HB_TRUE, HB_FALSE ) )
         {
            * plJulian   = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
            * plMillisec = HB_GET_LE_UINT32( &msgbuf[ 8 ] );
            fResult = HB_TRUE;
         }
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileTimeSet( const char * pszFileName, long lJulian, long lMillisec )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszFileName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszFileName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszFileName );

         HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_FTIMESET );
         HB_PUT_LE_UINT16( &msgbuf[  4 ], len );
         HB_PUT_LE_UINT32( &msgbuf[  6 ], lJulian );
         HB_PUT_LE_UINT32( &msgbuf[ 10 ], lMillisec );
         memset( msgbuf + 14, '\0', sizeof( msgbuf ) - 14 );
         fResult = s_fileSendMsg( conn, msgbuf, pszFileName, len, HB_TRUE, HB_FALSE );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileLink( const char * pszExisting, const char * pszNewName )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszExisting += NETIO_FILE_PREFIX_LEN;
   if( s_fileAccept( pszNewName ) )
      pszNewName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszExisting, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len1 = ( HB_U16 ) strlen( pszExisting );
         HB_U16 len2 = ( HB_U16 ) strlen( pszNewName );
         HB_BYTE * pBuffer = ( HB_BYTE * ) hb_xgrab( len1 + len2 );

         memcpy( pBuffer, pszExisting, len1 );
         memcpy( pBuffer + len1, pszNewName, len2 );
         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_LINK );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len1 );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], len2 );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         fResult = s_fileSendMsg( conn, msgbuf, pBuffer, len1 + len2,
                                  HB_TRUE, HB_FALSE );
         hb_xfree( pBuffer );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static HB_BOOL s_fileLinkSym( const char * pszTarget, const char * pszNewName )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_CONCLI conn;

   pszTarget += NETIO_FILE_PREFIX_LEN;
   if( s_fileAccept( pszNewName ) )
      pszNewName += NETIO_FILE_PREFIX_LEN;

   conn = s_fileConnect( &pszTarget, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len1 = ( HB_U16 ) strlen( pszTarget );
         HB_U16 len2 = ( HB_U16 ) strlen( pszNewName );
         HB_BYTE * pBuffer = ( HB_BYTE * ) hb_xgrab( len1 + len2 );

         memcpy( pBuffer, pszTarget, len1 );
         memcpy( pBuffer + len1, pszNewName, len2 );
         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_LINKSYM );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len1 );
         HB_PUT_LE_UINT16( &msgbuf[ 6 ], len2 );
         memset( msgbuf + 8, '\0', sizeof( msgbuf ) - 8 );
         fResult = s_fileSendMsg( conn, msgbuf, pBuffer, len1 + len2,
                                  HB_TRUE, HB_FALSE );
         hb_xfree( pBuffer );
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return fResult;
}

static char * s_fileLinkRead( const char * pszFileName )
{
   char * pszResult = NULL;
   PHB_CONCLI conn;

   pszFileName += NETIO_FILE_PREFIX_LEN;
   conn = s_fileConnect( &pszFileName, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
   if( conn )
   {
      if( s_fileConLock( conn ) )
      {
         HB_BYTE msgbuf[ NETIO_MSGLEN ];
         HB_U16 len = ( HB_U16 ) strlen( pszFileName );

         HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_LINKREAD );
         HB_PUT_LE_UINT16( &msgbuf[ 4 ], len );
         memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );
         if( s_fileSendMsg( conn, msgbuf, pszFileName, len, HB_TRUE, HB_FALSE ) )
         {
            HB_SIZE nResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] ), nRecv = 0;
            HB_ERRCODE errCode = ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 8 ] );

            if( nResult > 0 )
            {
               pszResult = ( char * ) hb_xgrab( nResult + 1 );
               nRecv = s_fileRecvAll( conn, pszResult, ( long ) nResult );
               if( nRecv != nResult )
               {
                  hb_xfree( pszResult );
                  pszResult = NULL;
               }
               else
                  pszResult[ nResult ] = '\0';
            }
            if( nRecv != nResult )
            {
               conn->errcode = hb_socketGetError();
               hb_errRT_NETIO( EG_CORRUPTION, 1014, 0, NULL, HB_ERR_FUNCNAME );
            }
            hb_fsSetError( errCode );
         }
         s_fileConUnlock( conn );
      }
      s_fileConClose( conn );
   }

   return pszResult;
}

static PHB_FILE s_fileOpen( const char * pszFileName, const char * pDefExt,
                            HB_USHORT uiExFlags, const char * pPaths,
                            PHB_ITEM pError )
{
   PHB_FILE pFile = NULL;
   PHB_CONCLI conn;
   const char * pszFile = pszFileName + NETIO_FILE_PREFIX_LEN;

   HB_SYMBOL_UNUSED( pPaths );

   conn = s_fileConnect( &pszFile, NULL, 0, 0, HB_FALSE,
                         NULL, 0, HB_ZLIB_COMPRESSION_DISABLE, 0 );
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

         if( s_fileSendMsg( conn, msgbuf, pszFile, len, HB_TRUE, HB_FALSE ) )
         {
            pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );
            pFile->pFuncs = s_fileMethods();
            pFile->conn = conn;
            pFile->fd = HB_GET_LE_UINT16( &msgbuf[ 4 ] );
         }
         s_fileConUnlock( conn );
      }

      if( ! pFile )
         s_fileConClose( conn );
   }

   if( pError )
   {
      hb_errPutFileName( pError, pszFileName );
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
      s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE );
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

      fResult = s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, ! fUnLock, HB_FALSE );
      s_fileConUnlock( pFile->conn );
   }

   return fResult;
}

static int s_fileLockTest( PHB_FILE pFile, HB_FOFFSET ulStart, HB_FOFFSET ulLen,
                           int iType )
{
   int iResult = -1;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_TESTLOCK );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      HB_PUT_LE_UINT64( &msgbuf[ 6 ], ulStart );
      HB_PUT_LE_UINT64( &msgbuf[ 14 ], ulLen );
      HB_PUT_LE_UINT16( &msgbuf[ 22 ], ( HB_USHORT ) iType );
#if NETIO_MSGLEN > 24
      memset( msgbuf + 24, '\0', sizeof( msgbuf ) - 24 );
#endif

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE ) )
         iResult = HB_GET_LE_INT32( &msgbuf[ 4 ] );

      s_fileConUnlock( pFile->conn );
   }

   return iResult;
}

static HB_SIZE s_fileRead( PHB_FILE pFile, void * data, HB_SIZE ulSize,
                           HB_MAXINT timeout )
{
   HB_SIZE ulResult = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_READ );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[  6 ], ulSize );
      HB_PUT_LE_UINT64( &msgbuf[ 10 ], timeout );
      memset( msgbuf + 18, '\0', sizeof( msgbuf ) - 18 );

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE ) )
      {
         HB_ERRCODE errCode = ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 8 ] );
         ulResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
         if( ulResult > 0 )
         {
            if( ulResult > ulSize ) /* error, it should not happen, enemy attack? */
            {
               pFile->conn->errcode = errCode = NETIO_ERR_WRONG_FILE_SIZE;
               hb_errRT_NETIO( EG_DATAWIDTH, 1011, 0, NULL, HB_ERR_FUNCNAME );
               ulResult = 0;
            }
            else if( s_fileRecvAll( pFile->conn, data, ( long ) ulResult ) != ( long ) ulResult )
            {
               pFile->conn->errcode = hb_socketGetError();
               errCode = NETIO_ERR_READ;
               hb_errRT_NETIO( EG_READ, 1012, pFile->conn->errcode, NULL, HB_ERR_FUNCNAME );
            }
         }
         hb_fsSetError( errCode );
      }
      s_fileConUnlock( pFile->conn );
   }

   return ulResult;
}

static HB_SIZE s_fileWrite( PHB_FILE pFile, const void * data, HB_SIZE ulSize,
                            HB_MAXINT timeout )
{
   HB_SIZE ulResult = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_WRITE );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[  6 ], ( long ) ulSize );
      HB_PUT_LE_UINT64( &msgbuf[ 10 ], timeout );
      memset( msgbuf + 18, '\0', sizeof( msgbuf ) - 18 );

      if( s_fileSendMsg( pFile->conn, msgbuf, data, ( long ) ulSize, HB_TRUE, HB_FALSE ) )
      {
         ulResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
         hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 8 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return ulResult;
}

static HB_SIZE s_fileReadAt( PHB_FILE pFile, void * data, HB_SIZE ulSize,
                             HB_FOFFSET llOffset )
{
   HB_SIZE ulResult = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_READAT );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[  6 ], ulSize );
      HB_PUT_LE_UINT64( &msgbuf[ 10 ], llOffset );
      memset( msgbuf + 18, '\0', sizeof( msgbuf ) - 18 );

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE ) )
      {
         HB_ERRCODE errCode = ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 8 ] );
         ulResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] );
         if( ulResult > 0 )
         {
            if( ulResult > ulSize ) /* error, it should not happen, enemy attack? */
            {
               pFile->conn->errcode = errCode = NETIO_ERR_WRONG_FILE_SIZE;
               hb_errRT_NETIO( EG_DATAWIDTH, 1009, 0, NULL, HB_ERR_FUNCNAME );
               ulResult = 0;
            }
            else if( s_fileRecvAll( pFile->conn, data, ( long ) ulResult ) != ( long ) ulResult )
            {
               pFile->conn->errcode = hb_socketGetError();
               errCode = NETIO_ERR_READ;
               hb_errRT_NETIO( EG_READ, 1010, pFile->conn->errcode, NULL, HB_ERR_FUNCNAME );
            }
         }
         hb_fsSetError( errCode );
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

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_WRITEAT );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[  6 ], ( long ) ulSize );
      HB_PUT_LE_UINT64( &msgbuf[ 10 ], llOffset );
      memset( msgbuf + 18, '\0', sizeof( msgbuf ) - 18 );

      if( s_fileSendMsg( pFile->conn, msgbuf, data, ( long ) ulSize, HB_TRUE, HB_FALSE ) )
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

      fResult = s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE );
      s_fileConUnlock( pFile->conn );
   }

   return fResult;
}

static HB_FOFFSET s_fileSeek( PHB_FILE pFile, HB_FOFFSET llOffset, HB_USHORT uiFlags )
{
   HB_FOFFSET llResult = 0;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( &msgbuf[  0 ], NETIO_SEEK );
      HB_PUT_LE_UINT16( &msgbuf[  4 ], pFile->fd );
      HB_PUT_LE_UINT64( &msgbuf[  6 ], llOffset );
      HB_PUT_LE_UINT16( &msgbuf[ 14 ], uiFlags );
      memset( msgbuf + 16, '\0', sizeof( msgbuf ) - 16 );

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE ) )
      {
         llResult = HB_GET_LE_UINT64( &msgbuf[ 4 ] );
         hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 12 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return llResult;
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

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE ) )
      {
         llOffset = HB_GET_LE_UINT64( &msgbuf[ 4 ] );
         hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 12 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return llOffset;
}

static HB_BOOL s_fileEof( PHB_FILE pFile )
{
   HB_BOOL fEof = HB_TRUE;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];

      HB_PUT_LE_UINT32( msgbuf, NETIO_EOF );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      memset( msgbuf + 6, '\0', sizeof( msgbuf ) - 6 );

      if( s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_TRUE, HB_FALSE ) )
      {
         fEof = HB_GET_LE_UINT16( &msgbuf[ 4 ] ) != 0;
         hb_fsSetError( ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 8 ] ) );
      }
      s_fileConUnlock( pFile->conn );
   }

   return fEof;
}

static void s_fileFlush( PHB_FILE pFile, HB_BOOL fDirty )
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

      s_fileSendMsg( pFile->conn, msgbuf, NULL, 0, HB_FALSE, HB_FALSE );
      s_fileConUnlock( pFile->conn );
   }
}

static HB_BOOL s_fileConfigure( PHB_FILE pFile, int iIndex, PHB_ITEM pValue )
{
   HB_BOOL fResult = HB_FALSE;

   if( s_fileConLock( pFile->conn ) )
   {
      HB_BYTE msgbuf[ NETIO_MSGLEN ];
      HB_SIZE itmSize = 0;
      char * itmData = NULL;

      if( pValue )
         itmData = hb_itemSerialize( pValue, HB_SERIALIZE_NUMSIZE, &itmSize );

      HB_PUT_LE_UINT32( &msgbuf[ 0 ], NETIO_CONFIGURE );
      HB_PUT_LE_UINT16( &msgbuf[ 4 ], pFile->fd );
      HB_PUT_LE_UINT32( &msgbuf[ 6 ], itmSize );
      HB_PUT_LE_UINT32( &msgbuf[ 10 ], iIndex );
      memset( msgbuf + 14, '\0', sizeof( msgbuf ) - 14 );

      hb_itemClear( pValue );

      if( s_fileSendMsg( pFile->conn, msgbuf, itmData, itmSize, HB_TRUE, HB_FALSE ) )
      {
         HB_ERRCODE errCode = ( HB_ERRCODE ) HB_GET_LE_UINT32( &msgbuf[ 12 ] );
         HB_SIZE nResult = HB_GET_LE_UINT32( &msgbuf[ 4 ] ), nRecv = 0;

         fResult = HB_GET_LE_UINT32( &msgbuf[ 8 ] ) != 0;
         if( nResult > 0 )
         {
            char * buffer = ( char * ) hb_xgrab( nResult );
            const char * data = buffer;

            nRecv = s_fileRecvAll( pFile->conn, buffer, ( long ) nResult );
            if( nRecv == nResult && pValue )
            {
               PHB_ITEM pResult = hb_itemDeserialize( &data, &nResult );

               if( pResult )
               {
                  hb_itemMove( pValue, pResult );
                  hb_itemRelease( pResult );
               }
            }
            hb_xfree( buffer );
         }
         hb_fsSetError( errCode );
         if( nRecv != nResult )
         {
            pFile->conn->errcode = hb_socketGetError();
            hb_errRT_NETIO( EG_CORRUPTION, 1015, 0, NULL, HB_ERR_FUNCNAME );
            fResult = HB_FALSE;
         }
      }
      if( itmData )
         hb_xfree( itmData );
      s_fileConUnlock( pFile->conn );
   }

   return fResult;
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
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _hb_file_netio_init_ )
   #include "hbiniseg.h"
#endif

#endif /* HB_NETIO_STARTUP_INIT */
