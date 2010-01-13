/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ConnectionMode { ConnectionModeHttp, ConnectionModeHttps }
 *  enum Error { NoError, HostNotFound, ConnectionRefused, UnexpectedClose, ..., UnknownError }
 *  enum State { Unconnected, HostLookup, Connecting, Sending, ..., Closing }
 */

#include <QtCore/QPointer>

#include <QtNetwork/QHttp>


/*
 * QHttp ( QObject * parent = 0 )
 * QHttp ( const QString & hostName, quint16 port = 80, QObject * parent = 0 )
 * QHttp ( const QString & hostName, ConnectionMode mode, quint16 port = 0, QObject * parent = 0 )
 * virtual ~QHttp ()
 */

/*
 * qint64 read ( char * data, qint64 maxlen )
 */
HB_FUNC( QT_QHTTP_READ )
{
   char * iData = ( char * ) hb_xgrab( hb_parnint( 3 ) + 1 );
   qint64 iRead;

   iRead = hbqt_par_QHttp( 1 )->read( iData, hb_parnint( 3 ) );

   hb_retnint( iRead );
   if( ! hb_storclen_buffer( iData, iRead, 2 ) )
      hb_xfree( iData );
}

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QHttp > pq;
} QGC_POINTER_QHttp;

QT_G_FUNC( hbqt_gcRelease_QHttp )
{
   QGC_POINTER_QHttp * p = ( QGC_POINTER_QHttp * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QHttp                        p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QHttp                       ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QHttp * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QHttp * ) p->ph )->~QHttp();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QHttp * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QHttp                       Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QHttp                       Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QHttp                       Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QHttp( void * pObj )
{
   QGC_POINTER_QHttp * p = ( QGC_POINTER_QHttp * ) hb_gcAllocate( sizeof( QGC_POINTER_QHttp ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QHttp;
   new( & p->pq ) QPointer< QHttp >( ( QHttp * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QHttp                       %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QHTTP )
{
   void * pObj = NULL;

   pObj = new QHttp( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QHttp( pObj ) );
}
/*
 * qint64 bytesAvailable () const
 */
HB_FUNC( QT_QHTTP_BYTESAVAILABLE )
{
   hb_retnint( hbqt_par_QHttp( 1 )->bytesAvailable() );
}

/*
 * void clearPendingRequests ()
 */
HB_FUNC( QT_QHTTP_CLEARPENDINGREQUESTS )
{
   hbqt_par_QHttp( 1 )->clearPendingRequests();
}

/*
 * int close ()
 */
HB_FUNC( QT_QHTTP_CLOSE )
{
   hb_retni( hbqt_par_QHttp( 1 )->close() );
}

/*
 * QIODevice * currentDestinationDevice () const
 */
HB_FUNC( QT_QHTTP_CURRENTDESTINATIONDEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QHttp( 1 )->currentDestinationDevice() );
}

/*
 * int currentId () const
 */
HB_FUNC( QT_QHTTP_CURRENTID )
{
   hb_retni( hbqt_par_QHttp( 1 )->currentId() );
}

/*
 * QHttpRequestHeader currentRequest () const
 */
HB_FUNC( QT_QHTTP_CURRENTREQUEST )
{
   hb_retptrGC( hbqt_gcAllocate_QHttpRequestHeader( new QHttpRequestHeader( hbqt_par_QHttp( 1 )->currentRequest() ) ) );
}

/*
 * QIODevice * currentSourceDevice () const
 */
HB_FUNC( QT_QHTTP_CURRENTSOURCEDEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QHttp( 1 )->currentSourceDevice() );
}

/*
 * Error error () const
 */
HB_FUNC( QT_QHTTP_ERROR )
{
   hb_retni( ( QHttp::Error ) hbqt_par_QHttp( 1 )->error() );
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QHTTP_ERRORSTRING )
{
   hb_retc( hbqt_par_QHttp( 1 )->errorString().toAscii().data() );
}

/*
 * int get ( const QString & path, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_GET )
{
   hb_retni( hbqt_par_QHttp( 1 )->get( QHttp::tr( hb_parc( 2 ) ), hbqt_par_QIODevice( 3 ) ) );
}

/*
 * bool hasPendingRequests () const
 */
HB_FUNC( QT_QHTTP_HASPENDINGREQUESTS )
{
   hb_retl( hbqt_par_QHttp( 1 )->hasPendingRequests() );
}

/*
 * int head ( const QString & path )
 */
HB_FUNC( QT_QHTTP_HEAD )
{
   hb_retni( hbqt_par_QHttp( 1 )->head( QHttp::tr( hb_parc( 2 ) ) ) );
}

/*
 * QHttpResponseHeader lastResponse () const
 */
HB_FUNC( QT_QHTTP_LASTRESPONSE )
{
   hb_retptrGC( hbqt_gcAllocate_QHttpResponseHeader( new QHttpResponseHeader( hbqt_par_QHttp( 1 )->lastResponse() ) ) );
}

/*
 * int post ( const QString & path, QIODevice * data, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_POST )
{
   hb_retni( hbqt_par_QHttp( 1 )->post( QHttp::tr( hb_parc( 2 ) ), hbqt_par_QIODevice( 3 ), hbqt_par_QIODevice( 4 ) ) );
}

/*
 * int post ( const QString & path, const QByteArray & data, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_POST_1 )
{
   hb_retni( hbqt_par_QHttp( 1 )->post( QHttp::tr( hb_parc( 2 ) ), *hbqt_par_QByteArray( 3 ), hbqt_par_QIODevice( 4 ) ) );
}

/*
 * QByteArray readAll ()
 */
HB_FUNC( QT_QHTTP_READALL )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QHttp( 1 )->readAll() ) ) );
}

/*
 * int request ( const QHttpRequestHeader & header, QIODevice * data = 0, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_REQUEST )
{
   hb_retni( hbqt_par_QHttp( 1 )->request( *hbqt_par_QHttpRequestHeader( 2 ), hbqt_par_QIODevice( 3 ), hbqt_par_QIODevice( 4 ) ) );
}

/*
 * int request ( const QHttpRequestHeader & header, const QByteArray & data, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_REQUEST_1 )
{
   hb_retni( hbqt_par_QHttp( 1 )->request( *hbqt_par_QHttpRequestHeader( 2 ), *hbqt_par_QByteArray( 3 ), hbqt_par_QIODevice( 4 ) ) );
}

/*
 * int setHost ( const QString & hostName, quint16 port = 80 )
 */
HB_FUNC( QT_QHTTP_SETHOST )
{
   hb_retni( hbqt_par_QHttp( 1 )->setHost( QHttp::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 80 ) ) );
}

/*
 * int setHost ( const QString & hostName, ConnectionMode mode, quint16 port = 0 )
 */
HB_FUNC( QT_QHTTP_SETHOST_1 )
{
   hb_retni( hbqt_par_QHttp( 1 )->setHost( QHttp::tr( hb_parc( 2 ) ), ( QHttp::ConnectionMode ) hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 * int setProxy ( const QString & host, int port, const QString & username = QString(), const QString & password = QString() )
 */
HB_FUNC( QT_QHTTP_SETPROXY )
{
   hb_retni( hbqt_par_QHttp( 1 )->setProxy( QHttp::tr( hb_parc( 2 ) ), hb_parni( 3 ), QHttp::tr( hb_parc( 4 ) ), QHttp::tr( hb_parc( 5 ) ) ) );
}

/*
 * int setUser ( const QString & userName, const QString & password = QString() )
 */
HB_FUNC( QT_QHTTP_SETUSER )
{
   hb_retni( hbqt_par_QHttp( 1 )->setUser( QHttp::tr( hb_parc( 2 ) ), QHttp::tr( hb_parc( 3 ) ) ) );
}

/*
 * State state () const
 */
HB_FUNC( QT_QHTTP_STATE )
{
   hb_retni( ( QHttp::State ) hbqt_par_QHttp( 1 )->state() );
}

/*
 * void abort ()
 */
HB_FUNC( QT_QHTTP_ABORT )
{
   hbqt_par_QHttp( 1 )->abort();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
