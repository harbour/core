/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


#include <QtNetwork/QHttp>


/*
 * QHttp ( QObject * parent = 0 )
 * QHttp ( const QString & hostName, quint16 port = 80, QObject * parent = 0 )
 * QHttp ( const QString & hostName, ConnectionMode mode, quint16 port = 0, QObject * parent = 0 )
 * virtual ~QHttp ()
 */
HB_FUNC( QT_QHTTP )
{
   hb_retptr( new QHttp( hbqt_par_QObject( 1 ) ) );
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
   hb_retptr( new QHttpRequestHeader( hbqt_par_QHttp( 1 )->currentRequest() ) );
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
   hb_retc( hbqt_par_QHttp( 1 )->errorString().toLatin1().data() );
}

/*
 * int get ( const QString & path, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_GET )
{
   hb_retni( hbqt_par_QHttp( 1 )->get( hbqt_par_QString( 2 ), hbqt_par_QIODevice( 3 ) ) );
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
   hb_retni( hbqt_par_QHttp( 1 )->head( hbqt_par_QString( 2 ) ) );
}

/*
 * QHttpResponseHeader lastResponse () const
 */
HB_FUNC( QT_QHTTP_LASTRESPONSE )
{
   hb_retptr( new QHttpResponseHeader( hbqt_par_QHttp( 1 )->lastResponse() ) );
}

/*
 * int post ( const QString & path, QIODevice * data, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_POST )
{
   hb_retni( hbqt_par_QHttp( 1 )->post( hbqt_par_QString( 2 ), hbqt_par_QIODevice( 3 ), hbqt_par_QIODevice( 4 ) ) );
}

/*
 * int post ( const QString & path, const QByteArray & data, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_POST_1 )
{
   hb_retni( hbqt_par_QHttp( 1 )->post( hbqt_par_QString( 2 ), *hbqt_par_QByteArray( 3 ), hbqt_par_QIODevice( 4 ) ) );
}

/*
 * qint64 read ( char * data, qint64 maxlen )
 */
HB_FUNC( QT_QHTTP_READ )
{
   hb_retnint( hbqt_par_QHttp( 1 )->read( hbqt_par_char( 2 ), hb_parnint( 3 ) ) );
}

/*
 * QByteArray readAll ()
 */
HB_FUNC( QT_QHTTP_READALL )
{
   hb_retptr( new QByteArray( hbqt_par_QHttp( 1 )->readAll() ) );
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
   hb_retni( hbqt_par_QHttp( 1 )->setHost( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 80 ) ) );
}

/*
 * int setHost ( const QString & hostName, ConnectionMode mode, quint16 port = 0 )
 */
HB_FUNC( QT_QHTTP_SETHOST_1 )
{
   hb_retni( hbqt_par_QHttp( 1 )->setHost( hbqt_par_QString( 2 ), ( QHttp::ConnectionMode ) hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 * int setProxy ( const QString & host, int port, const QString & username = QString(), const QString & password = QString() )
 */
HB_FUNC( QT_QHTTP_SETPROXY )
{
   hb_retni( hbqt_par_QHttp( 1 )->setProxy( hbqt_par_QString( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hbqt_par_QString( 5 ) ) );
}

/*
 * int setProxy ( const QNetworkProxy & proxy )
 */
HB_FUNC( QT_QHTTP_SETPROXY_1 )
{
   hb_retni( hbqt_par_QHttp( 1 )->setProxy( *hbqt_par_QNetworkProxy( 2 ) ) );
}

/*
 * int setSocket ( QTcpSocket * socket )
 */
HB_FUNC( QT_QHTTP_SETSOCKET )
{
   hb_retni( hbqt_par_QHttp( 1 )->setSocket( hbqt_par_QTcpSocket( 2 ) ) );
}

/*
 * int setUser ( const QString & userName, const QString & password = QString() )
 */
HB_FUNC( QT_QHTTP_SETUSER )
{
   hb_retni( hbqt_par_QHttp( 1 )->setUser( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
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

