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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

#include "hbqtcore.h"
#include "hbqtnetwork.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ConnectionMode { ConnectionModeHttp, ConnectionModeHttps }
 *  enum Error { NoError, HostNotFound, ConnectionRefused, UnexpectedClose, ..., UnknownError }
 *  enum State { Unconnected, HostLookup, Connecting, Sending, ..., Closing }
 */

/*
 *  Constructed[ 24/24 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // qint64 read ( char * data, qint64 maxlen )
 *  // int setProxy ( const QNetworkProxy & proxy )
 *  // int setSocket ( QTcpSocket * socket )
 *  //void ignoreSslErrors ()
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
   QPointer< QHttp > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHttp;

HBQT_GC_FUNC( hbqt_gcRelease_QHttp )
{
   QHttp  * ph = NULL ;
   HBQT_GC_T_QHttp * p = ( HBQT_GC_T_QHttp * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QHttp   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QHttp   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QHttp          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QHttp    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QHttp    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QHttp( void * pObj, bool bNew )
{
   HBQT_GC_T_QHttp * p = ( HBQT_GC_T_QHttp * ) hb_gcAllocate( sizeof( HBQT_GC_T_QHttp ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QHttp >( ( QHttp * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHttp;
   p->type = HBQT_TYPE_QHttp;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QHttp  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QHttp", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QHTTP )
{
   QHttp * pObj = NULL;

   pObj = new QHttp( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QHttp( ( void * ) pObj, true ) );
}

/*
 * qint64 bytesAvailable () const
 */
HB_FUNC( QT_QHTTP_BYTESAVAILABLE )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retnint( ( p )->bytesAvailable() );
   }
}

/*
 * void clearPendingRequests ()
 */
HB_FUNC( QT_QHTTP_CLEARPENDINGREQUESTS )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      ( p )->clearPendingRequests();
   }
}

/*
 * int close ()
 */
HB_FUNC( QT_QHTTP_CLOSE )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retni( ( p )->close() );
   }
}

/*
 * QIODevice * currentDestinationDevice () const
 */
HB_FUNC( QT_QHTTP_CURRENTDESTINATIONDEVICE )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->currentDestinationDevice(), false ) );
   }
}

/*
 * int currentId () const
 */
HB_FUNC( QT_QHTTP_CURRENTID )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retni( ( p )->currentId() );
   }
}

/*
 * QHttpRequestHeader currentRequest () const
 */
HB_FUNC( QT_QHTTP_CURRENTREQUEST )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QHttpRequestHeader( new QHttpRequestHeader( ( p )->currentRequest() ), true ) );
   }
}

/*
 * QIODevice * currentSourceDevice () const
 */
HB_FUNC( QT_QHTTP_CURRENTSOURCEDEVICE )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->currentSourceDevice(), false ) );
   }
}

/*
 * Error error () const
 */
HB_FUNC( QT_QHTTP_ERROR )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retni( ( QHttp::Error ) ( p )->error() );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QHTTP_ERRORSTRING )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
   }
}

/*
 * int get ( const QString & path, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_GET )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->get( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QIODevice( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool hasPendingRequests () const
 */
HB_FUNC( QT_QHTTP_HASPENDINGREQUESTS )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retl( ( p )->hasPendingRequests() );
   }
}

/*
 * int head ( const QString & path )
 */
HB_FUNC( QT_QHTTP_HEAD )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->head( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QHttpResponseHeader lastResponse () const
 */
HB_FUNC( QT_QHTTP_LASTRESPONSE )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QHttpResponseHeader( new QHttpResponseHeader( ( p )->lastResponse() ), true ) );
   }
}

/*
 * int post ( const QString & path, QIODevice * data, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_POST )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->post( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QIODevice( 3 ), hbqt_par_QIODevice( 4 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int post ( const QString & path, const QByteArray & data, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_POST_1 )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->post( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QByteArray( 3 ), hbqt_par_QIODevice( 4 ) ) );
      hb_strfree( pText );
   }
}

/*
 * QByteArray readAll ()
 */
HB_FUNC( QT_QHTTP_READALL )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAll() ), true ) );
   }
}

/*
 * int request ( const QHttpRequestHeader & header, QIODevice * data = 0, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_REQUEST )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retni( ( p )->request( *hbqt_par_QHttpRequestHeader( 2 ), hbqt_par_QIODevice( 3 ), hbqt_par_QIODevice( 4 ) ) );
   }
}

/*
 * int request ( const QHttpRequestHeader & header, const QByteArray & data, QIODevice * to = 0 )
 */
HB_FUNC( QT_QHTTP_REQUEST_1 )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retni( ( p )->request( *hbqt_par_QHttpRequestHeader( 2 ), *hbqt_par_QByteArray( 3 ), hbqt_par_QIODevice( 4 ) ) );
   }
}

/*
 * int setHost ( const QString & hostName, quint16 port = 80 )
 */
HB_FUNC( QT_QHTTP_SETHOST )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->setHost( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, 80 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int setHost ( const QString & hostName, ConnectionMode mode, quint16 port = 0 )
 */
HB_FUNC( QT_QHTTP_SETHOST_1 )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->setHost( hb_parstr_utf8( 2, &pText, NULL ), ( QHttp::ConnectionMode ) hb_parni( 3 ), hb_parni( 4 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int setProxy ( const QString & host, int port, const QString & username = QString(), const QString & password = QString() )
 */
HB_FUNC( QT_QHTTP_SETPROXY )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->setProxy( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ), hb_parstr_utf8( 5, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * int setUser ( const QString & userName, const QString & password = QString() )
 */
HB_FUNC( QT_QHTTP_SETUSER )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->setUser( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * State state () const
 */
HB_FUNC( QT_QHTTP_STATE )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      hb_retni( ( QHttp::State ) ( p )->state() );
   }
}

/*
 * void abort ()
 */
HB_FUNC( QT_QHTTP_ABORT )
{
   QHttp * p = hbqt_par_QHttp( 1 );
   if( p )
   {
      ( p )->abort();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
