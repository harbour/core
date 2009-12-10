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
 *  enum Attribute { HttpStatusCodeAttribute, HttpReasonPhraseAttribute, RedirectionTargetAttribute, ConnectionEncryptedAttribute, ..., UserMax }
 *  enum CacheLoadControl { AlwaysNetwork, PreferNetwork, PreferCache, AlwaysCache }
 *  enum KnownHeaders { ContentTypeHeader, ContentLengthHeader, LocationHeader, LastModifiedHeader, CookieHeader, SetCookieHeader }
 */

/*
 *  Constructed[ 9/12 [ 75.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QByteArray> rawHeaderList () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // void setSslConfiguration ( const QSslConfiguration & config )
 *  // QSslConfiguration sslConfiguration () const
 */

#include <QtCore/QPointer>

#include <QtNetwork/QNetworkRequest>


/* QNetworkRequest ( const QUrl & url = QUrl() )
 * QNetworkRequest ( const QNetworkRequest & other )
 * ~QNetworkRequest ()
 */

QT_G_FUNC( release_QNetworkRequest )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QNetworkRequest              p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QNetworkRequest             ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QNetworkRequest * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES release_QNetworkRequest             Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QNetworkRequest             Object Allready deleted!" ) );
   }
}

void * gcAllocate_QNetworkRequest( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QNetworkRequest;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QNetworkRequest             %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QNETWORKREQUEST )
{
   void * pObj = NULL;

   pObj = new QNetworkRequest() ;

   hb_retptrGC( gcAllocate_QNetworkRequest( pObj ) );
}
/*
 * QVariant attribute ( Attribute code, const QVariant & defaultValue = QVariant() ) const
 */
HB_FUNC( QT_QNETWORKREQUEST_ATTRIBUTE )
{
   hb_retptrGC( gcAllocate_QVariant( new QVariant( hbqt_par_QNetworkRequest( 1 )->attribute( ( QNetworkRequest::Attribute ) hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) ) ) ) );
}

/*
 * bool hasRawHeader ( const QByteArray & headerName ) const
 */
HB_FUNC( QT_QNETWORKREQUEST_HASRAWHEADER )
{
   hb_retl( hbqt_par_QNetworkRequest( 1 )->hasRawHeader( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * QVariant header ( KnownHeaders header ) const
 */
HB_FUNC( QT_QNETWORKREQUEST_HEADER )
{
   hb_retptrGC( gcAllocate_QVariant( new QVariant( hbqt_par_QNetworkRequest( 1 )->header( ( QNetworkRequest::KnownHeaders ) hb_parni( 2 ) ) ) ) );
}

/*
 * QByteArray rawHeader ( const QByteArray & headerName ) const
 */
HB_FUNC( QT_QNETWORKREQUEST_RAWHEADER )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QNetworkRequest( 1 )->rawHeader( *hbqt_par_QByteArray( 2 ) ) ) ) );
}

/*
 * void setAttribute ( Attribute code, const QVariant & value )
 */
HB_FUNC( QT_QNETWORKREQUEST_SETATTRIBUTE )
{
   hbqt_par_QNetworkRequest( 1 )->setAttribute( ( QNetworkRequest::Attribute ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/*
 * void setHeader ( KnownHeaders header, const QVariant & value )
 */
HB_FUNC( QT_QNETWORKREQUEST_SETHEADER )
{
   hbqt_par_QNetworkRequest( 1 )->setHeader( ( QNetworkRequest::KnownHeaders ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/*
 * void setRawHeader ( const QByteArray & headerName, const QByteArray & headerValue )
 */
HB_FUNC( QT_QNETWORKREQUEST_SETRAWHEADER )
{
   hbqt_par_QNetworkRequest( 1 )->setRawHeader( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) );
}

/*
 * void setUrl ( const QUrl & url )
 */
HB_FUNC( QT_QNETWORKREQUEST_SETURL )
{
   hbqt_par_QNetworkRequest( 1 )->setUrl( *hbqt_par_QUrl( 2 ) );
}

/*
 * QUrl url () const
 */
HB_FUNC( QT_QNETWORKREQUEST_URL )
{
   hb_retptrGC( gcAllocate_QUrl( new QUrl( hbqt_par_QNetworkRequest( 1 )->url() ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
