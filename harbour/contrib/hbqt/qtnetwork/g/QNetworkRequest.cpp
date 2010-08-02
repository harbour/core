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

#include "hbqt.h"
#include "hbqtnetwork_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Attribute { HttpStatusCodeAttribute, HttpReasonPhraseAttribute, RedirectionTargetAttribute, ConnectionEncryptedAttribute, ..., UserMax }
 *  enum CacheLoadControl { AlwaysNetwork, PreferNetwork, PreferCache, AlwaysCache }
 *  enum KnownHeaders { ContentTypeHeader, ContentLengthHeader, LocationHeader, LastModifiedHeader, CookieHeader, SetCookieHeader }
 */

#include <QtCore/QPointer>

#include <QtNetwork/QNetworkRequest>


/* QNetworkRequest ( const QUrl & url = QUrl() )
 * QNetworkRequest ( const QNetworkRequest & other )
 * ~QNetworkRequest ()
 */

typedef struct
{
   QNetworkRequest * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QNetworkRequest;

QT_G_FUNC( hbqt_gcRelease_QNetworkRequest )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QNetworkRequest   /.\\", p->ph ) );
         delete ( ( QNetworkRequest * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QNetworkRequest   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QNetworkRequest    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QNetworkRequest    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QNetworkRequest( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QNetworkRequest * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QNetworkRequest;
   p->type = HBQT_TYPE_QNetworkRequest;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QNetworkRequest", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QNetworkRequest", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QNETWORKREQUEST )
{
   QNetworkRequest * pObj = NULL;

   pObj = new QNetworkRequest() ;

   hb_retptrGC( hbqt_gcAllocate_QNetworkRequest( ( void * ) pObj, true ) );
}

/*
 * QVariant attribute ( Attribute code, const QVariant & defaultValue = QVariant() ) const
 */
HB_FUNC( QT_QNETWORKREQUEST_ATTRIBUTE )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->attribute( ( QNetworkRequest::Attribute ) hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_ATTRIBUTE FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->attribute( ( QNetworkRequest::Attribute ) hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool hasRawHeader ( const QByteArray & headerName ) const
 */
HB_FUNC( QT_QNETWORKREQUEST_HASRAWHEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retl( ( p )->hasRawHeader( *hbqt_par_QByteArray( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_HASRAWHEADER FP=hb_retl( ( p )->hasRawHeader( *hbqt_par_QByteArray( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QVariant header ( KnownHeaders header ) const
 */
HB_FUNC( QT_QNETWORKREQUEST_HEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->header( ( QNetworkRequest::KnownHeaders ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_HEADER FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->header( ( QNetworkRequest::KnownHeaders ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray rawHeader ( const QByteArray & headerName ) const
 */
HB_FUNC( QT_QNETWORKREQUEST_RAWHEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->rawHeader( *hbqt_par_QByteArray( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_RAWHEADER FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->rawHeader( *hbqt_par_QByteArray( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QByteArray> rawHeaderList () const
 */
HB_FUNC( QT_QNETWORKREQUEST_RAWHEADERLIST )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->rawHeaderList() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_RAWHEADERLIST FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->rawHeaderList() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setAttribute ( Attribute code, const QVariant & value )
 */
HB_FUNC( QT_QNETWORKREQUEST_SETATTRIBUTE )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      ( p )->setAttribute( ( QNetworkRequest::Attribute ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_SETATTRIBUTE FP=( p )->setAttribute( ( QNetworkRequest::Attribute ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setHeader ( KnownHeaders header, const QVariant & value )
 */
HB_FUNC( QT_QNETWORKREQUEST_SETHEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      ( p )->setHeader( ( QNetworkRequest::KnownHeaders ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_SETHEADER FP=( p )->setHeader( ( QNetworkRequest::KnownHeaders ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setRawHeader ( const QByteArray & headerName, const QByteArray & headerValue )
 */
HB_FUNC( QT_QNETWORKREQUEST_SETRAWHEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      ( p )->setRawHeader( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_SETRAWHEADER FP=( p )->setRawHeader( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setUrl ( const QUrl & url )
 */
HB_FUNC( QT_QNETWORKREQUEST_SETURL )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      ( p )->setUrl( *hbqt_par_QUrl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_SETURL FP=( p )->setUrl( *hbqt_par_QUrl( 2 ) ); p is NULL" ) );
   }
}

/*
 * QUrl url () const
 */
HB_FUNC( QT_QNETWORKREQUEST_URL )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->url() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QNETWORKREQUEST_URL FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->url() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
