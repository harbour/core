/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtnetwork.h"

#if QT_VERSION >= 0x040500

/*
 *  enum Attribute { HttpStatusCodeAttribute, HttpReasonPhraseAttribute, RedirectionTargetAttribute, ConnectionEncryptedAttribute, ..., UserMax }
 *  enum CacheLoadControl { AlwaysNetwork, PreferNetwork, PreferCache, AlwaysCache }
 *  enum KnownHeaders { ContentTypeHeader, ContentLengthHeader, LocationHeader, LastModifiedHeader, CookieHeader, SetCookieHeader }
 */

/*
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
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

typedef struct
{
   QNetworkRequest * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QNetworkRequest;

HBQT_GC_FUNC( hbqt_gcRelease_QNetworkRequest )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QNetworkRequest * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QNetworkRequest( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QNetworkRequest * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QNetworkRequest;
   p->type = HBQT_TYPE_QNetworkRequest;

   return p;
}

HB_FUNC( QT_QNETWORKREQUEST )
{
   QNetworkRequest * pObj = NULL;

   pObj = new QNetworkRequest() ;

   hb_retptrGC( hbqt_gcAllocate_QNetworkRequest( ( void * ) pObj, true ) );
}

/* QVariant attribute ( Attribute code, const QVariant & defaultValue = QVariant() ) const */
HB_FUNC( QT_QNETWORKREQUEST_ATTRIBUTE )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->attribute( ( QNetworkRequest::Attribute ) hb_parni( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) ) ), true ) );
}

/* bool hasRawHeader ( const QByteArray & headerName ) const */
HB_FUNC( QT_QNETWORKREQUEST_HASRAWHEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retl( ( p )->hasRawHeader( *hbqt_par_QByteArray( 2 ) ) );
}

/* QVariant header ( KnownHeaders header ) const */
HB_FUNC( QT_QNETWORKREQUEST_HEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->header( ( QNetworkRequest::KnownHeaders ) hb_parni( 2 ) ) ), true ) );
}

/* QByteArray rawHeader ( const QByteArray & headerName ) const */
HB_FUNC( QT_QNETWORKREQUEST_RAWHEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->rawHeader( *hbqt_par_QByteArray( 2 ) ) ), true ) );
}

/* QList<QByteArray> rawHeaderList () const */
HB_FUNC( QT_QNETWORKREQUEST_RAWHEADERLIST )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->rawHeaderList() ), true ) );
}

/* void setAttribute ( Attribute code, const QVariant & value ) */
HB_FUNC( QT_QNETWORKREQUEST_SETATTRIBUTE )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      ( p )->setAttribute( ( QNetworkRequest::Attribute ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/* void setHeader ( KnownHeaders header, const QVariant & value ) */
HB_FUNC( QT_QNETWORKREQUEST_SETHEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      ( p )->setHeader( ( QNetworkRequest::KnownHeaders ) hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/* void setRawHeader ( const QByteArray & headerName, const QByteArray & headerValue ) */
HB_FUNC( QT_QNETWORKREQUEST_SETRAWHEADER )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      ( p )->setRawHeader( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) );
}

/* void setUrl ( const QUrl & url ) */
HB_FUNC( QT_QNETWORKREQUEST_SETURL )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      ( p )->setUrl( *hbqt_par_QUrl( 2 ) );
}

/* QUrl url () const */
HB_FUNC( QT_QNETWORKREQUEST_URL )
{
   QNetworkRequest * p = hbqt_par_QNetworkRequest( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->url() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
