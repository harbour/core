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

#if QT_VERSION >= 0x040500

/*
 *  enum FormattingOption { None, RemoveScheme, RemovePassword, RemoveUserInfo, ..., StripTrailingSlash }
 *  flags FormattingOptions
 *  enum ParsingMode { TolerantMode, StrictMode }
 */

/*
 *  Constructed[ 70/74 [ 94.59% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QList<QPair<QByteArray, QByteArray> > encodedQueryItems () const
 *  QList<QPair<QString, QString> > queryItems () const
 *  void setEncodedQueryItems ( const QList<QPair<QByteArray, QByteArray> > & query )
 *  void setQueryItems ( const QList<QPair<QString, QString> > & query )
 *
 *  *** Commented out protostypes ***
 *
 *  //void setQueryDelimiters ( char valueDelimiter, char pairDelimiter )
 */

#include <QtCore/QPointer>

#include <QtCore/QStringList>
#include <QtCore/QUrl>


/*
 * QUrl ()
 * QUrl ( const QString & url )
 * QUrl ( const QUrl & other )
 * QUrl ( const QString & url, ParsingMode parsingMode )
 * ~QUrl ()
 */
/*
 * void setQueryDelimiters ( char valueDelimiter, char pairDelimiter )
 */
HB_FUNC( QT_QURL_SETQUERYDELIMITERS )
{
   hbqt_par_QUrl( 1 )->setQueryDelimiters( ( char ) hb_parni( 2 ), ( char ) hb_parni( 3 ) );
}

typedef struct
{
   QUrl * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QUrl;

HBQT_GC_FUNC( hbqt_gcRelease_QUrl )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QUrl * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QUrl( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QUrl * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QUrl;
   p->type = HBQT_TYPE_QUrl;

   return p;
}

HB_FUNC( QT_QURL )
{
   QUrl * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QUrl( hbqt_par_QString( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QUrl( *hbqt_par_QUrl( 1 ) ) ;
   }
   else
   {
      pObj = new QUrl() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QUrl( ( void * ) pObj, true ) );
}

/* void addEncodedQueryItem ( const QByteArray & key, const QByteArray & value ) */
HB_FUNC( QT_QURL_ADDENCODEDQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->addEncodedQueryItem( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) );
}

/* void addQueryItem ( const QString & key, const QString & value ) */
HB_FUNC( QT_QURL_ADDQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->addQueryItem( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QList<QByteArray> allEncodedQueryItemValues ( const QByteArray & key ) const */
HB_FUNC( QT_QURL_ALLENCODEDQUERYITEMVALUES )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->allEncodedQueryItemValues( *hbqt_par_QByteArray( 2 ) ) ), true ) );
}

/* QStringList allQueryItemValues ( const QString & key ) const */
HB_FUNC( QT_QURL_ALLQUERYITEMVALUES )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->allQueryItemValues( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QString authority () const */
HB_FUNC( QT_QURL_AUTHORITY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->authority().toUtf8().data() );
}

/* void clear () */
HB_FUNC( QT_QURL_CLEAR )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->clear();
}

/* QByteArray encodedFragment () const */
HB_FUNC( QT_QURL_ENCODEDFRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedFragment() ), true ) );
}

/* QByteArray encodedHost () const */
HB_FUNC( QT_QURL_ENCODEDHOST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedHost() ), true ) );
}

/* QByteArray encodedPassword () const */
HB_FUNC( QT_QURL_ENCODEDPASSWORD )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedPassword() ), true ) );
}

/* QByteArray encodedPath () const */
HB_FUNC( QT_QURL_ENCODEDPATH )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedPath() ), true ) );
}

/* QByteArray encodedQuery () const */
HB_FUNC( QT_QURL_ENCODEDQUERY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedQuery() ), true ) );
}

/* QByteArray encodedQueryItemValue ( const QByteArray & key ) const */
HB_FUNC( QT_QURL_ENCODEDQUERYITEMVALUE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedQueryItemValue( *hbqt_par_QByteArray( 2 ) ) ), true ) );
}

/* QByteArray encodedUserName () const */
HB_FUNC( QT_QURL_ENCODEDUSERNAME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedUserName() ), true ) );
}

/* QString errorString () const */
HB_FUNC( QT_QURL_ERRORSTRING )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
}

/* QString fragment () const */
HB_FUNC( QT_QURL_FRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fragment().toUtf8().data() );
}

/* bool hasEncodedQueryItem ( const QByteArray & key ) const */
HB_FUNC( QT_QURL_HASENCODEDQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->hasEncodedQueryItem( *hbqt_par_QByteArray( 2 ) ) );
}

/* bool hasFragment () const */
HB_FUNC( QT_QURL_HASFRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->hasFragment() );
}

/* bool hasQuery () const */
HB_FUNC( QT_QURL_HASQUERY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->hasQuery() );
}

/* bool hasQueryItem ( const QString & key ) const */
HB_FUNC( QT_QURL_HASQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->hasQueryItem( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* QString host () const */
HB_FUNC( QT_QURL_HOST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->host().toUtf8().data() );
}

/* bool isEmpty () const */
HB_FUNC( QT_QURL_ISEMPTY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* bool isParentOf ( const QUrl & childUrl ) const */
HB_FUNC( QT_QURL_ISPARENTOF )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->isParentOf( *hbqt_par_QUrl( 2 ) ) );
}

/* bool isRelative () const */
HB_FUNC( QT_QURL_ISRELATIVE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->isRelative() );
}

/* bool isValid () const */
HB_FUNC( QT_QURL_ISVALID )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* QString password () const */
HB_FUNC( QT_QURL_PASSWORD )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->password().toUtf8().data() );
}

/* QString path () const */
HB_FUNC( QT_QURL_PATH )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->path().toUtf8().data() );
}

/* int port () const */
HB_FUNC( QT_QURL_PORT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retni( ( p )->port() );
}

/* int port ( int defaultPort ) const */
HB_FUNC( QT_QURL_PORT_1 )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retni( ( p )->port( hb_parni( 2 ) ) );
}

/* QString queryItemValue ( const QString & key ) const */
HB_FUNC( QT_QURL_QUERYITEMVALUE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->queryItemValue( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/* char queryPairDelimiter () const */
HB_FUNC( QT_QURL_QUERYPAIRDELIMITER )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retni( ( p )->queryPairDelimiter() );
}

/* char queryValueDelimiter () const */
HB_FUNC( QT_QURL_QUERYVALUEDELIMITER )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retni( ( p )->queryValueDelimiter() );
}

/* void removeAllEncodedQueryItems ( const QByteArray & key ) */
HB_FUNC( QT_QURL_REMOVEALLENCODEDQUERYITEMS )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->removeAllEncodedQueryItems( *hbqt_par_QByteArray( 2 ) );
}

/* void removeAllQueryItems ( const QString & key ) */
HB_FUNC( QT_QURL_REMOVEALLQUERYITEMS )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeAllQueryItems( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void removeEncodedQueryItem ( const QByteArray & key ) */
HB_FUNC( QT_QURL_REMOVEENCODEDQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->removeEncodedQueryItem( *hbqt_par_QByteArray( 2 ) );
}

/* void removeQueryItem ( const QString & key ) */
HB_FUNC( QT_QURL_REMOVEQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeQueryItem( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QUrl resolved ( const QUrl & relative ) const */
HB_FUNC( QT_QURL_RESOLVED )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->resolved( *hbqt_par_QUrl( 2 ) ) ), true ) );
}

/* QString scheme () const */
HB_FUNC( QT_QURL_SCHEME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->scheme().toUtf8().data() );
}

/* void setAuthority ( const QString & authority ) */
HB_FUNC( QT_QURL_SETAUTHORITY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setAuthority( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setEncodedFragment ( const QByteArray & fragment ) */
HB_FUNC( QT_QURL_SETENCODEDFRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedFragment( *hbqt_par_QByteArray( 2 ) );
}

/* void setEncodedHost ( const QByteArray & host ) */
HB_FUNC( QT_QURL_SETENCODEDHOST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedHost( *hbqt_par_QByteArray( 2 ) );
}

/* void setEncodedPassword ( const QByteArray & password ) */
HB_FUNC( QT_QURL_SETENCODEDPASSWORD )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedPassword( *hbqt_par_QByteArray( 2 ) );
}

/* void setEncodedPath ( const QByteArray & path ) */
HB_FUNC( QT_QURL_SETENCODEDPATH )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedPath( *hbqt_par_QByteArray( 2 ) );
}

/* void setEncodedQuery ( const QByteArray & query ) */
HB_FUNC( QT_QURL_SETENCODEDQUERY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedQuery( *hbqt_par_QByteArray( 2 ) );
}

/* void setEncodedUrl ( const QByteArray & encodedUrl ) */
HB_FUNC( QT_QURL_SETENCODEDURL )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedUrl( *hbqt_par_QByteArray( 2 ) );
}

/* void setEncodedUrl ( const QByteArray & encodedUrl, ParsingMode parsingMode ) */
HB_FUNC( QT_QURL_SETENCODEDURL_1 )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedUrl( *hbqt_par_QByteArray( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) );
}

/* void setEncodedUserName ( const QByteArray & userName ) */
HB_FUNC( QT_QURL_SETENCODEDUSERNAME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedUserName( *hbqt_par_QByteArray( 2 ) );
}

/* void setFragment ( const QString & fragment ) */
HB_FUNC( QT_QURL_SETFRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFragment( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setHost ( const QString & host ) */
HB_FUNC( QT_QURL_SETHOST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setHost( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setPassword ( const QString & password ) */
HB_FUNC( QT_QURL_SETPASSWORD )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPassword( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setPath ( const QString & path ) */
HB_FUNC( QT_QURL_SETPATH )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setPort ( int port ) */
HB_FUNC( QT_QURL_SETPORT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setPort( hb_parni( 2 ) );
}

/* void setScheme ( const QString & scheme ) */
HB_FUNC( QT_QURL_SETSCHEME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setScheme( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setUrl ( const QString & url ) */
HB_FUNC( QT_QURL_SETURL )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setUrl( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setUrl ( const QString & url, ParsingMode parsingMode ) */
HB_FUNC( QT_QURL_SETURL_1 )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setUrl( hb_parstr_utf8( 2, &pText, NULL ), ( QUrl::ParsingMode ) hb_parni( 3 ) );
      hb_strfree( pText );
   }
}

/* void setUserInfo ( const QString & userInfo ) */
HB_FUNC( QT_QURL_SETUSERINFO )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setUserInfo( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setUserName ( const QString & userName ) */
HB_FUNC( QT_QURL_SETUSERNAME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      ( p )->setUserName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QByteArray toEncoded ( FormattingOptions options = None ) const */
HB_FUNC( QT_QURL_TOENCODED )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toEncoded( ( HB_ISNUM( 2 ) ? ( QUrl::FormattingOptions ) hb_parni( 2 ) : ( QUrl::FormattingOptions ) QUrl::None ) ) ), true ) );
}

/* QString toLocalFile () const */
HB_FUNC( QT_QURL_TOLOCALFILE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toLocalFile().toUtf8().data() );
}

/* QString toString ( FormattingOptions options = None ) const */
HB_FUNC( QT_QURL_TOSTRING )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toString( ( HB_ISNUM( 2 ) ? ( QUrl::FormattingOptions ) hb_parni( 2 ) : ( QUrl::FormattingOptions ) QUrl::None ) ).toUtf8().data() );
}

/* QString userInfo () const */
HB_FUNC( QT_QURL_USERINFO )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->userInfo().toUtf8().data() );
}

/* QString userName () const */
HB_FUNC( QT_QURL_USERNAME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->userName().toUtf8().data() );
}

/* QString fromAce ( const QByteArray & domain ) */
HB_FUNC( QT_QURL_FROMACE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fromAce( *hbqt_par_QByteArray( 2 ) ).toUtf8().data() );
}

/* QUrl fromEncoded ( const QByteArray & input ) */
HB_FUNC( QT_QURL_FROMENCODED )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromEncoded( *hbqt_par_QByteArray( 2 ) ) ), true ) );
}

/* QUrl fromEncoded ( const QByteArray & input, ParsingMode parsingMode ) */
HB_FUNC( QT_QURL_FROMENCODED_1 )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromEncoded( *hbqt_par_QByteArray( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) ) ), true ) );
}

/* QUrl fromLocalFile ( const QString & localFile ) */
HB_FUNC( QT_QURL_FROMLOCALFILE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromLocalFile( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QString fromPercentEncoding ( const QByteArray & input ) */
HB_FUNC( QT_QURL_FROMPERCENTENCODING )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fromPercentEncoding( *hbqt_par_QByteArray( 2 ) ).toUtf8().data() );
}

/* QStringList idnWhitelist () */
HB_FUNC( QT_QURL_IDNWHITELIST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->idnWhitelist() ), true ) );
}

/* void setIdnWhitelist ( const QStringList & list ) */
HB_FUNC( QT_QURL_SETIDNWHITELIST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setIdnWhitelist( *hbqt_par_QStringList( 2 ) );
}

/* QByteArray toAce ( const QString & domain ) */
HB_FUNC( QT_QURL_TOACE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toAce( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QByteArray toPercentEncoding ( const QString & input, const QByteArray & exclude = QByteArray(), const QByteArray & include = QByteArray() ) */
HB_FUNC( QT_QURL_TOPERCENTENCODING )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toPercentEncoding( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QByteArray( 3 ) : QByteArray() ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QByteArray( 4 ) : QByteArray() ) ) ), true ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
