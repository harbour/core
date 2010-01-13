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
 *  enum FormattingOption { None, RemoveScheme, RemovePassword, RemoveUserInfo, ..., StripTrailingSlash }
 *  flags FormattingOptions
 *  enum ParsingMode { TolerantMode, StrictMode }
 */

/*
 *  Constructed[ 69/75 [ 92.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QByteArray> allEncodedQueryItemValues ( const QByteArray & key ) const
 *  QList<QPair<QByteArray, QByteArray> > encodedQueryItems () const
 *  QList<QPair<QString, QString> > queryItems () const
 *  void setEncodedQueryItems ( const QList<QPair<QByteArray, QByteArray> > & query )
 *  void setQueryItems ( const QList<QPair<QString, QString> > & query )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  //void setQueryDelimiters ( char valueDelimiter, char pairDelimiter )
 */

#include <QtCore/QPointer>

#include <QStringList>
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

QT_G_FUNC( hbqt_gcRelease_QUrl )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QUrl                         p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QUrl                        ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QUrl * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES hbqt_gcRelease_QUrl                        Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QUrl                        Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QUrl( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QUrl;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QUrl                        %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QURL )
{
   void * pObj = NULL;

   pObj = new QUrl( hbqt_par_QString( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QUrl( pObj ) );
}
/*
 * void addEncodedQueryItem ( const QByteArray & key, const QByteArray & value )
 */
HB_FUNC( QT_QURL_ADDENCODEDQUERYITEM )
{
   hbqt_par_QUrl( 1 )->addEncodedQueryItem( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) );
}

/*
 * void addQueryItem ( const QString & key, const QString & value )
 */
HB_FUNC( QT_QURL_ADDQUERYITEM )
{
   hbqt_par_QUrl( 1 )->addQueryItem( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * QStringList allQueryItemValues ( const QString & key ) const
 */
HB_FUNC( QT_QURL_ALLQUERYITEMVALUES )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QUrl( 1 )->allQueryItemValues( hbqt_par_QString( 2 ) ) ) ) );
}

/*
 * QString authority () const
 */
HB_FUNC( QT_QURL_AUTHORITY )
{
   hb_retc( hbqt_par_QUrl( 1 )->authority().toAscii().data() );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QURL_CLEAR )
{
   hbqt_par_QUrl( 1 )->clear();
}

/*
 * QByteArray encodedFragment () const
 */
HB_FUNC( QT_QURL_ENCODEDFRAGMENT )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->encodedFragment() ) ) );
}

/*
 * QByteArray encodedHost () const
 */
HB_FUNC( QT_QURL_ENCODEDHOST )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->encodedHost() ) ) );
}

/*
 * QByteArray encodedPassword () const
 */
HB_FUNC( QT_QURL_ENCODEDPASSWORD )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->encodedPassword() ) ) );
}

/*
 * QByteArray encodedPath () const
 */
HB_FUNC( QT_QURL_ENCODEDPATH )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->encodedPath() ) ) );
}

/*
 * QByteArray encodedQuery () const
 */
HB_FUNC( QT_QURL_ENCODEDQUERY )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->encodedQuery() ) ) );
}

/*
 * QByteArray encodedQueryItemValue ( const QByteArray & key ) const
 */
HB_FUNC( QT_QURL_ENCODEDQUERYITEMVALUE )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->encodedQueryItemValue( *hbqt_par_QByteArray( 2 ) ) ) ) );
}

/*
 * QByteArray encodedUserName () const
 */
HB_FUNC( QT_QURL_ENCODEDUSERNAME )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->encodedUserName() ) ) );
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QURL_ERRORSTRING )
{
   hb_retc( hbqt_par_QUrl( 1 )->errorString().toAscii().data() );
}

/*
 * QString fragment () const
 */
HB_FUNC( QT_QURL_FRAGMENT )
{
   hb_retc( hbqt_par_QUrl( 1 )->fragment().toAscii().data() );
}

/*
 * bool hasEncodedQueryItem ( const QByteArray & key ) const
 */
HB_FUNC( QT_QURL_HASENCODEDQUERYITEM )
{
   hb_retl( hbqt_par_QUrl( 1 )->hasEncodedQueryItem( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * bool hasFragment () const
 */
HB_FUNC( QT_QURL_HASFRAGMENT )
{
   hb_retl( hbqt_par_QUrl( 1 )->hasFragment() );
}

/*
 * bool hasQuery () const
 */
HB_FUNC( QT_QURL_HASQUERY )
{
   hb_retl( hbqt_par_QUrl( 1 )->hasQuery() );
}

/*
 * bool hasQueryItem ( const QString & key ) const
 */
HB_FUNC( QT_QURL_HASQUERYITEM )
{
   hb_retl( hbqt_par_QUrl( 1 )->hasQueryItem( hbqt_par_QString( 2 ) ) );
}

/*
 * QString host () const
 */
HB_FUNC( QT_QURL_HOST )
{
   hb_retc( hbqt_par_QUrl( 1 )->host().toAscii().data() );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QURL_ISEMPTY )
{
   hb_retl( hbqt_par_QUrl( 1 )->isEmpty() );
}

/*
 * bool isParentOf ( const QUrl & childUrl ) const
 */
HB_FUNC( QT_QURL_ISPARENTOF )
{
   hb_retl( hbqt_par_QUrl( 1 )->isParentOf( *hbqt_par_QUrl( 2 ) ) );
}

/*
 * bool isRelative () const
 */
HB_FUNC( QT_QURL_ISRELATIVE )
{
   hb_retl( hbqt_par_QUrl( 1 )->isRelative() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QURL_ISVALID )
{
   hb_retl( hbqt_par_QUrl( 1 )->isValid() );
}

/*
 * QString password () const
 */
HB_FUNC( QT_QURL_PASSWORD )
{
   hb_retc( hbqt_par_QUrl( 1 )->password().toAscii().data() );
}

/*
 * QString path () const
 */
HB_FUNC( QT_QURL_PATH )
{
   hb_retc( hbqt_par_QUrl( 1 )->path().toAscii().data() );
}

/*
 * int port () const
 */
HB_FUNC( QT_QURL_PORT )
{
   hb_retni( hbqt_par_QUrl( 1 )->port() );
}

/*
 * int port ( int defaultPort ) const
 */
HB_FUNC( QT_QURL_PORT_1 )
{
   hb_retni( hbqt_par_QUrl( 1 )->port( hb_parni( 2 ) ) );
}

/*
 * QString queryItemValue ( const QString & key ) const
 */
HB_FUNC( QT_QURL_QUERYITEMVALUE )
{
   hb_retc( hbqt_par_QUrl( 1 )->queryItemValue( hbqt_par_QString( 2 ) ).toAscii().data() );
}

/*
 * char queryPairDelimiter () const
 */
HB_FUNC( QT_QURL_QUERYPAIRDELIMITER )
{
   hb_retni( hbqt_par_QUrl( 1 )->queryPairDelimiter() );
}

/*
 * char queryValueDelimiter () const
 */
HB_FUNC( QT_QURL_QUERYVALUEDELIMITER )
{
   hb_retni( hbqt_par_QUrl( 1 )->queryValueDelimiter() );
}

/*
 * void removeAllEncodedQueryItems ( const QByteArray & key )
 */
HB_FUNC( QT_QURL_REMOVEALLENCODEDQUERYITEMS )
{
   hbqt_par_QUrl( 1 )->removeAllEncodedQueryItems( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void removeAllQueryItems ( const QString & key )
 */
HB_FUNC( QT_QURL_REMOVEALLQUERYITEMS )
{
   hbqt_par_QUrl( 1 )->removeAllQueryItems( hbqt_par_QString( 2 ) );
}

/*
 * void removeEncodedQueryItem ( const QByteArray & key )
 */
HB_FUNC( QT_QURL_REMOVEENCODEDQUERYITEM )
{
   hbqt_par_QUrl( 1 )->removeEncodedQueryItem( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void removeQueryItem ( const QString & key )
 */
HB_FUNC( QT_QURL_REMOVEQUERYITEM )
{
   hbqt_par_QUrl( 1 )->removeQueryItem( hbqt_par_QString( 2 ) );
}

/*
 * QUrl resolved ( const QUrl & relative ) const
 */
HB_FUNC( QT_QURL_RESOLVED )
{
   hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( hbqt_par_QUrl( 1 )->resolved( *hbqt_par_QUrl( 2 ) ) ) ) );
}

/*
 * QString scheme () const
 */
HB_FUNC( QT_QURL_SCHEME )
{
   hb_retc( hbqt_par_QUrl( 1 )->scheme().toAscii().data() );
}

/*
 * void setAuthority ( const QString & authority )
 */
HB_FUNC( QT_QURL_SETAUTHORITY )
{
   hbqt_par_QUrl( 1 )->setAuthority( hbqt_par_QString( 2 ) );
}

/*
 * void setEncodedFragment ( const QByteArray & fragment )
 */
HB_FUNC( QT_QURL_SETENCODEDFRAGMENT )
{
   hbqt_par_QUrl( 1 )->setEncodedFragment( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setEncodedHost ( const QByteArray & host )
 */
HB_FUNC( QT_QURL_SETENCODEDHOST )
{
   hbqt_par_QUrl( 1 )->setEncodedHost( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setEncodedPassword ( const QByteArray & password )
 */
HB_FUNC( QT_QURL_SETENCODEDPASSWORD )
{
   hbqt_par_QUrl( 1 )->setEncodedPassword( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setEncodedPath ( const QByteArray & path )
 */
HB_FUNC( QT_QURL_SETENCODEDPATH )
{
   hbqt_par_QUrl( 1 )->setEncodedPath( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setEncodedQuery ( const QByteArray & query )
 */
HB_FUNC( QT_QURL_SETENCODEDQUERY )
{
   hbqt_par_QUrl( 1 )->setEncodedQuery( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setEncodedUrl ( const QByteArray & encodedUrl )
 */
HB_FUNC( QT_QURL_SETENCODEDURL )
{
   hbqt_par_QUrl( 1 )->setEncodedUrl( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setEncodedUrl ( const QByteArray & encodedUrl, ParsingMode parsingMode )
 */
HB_FUNC( QT_QURL_SETENCODEDURL_1 )
{
   hbqt_par_QUrl( 1 )->setEncodedUrl( *hbqt_par_QByteArray( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) );
}

/*
 * void setEncodedUserName ( const QByteArray & userName )
 */
HB_FUNC( QT_QURL_SETENCODEDUSERNAME )
{
   hbqt_par_QUrl( 1 )->setEncodedUserName( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void setFragment ( const QString & fragment )
 */
HB_FUNC( QT_QURL_SETFRAGMENT )
{
   hbqt_par_QUrl( 1 )->setFragment( hbqt_par_QString( 2 ) );
}

/*
 * void setHost ( const QString & host )
 */
HB_FUNC( QT_QURL_SETHOST )
{
   hbqt_par_QUrl( 1 )->setHost( hbqt_par_QString( 2 ) );
}

/*
 * void setPassword ( const QString & password )
 */
HB_FUNC( QT_QURL_SETPASSWORD )
{
   hbqt_par_QUrl( 1 )->setPassword( hbqt_par_QString( 2 ) );
}

/*
 * void setPath ( const QString & path )
 */
HB_FUNC( QT_QURL_SETPATH )
{
   hbqt_par_QUrl( 1 )->setPath( hbqt_par_QString( 2 ) );
}

/*
 * void setPort ( int port )
 */
HB_FUNC( QT_QURL_SETPORT )
{
   hbqt_par_QUrl( 1 )->setPort( hb_parni( 2 ) );
}

/*
 * void setScheme ( const QString & scheme )
 */
HB_FUNC( QT_QURL_SETSCHEME )
{
   hbqt_par_QUrl( 1 )->setScheme( hbqt_par_QString( 2 ) );
}

/*
 * void setUrl ( const QString & url )
 */
HB_FUNC( QT_QURL_SETURL )
{
   hbqt_par_QUrl( 1 )->setUrl( hbqt_par_QString( 2 ) );
}

/*
 * void setUrl ( const QString & url, ParsingMode parsingMode )
 */
HB_FUNC( QT_QURL_SETURL_1 )
{
   hbqt_par_QUrl( 1 )->setUrl( hbqt_par_QString( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) );
}

/*
 * void setUserInfo ( const QString & userInfo )
 */
HB_FUNC( QT_QURL_SETUSERINFO )
{
   hbqt_par_QUrl( 1 )->setUserInfo( hbqt_par_QString( 2 ) );
}

/*
 * void setUserName ( const QString & userName )
 */
HB_FUNC( QT_QURL_SETUSERNAME )
{
   hbqt_par_QUrl( 1 )->setUserName( hbqt_par_QString( 2 ) );
}

/*
 * QByteArray toEncoded ( FormattingOptions options = None ) const
 */
HB_FUNC( QT_QURL_TOENCODED )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->toEncoded( ( HB_ISNUM( 2 ) ? ( QUrl::FormattingOptions ) hb_parni( 2 ) : ( QUrl::FormattingOptions ) QUrl::None ) ) ) ) );
}

/*
 * QString toLocalFile () const
 */
HB_FUNC( QT_QURL_TOLOCALFILE )
{
   hb_retc( hbqt_par_QUrl( 1 )->toLocalFile().toAscii().data() );
}

/*
 * QString toString ( FormattingOptions options = None ) const
 */
HB_FUNC( QT_QURL_TOSTRING )
{
   hb_retc( hbqt_par_QUrl( 1 )->toString( ( HB_ISNUM( 2 ) ? ( QUrl::FormattingOptions ) hb_parni( 2 ) : ( QUrl::FormattingOptions ) QUrl::None ) ).toAscii().data() );
}

/*
 * QString userInfo () const
 */
HB_FUNC( QT_QURL_USERINFO )
{
   hb_retc( hbqt_par_QUrl( 1 )->userInfo().toAscii().data() );
}

/*
 * QString userName () const
 */
HB_FUNC( QT_QURL_USERNAME )
{
   hb_retc( hbqt_par_QUrl( 1 )->userName().toAscii().data() );
}

/*
 * QString fromAce ( const QByteArray & domain )
 */
HB_FUNC( QT_QURL_FROMACE )
{
   hb_retc( hbqt_par_QUrl( 1 )->fromAce( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
}

/*
 * QUrl fromEncoded ( const QByteArray & input )
 */
HB_FUNC( QT_QURL_FROMENCODED )
{
   hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( hbqt_par_QUrl( 1 )->fromEncoded( *hbqt_par_QByteArray( 2 ) ) ) ) );
}

/*
 * QUrl fromEncoded ( const QByteArray & input, ParsingMode parsingMode )
 */
HB_FUNC( QT_QURL_FROMENCODED_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( hbqt_par_QUrl( 1 )->fromEncoded( *hbqt_par_QByteArray( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) ) ) ) );
}

/*
 * QUrl fromLocalFile ( const QString & localFile )
 */
HB_FUNC( QT_QURL_FROMLOCALFILE )
{
   hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( hbqt_par_QUrl( 1 )->fromLocalFile( hbqt_par_QString( 2 ) ) ) ) );
}

/*
 * QString fromPercentEncoding ( const QByteArray & input )
 */
HB_FUNC( QT_QURL_FROMPERCENTENCODING )
{
   hb_retc( hbqt_par_QUrl( 1 )->fromPercentEncoding( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
}

/*
 * QStringList idnWhitelist ()
 */
HB_FUNC( QT_QURL_IDNWHITELIST )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QUrl( 1 )->idnWhitelist() ) ) );
}

/*
 * void setIdnWhitelist ( const QStringList & list )
 */
HB_FUNC( QT_QURL_SETIDNWHITELIST )
{
   hbqt_par_QUrl( 1 )->setIdnWhitelist( *hbqt_par_QStringList( 2 ) );
}

/*
 * QByteArray toAce ( const QString & domain )
 */
HB_FUNC( QT_QURL_TOACE )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->toAce( hbqt_par_QString( 2 ) ) ) ) );
}

/*
 * QByteArray toPercentEncoding ( const QString & input, const QByteArray & exclude = QByteArray(), const QByteArray & include = QByteArray() )
 */
HB_FUNC( QT_QURL_TOPERCENTENCODING )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QUrl( 1 )->toPercentEncoding( hbqt_par_QString( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QByteArray( 3 ) : QByteArray() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QByteArray( 4 ) : QByteArray() ) ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
