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

#include "../hbqt.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum FormattingOption { None, RemoveScheme, RemovePassword, RemoveUserInfo, ..., StripTrailingSlash }
 *  flags FormattingOptions
 *  enum ParsingMode { TolerantMode, StrictMode }
 */

/*
 *  Constructed[ 70/75 [ 93.33% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QUrl;

QT_G_FUNC( hbqt_gcRelease_QUrl )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QUrl   /.\\", p->ph ) );
         delete ( ( QUrl * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QUrl   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QUrl    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QUrl    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QUrl( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QUrl * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QUrl;
   p->type = HBQT_TYPE_QUrl;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QUrl", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QUrl", pObj ) );
   }
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

/*
 * void addEncodedQueryItem ( const QByteArray & key, const QByteArray & value )
 */
HB_FUNC( QT_QURL_ADDENCODEDQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->addEncodedQueryItem( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ADDENCODEDQUERYITEM FP=( p )->addEncodedQueryItem( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) ); p is NULL" ) );
   }
}

/*
 * void addQueryItem ( const QString & key, const QString & value )
 */
HB_FUNC( QT_QURL_ADDQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->addQueryItem( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ADDQUERYITEM FP=( p )->addQueryItem( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * QList<QByteArray> allEncodedQueryItemValues ( const QByteArray & key ) const
 */
HB_FUNC( QT_QURL_ALLENCODEDQUERYITEMVALUES )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->allEncodedQueryItemValues( *hbqt_par_QByteArray( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ALLENCODEDQUERYITEMVALUES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->allEncodedQueryItemValues( *hbqt_par_QByteArray( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QStringList allQueryItemValues ( const QString & key ) const
 */
HB_FUNC( QT_QURL_ALLQUERYITEMVALUES )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->allQueryItemValues( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ALLQUERYITEMVALUES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->allQueryItemValues( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString authority () const
 */
HB_FUNC( QT_QURL_AUTHORITY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->authority().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_AUTHORITY FP=hb_retc( ( p )->authority().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QURL_CLEAR )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * QByteArray encodedFragment () const
 */
HB_FUNC( QT_QURL_ENCODEDFRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedFragment() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ENCODEDFRAGMENT FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedFragment() ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray encodedHost () const
 */
HB_FUNC( QT_QURL_ENCODEDHOST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedHost() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ENCODEDHOST FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedHost() ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray encodedPassword () const
 */
HB_FUNC( QT_QURL_ENCODEDPASSWORD )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedPassword() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ENCODEDPASSWORD FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedPassword() ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray encodedPath () const
 */
HB_FUNC( QT_QURL_ENCODEDPATH )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedPath() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ENCODEDPATH FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedPath() ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray encodedQuery () const
 */
HB_FUNC( QT_QURL_ENCODEDQUERY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedQuery() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ENCODEDQUERY FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedQuery() ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray encodedQueryItemValue ( const QByteArray & key ) const
 */
HB_FUNC( QT_QURL_ENCODEDQUERYITEMVALUE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedQueryItemValue( *hbqt_par_QByteArray( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ENCODEDQUERYITEMVALUE FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedQueryItemValue( *hbqt_par_QByteArray( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray encodedUserName () const
 */
HB_FUNC( QT_QURL_ENCODEDUSERNAME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedUserName() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ENCODEDUSERNAME FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->encodedUserName() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QURL_ERRORSTRING )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->errorString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ERRORSTRING FP=hb_retc( ( p )->errorString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString fragment () const
 */
HB_FUNC( QT_QURL_FRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->fragment().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_FRAGMENT FP=hb_retc( ( p )->fragment().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool hasEncodedQueryItem ( const QByteArray & key ) const
 */
HB_FUNC( QT_QURL_HASENCODEDQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->hasEncodedQueryItem( *hbqt_par_QByteArray( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_HASENCODEDQUERYITEM FP=hb_retl( ( p )->hasEncodedQueryItem( *hbqt_par_QByteArray( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool hasFragment () const
 */
HB_FUNC( QT_QURL_HASFRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->hasFragment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_HASFRAGMENT FP=hb_retl( ( p )->hasFragment() ); p is NULL" ) );
   }
}

/*
 * bool hasQuery () const
 */
HB_FUNC( QT_QURL_HASQUERY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->hasQuery() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_HASQUERY FP=hb_retl( ( p )->hasQuery() ); p is NULL" ) );
   }
}

/*
 * bool hasQueryItem ( const QString & key ) const
 */
HB_FUNC( QT_QURL_HASQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->hasQueryItem( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_HASQUERYITEM FP=hb_retl( ( p )->hasQueryItem( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString host () const
 */
HB_FUNC( QT_QURL_HOST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->host().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_HOST FP=hb_retc( ( p )->host().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QURL_ISEMPTY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ISEMPTY FP=hb_retl( ( p )->isEmpty() ); p is NULL" ) );
   }
}

/*
 * bool isParentOf ( const QUrl & childUrl ) const
 */
HB_FUNC( QT_QURL_ISPARENTOF )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->isParentOf( *hbqt_par_QUrl( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ISPARENTOF FP=hb_retl( ( p )->isParentOf( *hbqt_par_QUrl( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isRelative () const
 */
HB_FUNC( QT_QURL_ISRELATIVE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->isRelative() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ISRELATIVE FP=hb_retl( ( p )->isRelative() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QURL_ISVALID )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * QString password () const
 */
HB_FUNC( QT_QURL_PASSWORD )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->password().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_PASSWORD FP=hb_retc( ( p )->password().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString path () const
 */
HB_FUNC( QT_QURL_PATH )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->path().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_PATH FP=hb_retc( ( p )->path().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int port () const
 */
HB_FUNC( QT_QURL_PORT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retni( ( p )->port() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_PORT FP=hb_retni( ( p )->port() ); p is NULL" ) );
   }
}

/*
 * int port ( int defaultPort ) const
 */
HB_FUNC( QT_QURL_PORT_1 )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retni( ( p )->port( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_PORT_1 FP=hb_retni( ( p )->port( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString queryItemValue ( const QString & key ) const
 */
HB_FUNC( QT_QURL_QUERYITEMVALUE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->queryItemValue( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_QUERYITEMVALUE FP=hb_retc( ( p )->queryItemValue( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * char queryPairDelimiter () const
 */
HB_FUNC( QT_QURL_QUERYPAIRDELIMITER )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retni( ( p )->queryPairDelimiter() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_QUERYPAIRDELIMITER FP=hb_retni( ( p )->queryPairDelimiter() ); p is NULL" ) );
   }
}

/*
 * char queryValueDelimiter () const
 */
HB_FUNC( QT_QURL_QUERYVALUEDELIMITER )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retni( ( p )->queryValueDelimiter() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_QUERYVALUEDELIMITER FP=hb_retni( ( p )->queryValueDelimiter() ); p is NULL" ) );
   }
}

/*
 * void removeAllEncodedQueryItems ( const QByteArray & key )
 */
HB_FUNC( QT_QURL_REMOVEALLENCODEDQUERYITEMS )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->removeAllEncodedQueryItems( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_REMOVEALLENCODEDQUERYITEMS FP=( p )->removeAllEncodedQueryItems( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeAllQueryItems ( const QString & key )
 */
HB_FUNC( QT_QURL_REMOVEALLQUERYITEMS )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->removeAllQueryItems( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_REMOVEALLQUERYITEMS FP=( p )->removeAllQueryItems( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeEncodedQueryItem ( const QByteArray & key )
 */
HB_FUNC( QT_QURL_REMOVEENCODEDQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->removeEncodedQueryItem( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_REMOVEENCODEDQUERYITEM FP=( p )->removeEncodedQueryItem( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeQueryItem ( const QString & key )
 */
HB_FUNC( QT_QURL_REMOVEQUERYITEM )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->removeQueryItem( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_REMOVEQUERYITEM FP=( p )->removeQueryItem( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QUrl resolved ( const QUrl & relative ) const
 */
HB_FUNC( QT_QURL_RESOLVED )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->resolved( *hbqt_par_QUrl( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_RESOLVED FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->resolved( *hbqt_par_QUrl( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString scheme () const
 */
HB_FUNC( QT_QURL_SCHEME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->scheme().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SCHEME FP=hb_retc( ( p )->scheme().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setAuthority ( const QString & authority )
 */
HB_FUNC( QT_QURL_SETAUTHORITY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setAuthority( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETAUTHORITY FP=( p )->setAuthority( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEncodedFragment ( const QByteArray & fragment )
 */
HB_FUNC( QT_QURL_SETENCODEDFRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedFragment( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETENCODEDFRAGMENT FP=( p )->setEncodedFragment( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEncodedHost ( const QByteArray & host )
 */
HB_FUNC( QT_QURL_SETENCODEDHOST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedHost( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETENCODEDHOST FP=( p )->setEncodedHost( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEncodedPassword ( const QByteArray & password )
 */
HB_FUNC( QT_QURL_SETENCODEDPASSWORD )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedPassword( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETENCODEDPASSWORD FP=( p )->setEncodedPassword( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEncodedPath ( const QByteArray & path )
 */
HB_FUNC( QT_QURL_SETENCODEDPATH )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedPath( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETENCODEDPATH FP=( p )->setEncodedPath( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEncodedQuery ( const QByteArray & query )
 */
HB_FUNC( QT_QURL_SETENCODEDQUERY )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedQuery( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETENCODEDQUERY FP=( p )->setEncodedQuery( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEncodedUrl ( const QByteArray & encodedUrl )
 */
HB_FUNC( QT_QURL_SETENCODEDURL )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedUrl( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETENCODEDURL FP=( p )->setEncodedUrl( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEncodedUrl ( const QByteArray & encodedUrl, ParsingMode parsingMode )
 */
HB_FUNC( QT_QURL_SETENCODEDURL_1 )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedUrl( *hbqt_par_QByteArray( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETENCODEDURL_1 FP=( p )->setEncodedUrl( *hbqt_par_QByteArray( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setEncodedUserName ( const QByteArray & userName )
 */
HB_FUNC( QT_QURL_SETENCODEDUSERNAME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setEncodedUserName( *hbqt_par_QByteArray( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETENCODEDUSERNAME FP=( p )->setEncodedUserName( *hbqt_par_QByteArray( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFragment ( const QString & fragment )
 */
HB_FUNC( QT_QURL_SETFRAGMENT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setFragment( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETFRAGMENT FP=( p )->setFragment( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHost ( const QString & host )
 */
HB_FUNC( QT_QURL_SETHOST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setHost( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETHOST FP=( p )->setHost( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPassword ( const QString & password )
 */
HB_FUNC( QT_QURL_SETPASSWORD )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setPassword( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETPASSWORD FP=( p )->setPassword( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPath ( const QString & path )
 */
HB_FUNC( QT_QURL_SETPATH )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setPath( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETPATH FP=( p )->setPath( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPort ( int port )
 */
HB_FUNC( QT_QURL_SETPORT )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setPort( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETPORT FP=( p )->setPort( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setScheme ( const QString & scheme )
 */
HB_FUNC( QT_QURL_SETSCHEME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setScheme( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETSCHEME FP=( p )->setScheme( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUrl ( const QString & url )
 */
HB_FUNC( QT_QURL_SETURL )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setUrl( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETURL FP=( p )->setUrl( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUrl ( const QString & url, ParsingMode parsingMode )
 */
HB_FUNC( QT_QURL_SETURL_1 )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setUrl( hbqt_par_QString( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETURL_1 FP=( p )->setUrl( hbqt_par_QString( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setUserInfo ( const QString & userInfo )
 */
HB_FUNC( QT_QURL_SETUSERINFO )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setUserInfo( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETUSERINFO FP=( p )->setUserInfo( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUserName ( const QString & userName )
 */
HB_FUNC( QT_QURL_SETUSERNAME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setUserName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETUSERNAME FP=( p )->setUserName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QByteArray toEncoded ( FormattingOptions options = None ) const
 */
HB_FUNC( QT_QURL_TOENCODED )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toEncoded( ( HB_ISNUM( 2 ) ? ( QUrl::FormattingOptions ) hb_parni( 2 ) : ( QUrl::FormattingOptions ) QUrl::None ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_TOENCODED FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toEncoded( ( HB_ISNUM( 2 ) ? ( QUrl::FormattingOptions ) hb_parni( 2 ) : ( QUrl::FormattingOptions ) QUrl::None ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString toLocalFile () const
 */
HB_FUNC( QT_QURL_TOLOCALFILE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->toLocalFile().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_TOLOCALFILE FP=hb_retc( ( p )->toLocalFile().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toString ( FormattingOptions options = None ) const
 */
HB_FUNC( QT_QURL_TOSTRING )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->toString( ( HB_ISNUM( 2 ) ? ( QUrl::FormattingOptions ) hb_parni( 2 ) : ( QUrl::FormattingOptions ) QUrl::None ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_TOSTRING FP=hb_retc( ( p )->toString( ( HB_ISNUM( 2 ) ? ( QUrl::FormattingOptions ) hb_parni( 2 ) : ( QUrl::FormattingOptions ) QUrl::None ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString userInfo () const
 */
HB_FUNC( QT_QURL_USERINFO )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->userInfo().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_USERINFO FP=hb_retc( ( p )->userInfo().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString userName () const
 */
HB_FUNC( QT_QURL_USERNAME )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->userName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_USERNAME FP=hb_retc( ( p )->userName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString fromAce ( const QByteArray & domain )
 */
HB_FUNC( QT_QURL_FROMACE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->fromAce( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_FROMACE FP=hb_retc( ( p )->fromAce( *hbqt_par_QByteArray( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QUrl fromEncoded ( const QByteArray & input )
 */
HB_FUNC( QT_QURL_FROMENCODED )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromEncoded( *hbqt_par_QByteArray( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_FROMENCODED FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromEncoded( *hbqt_par_QByteArray( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QUrl fromEncoded ( const QByteArray & input, ParsingMode parsingMode )
 */
HB_FUNC( QT_QURL_FROMENCODED_1 )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromEncoded( *hbqt_par_QByteArray( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_FROMENCODED_1 FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromEncoded( *hbqt_par_QByteArray( 2 ), ( QUrl::ParsingMode ) hb_parni( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QUrl fromLocalFile ( const QString & localFile )
 */
HB_FUNC( QT_QURL_FROMLOCALFILE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromLocalFile( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_FROMLOCALFILE FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->fromLocalFile( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString fromPercentEncoding ( const QByteArray & input )
 */
HB_FUNC( QT_QURL_FROMPERCENTENCODING )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retc( ( p )->fromPercentEncoding( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_FROMPERCENTENCODING FP=hb_retc( ( p )->fromPercentEncoding( *hbqt_par_QByteArray( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList idnWhitelist ()
 */
HB_FUNC( QT_QURL_IDNWHITELIST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->idnWhitelist() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_IDNWHITELIST FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->idnWhitelist() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setIdnWhitelist ( const QStringList & list )
 */
HB_FUNC( QT_QURL_SETIDNWHITELIST )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      ( p )->setIdnWhitelist( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_SETIDNWHITELIST FP=( p )->setIdnWhitelist( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * QByteArray toAce ( const QString & domain )
 */
HB_FUNC( QT_QURL_TOACE )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toAce( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_TOACE FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toAce( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QByteArray toPercentEncoding ( const QString & input, const QByteArray & exclude = QByteArray(), const QByteArray & include = QByteArray() )
 */
HB_FUNC( QT_QURL_TOPERCENTENCODING )
{
   QUrl * p = hbqt_par_QUrl( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toPercentEncoding( hbqt_par_QString( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QByteArray( 3 ) : QByteArray() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QByteArray( 4 ) : QByteArray() ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QURL_TOPERCENTENCODING FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toPercentEncoding( hbqt_par_QString( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QByteArray( 3 ) : QByteArray() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QByteArray( 4 ) : QByteArray() ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
