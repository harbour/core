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

#include <QtCore/QPointer>

#include <qvariant.h>

#include <QtWebKit/QWebHistoryItem>


/*
 * QWebHistoryItem ( const QWebHistoryItem & other )
 * ~QWebHistoryItem ()
 */

QT_G_FUNC( release_QWebHistoryItem )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QWebHistoryItem" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      delete ( ( QWebHistoryItem * ) ph );
      ph = NULL;
   }
}

HB_FUNC( QT_QWEBHISTORYITEM )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QWebHistoryItem( *hbqt_par_QWebHistoryItem( 1 ) ) ;
   }

   p->ph = pObj;
   p->func = release_QWebHistoryItem;

   hb_retptrGC( p );
}
/*
 * QIcon icon () const
 */
HB_FUNC( QT_QWEBHISTORYITEM_ICON )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QIcon( hbqt_par_QWebHistoryItem( 1 )->icon() ), release_QIcon ) );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QWEBHISTORYITEM_ISVALID )
{
   hb_retl( hbqt_par_QWebHistoryItem( 1 )->isValid() );
}

/*
 * QDateTime lastVisited () const
 */
HB_FUNC( QT_QWEBHISTORYITEM_LASTVISITED )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QDateTime( hbqt_par_QWebHistoryItem( 1 )->lastVisited() ), release_QDateTime ) );
}

/*
 * QUrl originalUrl () const
 */
HB_FUNC( QT_QWEBHISTORYITEM_ORIGINALURL )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QUrl( hbqt_par_QWebHistoryItem( 1 )->originalUrl() ), release_QUrl ) );
}

/*
 * void setUserData ( const QVariant & userData )
 */
HB_FUNC( QT_QWEBHISTORYITEM_SETUSERDATA )
{
   hbqt_par_QWebHistoryItem( 1 )->setUserData( *hbqt_par_QVariant( 2 ) );
}

/*
 * QString title () const
 */
HB_FUNC( QT_QWEBHISTORYITEM_TITLE )
{
   hb_retc( hbqt_par_QWebHistoryItem( 1 )->title().toAscii().data() );
}

/*
 * QUrl url () const
 */
HB_FUNC( QT_QWEBHISTORYITEM_URL )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QUrl( hbqt_par_QWebHistoryItem( 1 )->url() ), release_QUrl ) );
}

/*
 * QVariant userData () const
 */
HB_FUNC( QT_QWEBHISTORYITEM_USERDATA )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QWebHistoryItem( 1 )->userData() ), release_QVariant ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
