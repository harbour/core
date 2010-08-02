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
#include "hbqtwebkit_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtWebKit/QWebHistory>
#include "../hbqt_local.h"


/*
 *
 */

typedef struct
{
   QWebHistory * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QWebHistory;

QT_G_FUNC( hbqt_gcRelease_QWebHistory )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWebHistory( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QWebHistory * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebHistory;
   p->type = HBQT_TYPE_QWebHistory;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWebHistory", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWebHistory", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWEBHISTORY )
{
   //hb_retptr( ( * ) new () );
}

/*
 * void back ()
 */
HB_FUNC( QT_QWEBHISTORY_BACK )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->back();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_BACK FP=( p )->back(); p is NULL" ) );
   }
}

/*
 * QWebHistoryItem backItem () const
 */
HB_FUNC( QT_QWEBHISTORY_BACKITEM )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->backItem() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_BACKITEM FP=hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->backItem() ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QWebHistoryItem> backItems ( int maxItems ) const
 */
HB_FUNC( QT_QWEBHISTORY_BACKITEMS )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->backItems( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_BACKITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->backItems( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool canGoBack () const
 */
HB_FUNC( QT_QWEBHISTORY_CANGOBACK )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retl( ( p )->canGoBack() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_CANGOBACK FP=hb_retl( ( p )->canGoBack() ); p is NULL" ) );
   }
}

/*
 * bool canGoForward () const
 */
HB_FUNC( QT_QWEBHISTORY_CANGOFORWARD )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retl( ( p )->canGoForward() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_CANGOFORWARD FP=hb_retl( ( p )->canGoForward() ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QWEBHISTORY_CLEAR )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QWEBHISTORY_COUNT )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * QWebHistoryItem currentItem () const
 */
HB_FUNC( QT_QWEBHISTORY_CURRENTITEM )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->currentItem() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_CURRENTITEM FP=hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->currentItem() ), true ) ); p is NULL" ) );
   }
}

/*
 * int currentItemIndex () const
 */
HB_FUNC( QT_QWEBHISTORY_CURRENTITEMINDEX )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retni( ( p )->currentItemIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_CURRENTITEMINDEX FP=hb_retni( ( p )->currentItemIndex() ); p is NULL" ) );
   }
}

/*
 * void forward ()
 */
HB_FUNC( QT_QWEBHISTORY_FORWARD )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->forward();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_FORWARD FP=( p )->forward(); p is NULL" ) );
   }
}

/*
 * QWebHistoryItem forwardItem () const
 */
HB_FUNC( QT_QWEBHISTORY_FORWARDITEM )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->forwardItem() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_FORWARDITEM FP=hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->forwardItem() ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QWebHistoryItem> forwardItems ( int maxItems ) const
 */
HB_FUNC( QT_QWEBHISTORY_FORWARDITEMS )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->forwardItems( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_FORWARDITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->forwardItems( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void goToItem ( const QWebHistoryItem & item )
 */
HB_FUNC( QT_QWEBHISTORY_GOTOITEM )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->goToItem( *hbqt_par_QWebHistoryItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_GOTOITEM FP=( p )->goToItem( *hbqt_par_QWebHistoryItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWebHistoryItem itemAt ( int i ) const
 */
HB_FUNC( QT_QWEBHISTORY_ITEMAT )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->itemAt( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_ITEMAT FP=hb_retptrGC( hbqt_gcAllocate_QWebHistoryItem( new QWebHistoryItem( ( p )->itemAt( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QWebHistoryItem> items () const
 */
HB_FUNC( QT_QWEBHISTORY_ITEMS )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->items() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_ITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebHistoryItem>( ( p )->items() ), true ) ); p is NULL" ) );
   }
}

/*
 * int maximumItemCount () const
 */
HB_FUNC( QT_QWEBHISTORY_MAXIMUMITEMCOUNT )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      hb_retni( ( p )->maximumItemCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_MAXIMUMITEMCOUNT FP=hb_retni( ( p )->maximumItemCount() ); p is NULL" ) );
   }
}

/*
 * void setMaximumItemCount ( int count )
 */
HB_FUNC( QT_QWEBHISTORY_SETMAXIMUMITEMCOUNT )
{
   QWebHistory * p = hbqt_par_QWebHistory( 1 );
   if( p )
      ( p )->setMaximumItemCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHISTORY_SETMAXIMUMITEMCOUNT FP=( p )->setMaximumItemCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
