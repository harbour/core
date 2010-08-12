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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QGraphicsLayoutItem>
#include <QtCore/QRectF>
#include <QtCore/QSizeF>
#include <QtCore/QPointF>


/*
 * QGraphicsLayoutItem ( QGraphicsLayoutItem * parent = 0, bool isLayout = false )
 * virtual ~QGraphicsLayoutItem ()
 */

typedef struct
{
   QGraphicsLayoutItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsLayoutItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsLayoutItem )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsLayoutItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsLayoutItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsLayoutItem;
   p->type = HBQT_TYPE_QGraphicsLayoutItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsLayoutItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsLayoutItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSLAYOUTITEM )
{
   // hb_retptr( new QGraphicsLayoutItem() );
}

/*
 * QRectF contentsRect () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_CONTENTSRECT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->contentsRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_CONTENTSRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->contentsRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QSizeF effectiveSizeHint ( Qt::SizeHint which, const QSizeF & constraint = QSizeF() ) const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_EFFECTIVESIZEHINT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->effectiveSizeHint( ( Qt::SizeHint ) hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QSizeF( 3 ) : QSizeF() ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_EFFECTIVESIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->effectiveSizeHint( ( Qt::SizeHint ) hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QSizeF( 3 ) : QSizeF() ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF geometry () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_GEOMETRY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_GEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void getContentsMargins ( qreal * left, qreal * top, qreal * right, qreal * bottom ) const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_GETCONTENTSMARGINS )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   qreal qrLeft = 0;
   qreal qrTop = 0;
   qreal qrRight = 0;
   qreal qrBottom = 0;

   if( p )
      ( p )->getContentsMargins( &qrLeft, &qrTop, &qrRight, &qrBottom );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_GETCONTENTSMARGINS FP=( p )->getContentsMargins( &qrLeft, &qrTop, &qrRight, &qrBottom ); p is NULL" ) );
   }

   hb_stornd( qrLeft, 2 );
   hb_stornd( qrTop, 3 );
   hb_stornd( qrRight, 4 );
   hb_stornd( qrBottom, 5 );
}

/*
 * QGraphicsItem * graphicsItem () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_GRAPHICSITEM )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->graphicsItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_GRAPHICSITEM FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->graphicsItem(), false ) ); p is NULL" ) );
   }
}

/*
 * bool isLayout () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_ISLAYOUT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retl( ( p )->isLayout() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_ISLAYOUT FP=hb_retl( ( p )->isLayout() ); p is NULL" ) );
   }
}

/*
 * qreal maximumHeight () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MAXIMUMHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retnd( ( p )->maximumHeight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_MAXIMUMHEIGHT FP=hb_retnd( ( p )->maximumHeight() ); p is NULL" ) );
   }
}

/*
 * QSizeF maximumSize () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MAXIMUMSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->maximumSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_MAXIMUMSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->maximumSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal maximumWidth () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MAXIMUMWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retnd( ( p )->maximumWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_MAXIMUMWIDTH FP=hb_retnd( ( p )->maximumWidth() ); p is NULL" ) );
   }
}

/*
 * qreal minimumHeight () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MINIMUMHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retnd( ( p )->minimumHeight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_MINIMUMHEIGHT FP=hb_retnd( ( p )->minimumHeight() ); p is NULL" ) );
   }
}

/*
 * QSizeF minimumSize () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MINIMUMSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->minimumSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_MINIMUMSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->minimumSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal minimumWidth () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_MINIMUMWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retnd( ( p )->minimumWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_MINIMUMWIDTH FP=hb_retnd( ( p )->minimumWidth() ); p is NULL" ) );
   }
}

/*
 * bool ownedByLayout () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_OWNEDBYLAYOUT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retl( ( p )->ownedByLayout() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_OWNEDBYLAYOUT FP=hb_retl( ( p )->ownedByLayout() ); p is NULL" ) );
   }
}

/*
 * QGraphicsLayoutItem * parentLayoutItem () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_PARENTLAYOUTITEM )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLayoutItem( ( p )->parentLayoutItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_PARENTLAYOUTITEM FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsLayoutItem( ( p )->parentLayoutItem(), false ) ); p is NULL" ) );
   }
}

/*
 * qreal preferredHeight () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_PREFERREDHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retnd( ( p )->preferredHeight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_PREFERREDHEIGHT FP=hb_retnd( ( p )->preferredHeight() ); p is NULL" ) );
   }
}

/*
 * QSizeF preferredSize () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_PREFERREDSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->preferredSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_PREFERREDSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->preferredSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal preferredWidth () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_PREFERREDWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retnd( ( p )->preferredWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_PREFERREDWIDTH FP=hb_retnd( ( p )->preferredWidth() ); p is NULL" ) );
   }
}

/*
 * virtual void setGeometry ( const QRectF & rect )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETGEOMETRY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRectF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETGEOMETRY FP=( p )->setGeometry( *hbqt_par_QRectF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumHeight ( qreal height )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setMaximumHeight( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMHEIGHT FP=( p )->setMaximumHeight( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumSize ( const QSizeF & size )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setMaximumSize( *hbqt_par_QSizeF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMSIZE FP=( p )->setMaximumSize( *hbqt_par_QSizeF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumSize ( qreal w, qreal h )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMSIZE_1 )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setMaximumSize( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMSIZE_1 FP=( p )->setMaximumSize( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumWidth ( qreal width )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setMaximumWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETMAXIMUMWIDTH FP=( p )->setMaximumWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumHeight ( qreal height )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMINIMUMHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setMinimumHeight( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETMINIMUMHEIGHT FP=( p )->setMinimumHeight( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumSize ( const QSizeF & size )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMINIMUMSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setMinimumSize( *hbqt_par_QSizeF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETMINIMUMSIZE FP=( p )->setMinimumSize( *hbqt_par_QSizeF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumSize ( qreal w, qreal h )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMINIMUMSIZE_1 )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setMinimumSize( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETMINIMUMSIZE_1 FP=( p )->setMinimumSize( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumWidth ( qreal width )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETMINIMUMWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setMinimumWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETMINIMUMWIDTH FP=( p )->setMinimumWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setParentLayoutItem ( QGraphicsLayoutItem * parent )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPARENTLAYOUTITEM )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setParentLayoutItem( hbqt_par_QGraphicsLayoutItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETPARENTLAYOUTITEM FP=( p )->setParentLayoutItem( hbqt_par_QGraphicsLayoutItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPreferredHeight ( qreal height )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPREFERREDHEIGHT )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setPreferredHeight( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETPREFERREDHEIGHT FP=( p )->setPreferredHeight( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPreferredSize ( const QSizeF & size )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPREFERREDSIZE )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setPreferredSize( *hbqt_par_QSizeF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETPREFERREDSIZE FP=( p )->setPreferredSize( *hbqt_par_QSizeF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPreferredSize ( qreal w, qreal h )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPREFERREDSIZE_1 )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setPreferredSize( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETPREFERREDSIZE_1 FP=( p )->setPreferredSize( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setPreferredWidth ( qreal width )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETPREFERREDWIDTH )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setPreferredWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETPREFERREDWIDTH FP=( p )->setPreferredWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizePolicy ( const QSizePolicy & policy )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETSIZEPOLICY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setSizePolicy( *hbqt_par_QSizePolicy( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETSIZEPOLICY FP=( p )->setSizePolicy( *hbqt_par_QSizePolicy( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizePolicy ( QSizePolicy::Policy hPolicy, QSizePolicy::Policy vPolicy, QSizePolicy::ControlType controlType = QSizePolicy::DefaultType )
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SETSIZEPOLICY_1 )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->setSizePolicy( ( QSizePolicy::Policy ) hb_parni( 2 ), ( QSizePolicy::Policy ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QSizePolicy::ControlType ) hb_parni( 4 ) : ( QSizePolicy::ControlType ) QSizePolicy::DefaultType ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SETSIZEPOLICY_1 FP=( p )->setSizePolicy( ( QSizePolicy::Policy ) hb_parni( 2 ), ( QSizePolicy::Policy ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QSizePolicy::ControlType ) hb_parni( 4 ) : ( QSizePolicy::ControlType ) QSizePolicy::DefaultType ) ); p is NULL" ) );
   }
}

/*
 * QSizePolicy sizePolicy () const
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_SIZEPOLICY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizePolicy( new QSizePolicy( ( p )->sizePolicy() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_SIZEPOLICY FP=hb_retptrGC( hbqt_gcAllocate_QSizePolicy( new QSizePolicy( ( p )->sizePolicy() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void updateGeometry ()
 */
HB_FUNC( QT_QGRAPHICSLAYOUTITEM_UPDATEGEOMETRY )
{
   QGraphicsLayoutItem * p = hbqt_par_QGraphicsLayoutItem( 1 );
   if( p )
      ( p )->updateGeometry();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSLAYOUTITEM_UPDATEGEOMETRY FP=( p )->updateGeometry(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
