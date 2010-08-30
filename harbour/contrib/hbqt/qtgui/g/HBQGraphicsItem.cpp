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

#include <QtGui/QGraphicsItem>
#include "hbqt_hbqgraphicsitem.h"

/*
 * HBQGraphicsItem()
 * HBQGraphicsItem( QGraphicsItem * parent )
 * ~HBQGraphicsItem()
 */

typedef struct
{
   HBQGraphicsItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQGraphicsItem;

HBQT_GC_FUNC( hbqt_gcRelease_HBQGraphicsItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_HBQGraphicsItem   /.\\", p->ph ) );
         delete ( ( HBQGraphicsItem * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_HBQGraphicsItem   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_HBQGraphicsItem    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_HBQGraphicsItem    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQGraphicsItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( HBQGraphicsItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQGraphicsItem;
   p->type = HBQT_TYPE_HBQGraphicsItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_HBQGraphicsItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_HBQGraphicsItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_HBQGRAPHICSITEM )
{
   HBQGraphicsItem * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = new HBQGraphicsItem( hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new HBQGraphicsItem( hb_parni( 1 ), hbqt_par_QGraphicsItem( 2 ) ) ;
   }
   else {
      pObj = new HBQGraphicsItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_HBQGraphicsItem( ( void * ) pObj, true ) );
}

/*
 * void           hbSetBlock( PHB_ITEM block )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_HBSETBLOCK )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->hbSetBlock( hb_param( 2, HB_IT_ANY ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_HBSETBLOCK FP=( p )->hbSetBlock( hb_param( 2, HB_IT_ANY ) ); p is NULL" ) );
   }
}

/*
 * QRectF         boundingRect() const
 */
HB_FUNC( QT_HBQGRAPHICSITEM_BOUNDINGRECT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void   paint( QPainter * painter, const QStyleOptionGraphicsItem * option, QWidget * widget = 0 )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_PAINT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), hbqt_par_QStyleOptionGraphicsItem( 3 ), hbqt_par_QWidget( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_PAINT FP=( p )->paint( hbqt_par_QPainter( 2 ), hbqt_par_QStyleOptionGraphicsItem( 3 ), hbqt_par_QWidget( 4 ) ); p is NULL" ) );
   }
}

/*
 * int            determineResizeMode( const QPointF & pos )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_DETERMINERESIZEMODE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->determineResizeMode( *hbqt_par_QPointF( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_DETERMINERESIZEMODE FP=hb_retni( ( p )->determineResizeMode( *hbqt_par_QPointF( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QRectF         adjustRect( QRectF & rect )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_ADJUSTRECT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->adjustRect( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_ADJUSTRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->adjustRect( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void   prepare( QPainter * painter )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_PREPARE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->prepare( hbqt_par_QPainter( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_PREPARE FP=( p )->prepare( hbqt_par_QPainter( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           drawSelection( QPainter * painter, const QRectF & rect )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_DRAWSELECTION )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->drawSelection( hbqt_par_QPainter( 2 ), *hbqt_par_QRectF( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_DRAWSELECTION FP=( p )->drawSelection( hbqt_par_QPainter( 2 ), *hbqt_par_QRectF( 3 ) ); p is NULL" ) );
   }
}

/*
 * void           setupPainter( QPainter * painter )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETUPPAINTER )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setupPainter( hbqt_par_QPainter( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETUPPAINTER FP=( p )->setupPainter( hbqt_par_QPainter( 2 ) ); p is NULL" ) );
   }
}

/*
 * QPen           pen()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_PEN )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->pen() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_PEN FP=hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->pen() ), true ) ); p is NULL" ) );
   }
}

/*
 * void           setPen( const QPen & pen )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETPEN )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setPen( *hbqt_par_QPen( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETPEN FP=( p )->setPen( *hbqt_par_QPen( 2 ) ); p is NULL" ) );
   }
}

/*
 * QBrush         brush()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_BRUSH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_BRUSH FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush() ), true ) ); p is NULL" ) );
   }
}

/*
 * void           setBrush( const QBrush & brush )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBRUSH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBrush( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETBRUSH FP=( p )->setBrush( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * QBrush         backgroundBrush()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_BACKGROUNDBRUSH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->backgroundBrush() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_BACKGROUNDBRUSH FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->backgroundBrush() ), true ) ); p is NULL" ) );
   }
}

/*
 * void           setBackgroundBrush( const QBrush & brush )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBACKGROUNDBRUSH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBackgroundBrush( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETBACKGROUNDBRUSH FP=( p )->setBackgroundBrush( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * QFont          font()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_FONT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * void           setFont( const QFont & font )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETFONT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            lineStyle()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_LINESTYLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->lineStyle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_LINESTYLE FP=hb_retni( ( p )->lineStyle() ); p is NULL" ) );
   }
}

/*
 * void           setLineStyle( int lineStyle )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETLINESTYLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setLineStyle( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETLINESTYLE FP=( p )->setLineStyle( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            startAngle()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_STARTANGLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->startAngle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_STARTANGLE FP=hb_retni( ( p )->startAngle() ); p is NULL" ) );
   }
}

/*
 * void           setStartAngle( int startAngle )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSTARTANGLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setStartAngle( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETSTARTANGLE FP=( p )->setStartAngle( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            spanAngle()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SPANANGLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->spanAngle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SPANANGLE FP=hb_retni( ( p )->spanAngle() ); p is NULL" ) );
   }
}

/*
 * void           setSpanAngle( int spanAngle )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSPANANGLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setSpanAngle( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETSPANANGLE FP=( p )->setSpanAngle( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * qreal          width() const
 */
HB_FUNC( QT_HBQGRAPHICSITEM_WIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->width() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_WIDTH FP=hb_retnd( ( p )->width() ); p is NULL" ) );
   }
}

/*
 * void           setWidth( qreal width )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETWIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETWIDTH FP=( p )->setWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * qreal          height() const
 */
HB_FUNC( QT_HBQGRAPHICSITEM_HEIGHT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_HEIGHT FP=hb_retnd( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * void           setHeight( qreal height )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETHEIGHT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setHeight( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETHEIGHT FP=( p )->setHeight( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            opacity()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_OPACITY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->opacity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_OPACITY FP=hb_retni( ( p )->opacity() ); p is NULL" ) );
   }
}

/*
 * void           setOpacity( const int opacity )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETOPACITY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setOpacity( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETOPACITY FP=( p )->setOpacity( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QRectF         geometry()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_GEOMETRY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_GEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * void           setGeometry( const QRectF & rect )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETGEOMETRY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRectF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETGEOMETRY FP=( p )->setGeometry( *hbqt_par_QRectF( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString        objectType()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_OBJECTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retc( ( p )->objectType().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_OBJECTTYPE FP=hb_retc( ( p )->objectType().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void           setObjectType( const QString & type )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETOBJECTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setObjectType( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETOBJECTTYPE FP=( p )->setObjectType( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString        objectName()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_OBJECTNAME )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retc( ( p )->objectName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_OBJECTNAME FP=hb_retc( ( p )->objectName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void           setObjectName( const QString & name )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETOBJECTNAME )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setObjectName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETOBJECTNAME FP=( p )->setObjectName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString        text()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_TEXT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void           setText( const QString & type )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETTEXT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setText( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETTEXT FP=( p )->setText( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            paintType()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_PAINTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->paintType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_PAINTTYPE FP=hb_retni( ( p )->paintType() ); p is NULL" ) );
   }
}

/*
 * void           setPaintType( int paintType )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETPAINTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setPaintType( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETPAINTTYPE FP=( p )->setPaintType( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            frameType()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_FRAMETYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->frameType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_FRAMETYPE FP=hb_retni( ( p )->frameType() ); p is NULL" ) );
   }
}

/*
 * void           setFrameType( int frameType )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETFRAMETYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setFrameType( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETFRAMETYPE FP=( p )->setFrameType( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            drawTextType()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_DRAWTEXTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->drawTextType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_DRAWTEXTTYPE FP=hb_retni( ( p )->drawTextType() ); p is NULL" ) );
   }
}

/*
 * void           setDrawTextType( int drawTextType )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETDRAWTEXTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setDrawTextType( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETDRAWTEXTTYPE FP=( p )->setDrawTextType( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QPixmap        pixmap()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_PIXMAP )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_PIXMAP FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) ); p is NULL" ) );
   }
}

/*
 * void           setPixmap( const QPixmap & pixmap )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETPIXMAP )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETPIXMAP FP=( p )->setPixmap( *hbqt_par_QPixmap( 2 ) ); p is NULL" ) );
   }
}

/*
 * QColor         textColor()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_TEXTCOLOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->textColor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_TEXTCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->textColor() ), true ) ); p is NULL" ) );
   }
}

/*
 * void           setTextColor( const QColor & color )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETTEXTCOLOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setTextColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETTEXTCOLOR FP=( p )->setTextColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            borderWidth()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_BORDERWIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->borderWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_BORDERWIDTH FP=hb_retni( ( p )->borderWidth() ); p is NULL" ) );
   }
}

/*
 * void           setBorderWidth( int bWidth )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBORDERWIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBorderWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETBORDERWIDTH FP=( p )->setBorderWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QColor         borderColor()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_BORDERCOLOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->borderColor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_BORDERCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->borderColor() ), true ) ); p is NULL" ) );
   }
}

/*
 * void           setBorderColor( const QColor & color )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBORDERCOLOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBorderColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETBORDERCOLOR FP=( p )->setBorderColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            sizePolicy()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SIZEPOLICY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->sizePolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SIZEPOLICY FP=hb_retni( ( p )->sizePolicy() ); p is NULL" ) );
   }
}

/*
 * void           setSizePolicy( int sizePolicy )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSIZEPOLICY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setSizePolicy( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETSIZEPOLICY FP=( p )->setSizePolicy( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            textFlags()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_TEXTFLAGS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->textFlags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_TEXTFLAGS FP=hb_retni( ( p )->textFlags() ); p is NULL" ) );
   }
}

/*
 * void           setTextFlags( int textFlags )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETTEXTFLAGS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setTextFlags( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETTEXTFLAGS FP=( p )->setTextFlags( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            resizeFlags()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_RESIZEFLAGS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->resizeFlags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_RESIZEFLAGS FP=hb_retni( ( p )->resizeFlags() ); p is NULL" ) );
   }
}

/*
 * void           setResizeFlags( int resizeFlags )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETRESIZEFLAGS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setResizeFlags( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETRESIZEFLAGS FP=( p )->setResizeFlags( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            resizeHandle()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_RESIZEHANDLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->resizeHandle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_RESIZEHANDLE FP=hb_retni( ( p )->resizeHandle() ); p is NULL" ) );
   }
}

/*
 * void           setResizeHandle( int resizeHandle )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETRESIZEHANDLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setResizeHandle( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETRESIZEHANDLE FP=( p )->setResizeHandle( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int            barsIdentation()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_BARSIDENTATION )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->barsIdentation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_BARSIDENTATION FP=hb_retni( ( p )->barsIdentation() ); p is NULL" ) );
   }
}

/*
 * void           setBarsIdentation( int barsIdentation )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBARSIDENTATION )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBarsIdentation( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETBARSIDENTATION FP=( p )->setBarsIdentation( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool           drawBorder()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_DRAWBORDER )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->drawBorder() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_DRAWBORDER FP=hb_retl( ( p )->drawBorder() ); p is NULL" ) );
   }
}

/*
 * void           setDrawBorder( bool drawBorder )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETDRAWBORDER )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setDrawBorder( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETDRAWBORDER FP=( p )->setDrawBorder( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool           showGrid()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SHOWGRID )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->showGrid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SHOWGRID FP=hb_retl( ( p )->showGrid() ); p is NULL" ) );
   }
}

/*
 * void           setShowGrid( bool showGrid )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSHOWGRID )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setShowGrid( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETSHOWGRID FP=( p )->setShowGrid( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool           showLabels()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SHOWLABELS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->showLabels() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SHOWLABELS FP=hb_retl( ( p )->showLabels() ); p is NULL" ) );
   }
}

/*
 * void           setShowLabels( bool showLabels )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSHOWLABELS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setShowLabels( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETSHOWLABELS FP=( p )->setShowLabels( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * qreal          toColorFactor()
 */
HB_FUNC( QT_HBQGRAPHICSITEM_TOCOLORFACTOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->toColorFactor() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_TOCOLORFACTOR FP=hb_retnd( ( p )->toColorFactor() ); p is NULL" ) );
   }
}

/*
 * void           setToColorFactor( qreal toColorFactor )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETTOCOLORFACTOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setToColorFactor( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETTOCOLORFACTOR FP=( p )->setToColorFactor( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           setBarValues( const QStringList & list )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBARVALUES )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBarValues( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETBARVALUES FP=( p )->setBarValues( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           setLegendColorRectWidth( int legendColorRectWidth )
 */
HB_FUNC( QT_HBQGRAPHICSITEM_SETLEGENDCOLORRECTWIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setLegendColorRectWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSITEM_SETLEGENDCOLORRECTWIDTH FP=( p )->setLegendColorRectWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
