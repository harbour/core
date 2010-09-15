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

/*
 *  enum ItemIndexMethod { BspTreeIndex, NoIndex }
 *  enum SceneLayer { ItemLayer, BackgroundLayer, ForegroundLayer, AllLayers }
 *  flags SceneLayers
 */

/*
 *  Constructed[ 69/72 [ 95.83% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 *  QGraphicsItemGroup * createItemGroup ( const QList<QGraphicsItem *> & items )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // virtual QVariant inputMethodQuery ( Qt::InputMethodQuery query ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsScene>
#include <QtGui/QPalette>

/*
 * QGraphicsScene ( QObject * parent = 0 )
 * QGraphicsScene ( const QRectF & sceneRect, QObject * parent = 0 )
 * QGraphicsScene ( qreal x, qreal y, qreal width, qreal height, QObject * parent = 0 )
 * virtual ~QGraphicsScene ()
 */

typedef struct
{
   QPointer< QGraphicsScene > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsScene;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsScene )
{
   QGraphicsScene  * ph = NULL ;
   HBQT_GC_T_QGraphicsScene * p = ( HBQT_GC_T_QGraphicsScene * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGraphicsScene   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGraphicsScene   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QGraphicsScene          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGraphicsScene    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGraphicsScene    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsScene( void * pObj, bool bNew )
{
   HBQT_GC_T_QGraphicsScene * p = ( HBQT_GC_T_QGraphicsScene * ) hb_gcAllocate( sizeof( HBQT_GC_T_QGraphicsScene ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QGraphicsScene >( ( QGraphicsScene * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsScene;
   p->type = HBQT_TYPE_QGraphicsScene;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsScene  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsScene", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSSCENE )
{
   QGraphicsScene * pObj = NULL;

   if( hb_pcount() >= 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QGraphicsScene( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISPOINTER( 5 ) ? hbqt_par_QObject( 5 ) : 0 ) ) ;
   }
   else if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QRectF )
      {
         pObj = new QGraphicsScene( *hbqt_par_QRectF( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QObject( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsScene( hbqt_par_QObject( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsScene() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsScene( ( void * ) pObj, true ) );
}

/*
 * QGraphicsWidget * activeWindow () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ACTIVEWINDOW )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->activeWindow(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ACTIVEWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->activeWindow(), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsEllipseItem * addEllipse ( const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDELLIPSE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsEllipseItem( ( p )->addEllipse( *hbqt_par_QRectF( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDELLIPSE FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsEllipseItem( ( p )->addEllipse( *hbqt_par_QRectF( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsEllipseItem * addEllipse ( qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDELLIPSE_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsEllipseItem( ( p )->addEllipse( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISPOINTER( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ), ( HB_ISPOINTER( 7 ) ? *hbqt_par_QBrush( 7 ) : QBrush() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDELLIPSE_1 FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsEllipseItem( ( p )->addEllipse( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISPOINTER( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ), ( HB_ISPOINTER( 7 ) ? *hbqt_par_QBrush( 7 ) : QBrush() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void addItem ( QGraphicsItem * item )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDITEM )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QGRAPHICSSCENE_ADDITEM()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QGRAPHICSSCENE_ADDITEM() Qt object: %p is attached to: %p", p->ph, q->ph ) );
      q->bNew = HB_FALSE;
      hbqt_par_QGraphicsScene( 1 )->addItem( hbqt_par_QGraphicsItem( 2 ) );
   }
}

/*
 * QGraphicsLineItem * addLine ( const QLineF & line, const QPen & pen = QPen() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDLINE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLineItem( ( p )->addLine( *hbqt_par_QLineF( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDLINE FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsLineItem( ( p )->addLine( *hbqt_par_QLineF( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsLineItem * addLine ( qreal x1, qreal y1, qreal x2, qreal y2, const QPen & pen = QPen() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDLINE_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLineItem( ( p )->addLine( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISPOINTER( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDLINE_1 FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsLineItem( ( p )->addLine( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISPOINTER( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsPathItem * addPath ( const QPainterPath & path, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDPATH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsPathItem( ( p )->addPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDPATH FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsPathItem( ( p )->addPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsPixmapItem * addPixmap ( const QPixmap & pixmap )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDPIXMAP )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsPixmapItem( ( p )->addPixmap( *hbqt_par_QPixmap( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDPIXMAP FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsPixmapItem( ( p )->addPixmap( *hbqt_par_QPixmap( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsPolygonItem * addPolygon ( const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDPOLYGON )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsPolygonItem( ( p )->addPolygon( *hbqt_par_QPolygonF( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDPOLYGON FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsPolygonItem( ( p )->addPolygon( *hbqt_par_QPolygonF( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsRectItem * addRect ( const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDRECT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsRectItem( ( p )->addRect( *hbqt_par_QRectF( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDRECT FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsRectItem( ( p )->addRect( *hbqt_par_QRectF( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsRectItem * addRect ( qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDRECT_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsRectItem( ( p )->addRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISPOINTER( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ), ( HB_ISPOINTER( 7 ) ? *hbqt_par_QBrush( 7 ) : QBrush() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDRECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsRectItem( ( p )->addRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISPOINTER( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ), ( HB_ISPOINTER( 7 ) ? *hbqt_par_QBrush( 7 ) : QBrush() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsSimpleTextItem * addSimpleText ( const QString & text, const QFont & font = QFont() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDSIMPLETEXT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsSimpleTextItem( ( p )->addSimpleText( QGraphicsScene::tr( hb_parc( 2 ) ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QFont( 3 ) : QFont() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDSIMPLETEXT FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsSimpleTextItem( ( p )->addSimpleText( QGraphicsScene::tr( hb_parc( 2 ) ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QFont( 3 ) : QFont() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsTextItem * addText ( const QString & text, const QFont & font = QFont() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDTEXT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsTextItem( ( p )->addText( QGraphicsScene::tr( hb_parc( 2 ) ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QFont( 3 ) : QFont() ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDTEXT FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsTextItem( ( p )->addText( QGraphicsScene::tr( hb_parc( 2 ) ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QFont( 3 ) : QFont() ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsProxyWidget * addWidget ( QWidget * widget, Qt::WindowFlags wFlags = 0 )
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADDWIDGET )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsProxyWidget( ( p )->addWidget( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADDWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsProxyWidget( ( p )->addWidget( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QBrush backgroundBrush () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_BACKGROUNDBRUSH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->backgroundBrush() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_BACKGROUNDBRUSH FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->backgroundBrush() ), true ) ); p is NULL" ) );
   }
}

/*
 * int bspTreeDepth () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_BSPTREEDEPTH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retni( ( p )->bspTreeDepth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_BSPTREEDEPTH FP=hb_retni( ( p )->bspTreeDepth() ); p is NULL" ) );
   }
}

/*
 * void clearFocus ()
 */
HB_FUNC( QT_QGRAPHICSSCENE_CLEARFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->clearFocus();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_CLEARFOCUS FP=( p )->clearFocus(); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> collidingItems ( const QGraphicsItem * item, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
 */
HB_FUNC( QT_QGRAPHICSSCENE_COLLIDINGITEMS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->collidingItems( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_COLLIDINGITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->collidingItems( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void destroyItemGroup ( QGraphicsItemGroup * group )
 */
HB_FUNC( QT_QGRAPHICSSCENE_DESTROYITEMGROUP )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->destroyItemGroup( hbqt_par_QGraphicsItemGroup( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_DESTROYITEMGROUP FP=( p )->destroyItemGroup( hbqt_par_QGraphicsItemGroup( 2 ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsItem * focusItem () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_FOCUSITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->focusItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_FOCUSITEM FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->focusItem(), false ) ); p is NULL" ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_FONT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * QBrush foregroundBrush () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_FOREGROUNDBRUSH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foregroundBrush() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_FOREGROUNDBRUSH FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foregroundBrush() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool hasFocus () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_HASFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retl( ( p )->hasFocus() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_HASFOCUS FP=hb_retl( ( p )->hasFocus() ); p is NULL" ) );
   }
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_HEIGHT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retnd( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_HEIGHT FP=hb_retnd( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * void invalidate ( qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers )
 */
HB_FUNC( QT_QGRAPHICSSCENE_INVALIDATE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->invalidate( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISNUM( 6 ) ? ( QGraphicsScene::SceneLayers ) hb_parni( 6 ) : ( QGraphicsScene::SceneLayers ) QGraphicsScene::AllLayers ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_INVALIDATE FP=( p )->invalidate( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISNUM( 6 ) ? ( QGraphicsScene::SceneLayers ) hb_parni( 6 ) : ( QGraphicsScene::SceneLayers ) QGraphicsScene::AllLayers ) ); p is NULL" ) );
   }
}

/*
 * bool isSortCacheEnabled () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ISSORTCACHEENABLED )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retl( ( p )->isSortCacheEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ISSORTCACHEENABLED FP=hb_retl( ( p )->isSortCacheEnabled() ); p is NULL" ) );
   }
}

/*
 * QGraphicsItem * itemAt ( const QPointF & position ) const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMAT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->itemAt( *hbqt_par_QPointF( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMAT FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->itemAt( *hbqt_par_QPointF( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsItem * itemAt ( qreal x, qreal y ) const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMAT_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->itemAt( hb_parnd( 2 ), hb_parnd( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMAT_1 FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->itemAt( hb_parnd( 2 ), hb_parnd( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * ItemIndexMethod itemIndexMethod () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMINDEXMETHOD )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retni( ( QGraphicsScene::ItemIndexMethod ) ( p )->itemIndexMethod() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMINDEXMETHOD FP=hb_retni( ( QGraphicsScene::ItemIndexMethod ) ( p )->itemIndexMethod() ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> items () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items() ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> items ( const QPointF & pos ) const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPointF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMS_1 FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPointF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> items ( qreal x, qreal y, qreal w, qreal h, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_2 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::ItemSelectionMode ) hb_parni( 6 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMS_2 FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::ItemSelectionMode ) hb_parni( 6 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> items ( const QRectF & rectangle, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_3 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QRectF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMS_3 FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QRectF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> items ( const QPolygonF & polygon, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_4 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPolygonF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMS_4 FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPolygonF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> items ( const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_5 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMS_5 FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF itemsBoundingRect () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMSBOUNDINGRECT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->itemsBoundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ITEMSBOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->itemsBoundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsItem * mouseGrabberItem () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_MOUSEGRABBERITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->mouseGrabberItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_MOUSEGRABBERITEM FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->mouseGrabberItem(), false ) ); p is NULL" ) );
   }
}

/*
 * QPalette palette () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_PALETTE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_PALETTE FP=hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) ); p is NULL" ) );
   }
}

/*
 * void removeItem ( QGraphicsItem * item )
 */
HB_FUNC( QT_QGRAPHICSSCENE_REMOVEITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->removeItem( hbqt_par_QGraphicsItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_REMOVEITEM FP=( p )->removeItem( hbqt_par_QGraphicsItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void render ( QPainter * painter, const QRectF & target = QRectF(), const QRectF & source = QRectF(), Qt::AspectRatioMode aspectRatioMode = Qt::KeepAspectRatio )
 */
HB_FUNC( QT_QGRAPHICSSCENE_RENDER )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->render( hbqt_par_QPainter( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QRectF( 3 ) : QRectF() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QRectF( 4 ) : QRectF() ), ( HB_ISNUM( 5 ) ? ( Qt::AspectRatioMode ) hb_parni( 5 ) : ( Qt::AspectRatioMode ) Qt::KeepAspectRatio ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_RENDER FP=( p )->render( hbqt_par_QPainter( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QRectF( 3 ) : QRectF() ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QRectF( 4 ) : QRectF() ), ( HB_ISNUM( 5 ) ? ( Qt::AspectRatioMode ) hb_parni( 5 ) : ( Qt::AspectRatioMode ) Qt::KeepAspectRatio ) ); p is NULL" ) );
   }
}

/*
 * QRectF sceneRect () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_SCENERECT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->sceneRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SCENERECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->sceneRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> selectedItems () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_SELECTEDITEMS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->selectedItems() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SELECTEDITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->selectedItems() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath selectionArea () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_SELECTIONAREA )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->selectionArea() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SELECTIONAREA FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->selectionArea() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setActiveWindow ( QGraphicsWidget * widget )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETACTIVEWINDOW )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setActiveWindow( hbqt_par_QGraphicsWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETACTIVEWINDOW FP=( p )->setActiveWindow( hbqt_par_QGraphicsWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackgroundBrush ( const QBrush & brush )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETBACKGROUNDBRUSH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setBackgroundBrush( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETBACKGROUNDBRUSH FP=( p )->setBackgroundBrush( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBspTreeDepth ( int depth )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETBSPTREEDEPTH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setBspTreeDepth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETBSPTREEDEPTH FP=( p )->setBspTreeDepth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFocus ( Qt::FocusReason focusReason = Qt::OtherFocusReason )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setFocus( ( HB_ISNUM( 2 ) ? ( Qt::FocusReason ) hb_parni( 2 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETFOCUS FP=( p )->setFocus( ( HB_ISNUM( 2 ) ? ( Qt::FocusReason ) hb_parni( 2 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) ); p is NULL" ) );
   }
}

/*
 * void setFocusItem ( QGraphicsItem * item, Qt::FocusReason focusReason = Qt::OtherFocusReason )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETFOCUSITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setFocusItem( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FocusReason ) hb_parni( 3 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETFOCUSITEM FP=( p )->setFocusItem( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FocusReason ) hb_parni( 3 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETFONT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setForegroundBrush ( const QBrush & brush )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETFOREGROUNDBRUSH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setForegroundBrush( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETFOREGROUNDBRUSH FP=( p )->setForegroundBrush( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItemIndexMethod ( ItemIndexMethod method )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETITEMINDEXMETHOD )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setItemIndexMethod( ( QGraphicsScene::ItemIndexMethod ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETITEMINDEXMETHOD FP=( p )->setItemIndexMethod( ( QGraphicsScene::ItemIndexMethod ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPalette ( const QPalette & palette )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETPALETTE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setPalette( *hbqt_par_QPalette( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETPALETTE FP=( p )->setPalette( *hbqt_par_QPalette( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSceneRect ( const QRectF & rect )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETSCENERECT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSceneRect( *hbqt_par_QRectF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETSCENERECT FP=( p )->setSceneRect( *hbqt_par_QRectF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSceneRect ( qreal x, qreal y, qreal w, qreal h )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETSCENERECT_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSceneRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETSCENERECT_1 FP=( p )->setSceneRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectionArea ( const QPainterPath & path )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETSELECTIONAREA )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSelectionArea( *hbqt_par_QPainterPath( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETSELECTIONAREA FP=( p )->setSelectionArea( *hbqt_par_QPainterPath( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectionArea ( const QPainterPath & path, Qt::ItemSelectionMode mode )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETSELECTIONAREA_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSelectionArea( *hbqt_par_QPainterPath( 2 ), ( Qt::ItemSelectionMode ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETSELECTIONAREA_1 FP=( p )->setSelectionArea( *hbqt_par_QPainterPath( 2 ), ( Qt::ItemSelectionMode ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSortCacheEnabled ( bool enabled )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETSORTCACHEENABLED )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSortCacheEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETSORTCACHEENABLED FP=( p )->setSortCacheEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStickyFocus ( bool enabled )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETSTICKYFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setStickyFocus( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETSTICKYFOCUS FP=( p )->setStickyFocus( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStyle ( QStyle * style )
 */
HB_FUNC( QT_QGRAPHICSSCENE_SETSTYLE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setStyle( hbqt_par_QStyle( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_SETSTYLE FP=( p )->setStyle( hbqt_par_QStyle( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool stickyFocus () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_STICKYFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retl( ( p )->stickyFocus() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_STICKYFOCUS FP=hb_retl( ( p )->stickyFocus() ); p is NULL" ) );
   }
}

/*
 * QStyle * style () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_STYLE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_STYLE FP=hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) ); p is NULL" ) );
   }
}

/*
 * void update ( qreal x, qreal y, qreal w, qreal h )
 */
HB_FUNC( QT_QGRAPHICSSCENE_UPDATE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->update( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_UPDATE FP=( p )->update( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsView *> views () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_VIEWS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsView *>( ( p )->views() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_VIEWS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsView *>( ( p )->views() ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QGRAPHICSSCENE_WIDTH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retnd( ( p )->width() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_WIDTH FP=hb_retnd( ( p )->width() ); p is NULL" ) );
   }
}

/*
 * void advance ()
 */
HB_FUNC( QT_QGRAPHICSSCENE_ADVANCE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->advance();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_ADVANCE FP=( p )->advance(); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QGRAPHICSSCENE_CLEAR )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * void clearSelection ()
 */
HB_FUNC( QT_QGRAPHICSSCENE_CLEARSELECTION )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->clearSelection();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_CLEARSELECTION FP=( p )->clearSelection(); p is NULL" ) );
   }
}

/*
 * void invalidate ( const QRectF & rect = QRectF(), SceneLayers layers = AllLayers )
 */
HB_FUNC( QT_QGRAPHICSSCENE_INVALIDATE_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->invalidate( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ), ( HB_ISNUM( 3 ) ? ( QGraphicsScene::SceneLayers ) hb_parni( 3 ) : ( QGraphicsScene::SceneLayers ) QGraphicsScene::AllLayers ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_INVALIDATE_1 FP=( p )->invalidate( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ), ( HB_ISNUM( 3 ) ? ( QGraphicsScene::SceneLayers ) hb_parni( 3 ) : ( QGraphicsScene::SceneLayers ) QGraphicsScene::AllLayers ) ); p is NULL" ) );
   }
}

/*
 * void update ( const QRectF & rect = QRectF() )
 */
HB_FUNC( QT_QGRAPHICSSCENE_UPDATE_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->update( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSSCENE_UPDATE_1 FP=( p )->update( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
