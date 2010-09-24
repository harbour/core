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
 *  enum ShapeMode { MaskShape, BoundingRectShape, HeuristicMaskShape }
 */

/*
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsPixmapItem>


/*
 * QGraphicsPixmapItem ( QGraphicsItem * parent = 0 )
 * QGraphicsPixmapItem ( const QPixmap & pixmap, QGraphicsItem * parent = 0 )
 * ~QGraphicsPixmapItem ()
 */

typedef struct
{
   QGraphicsPixmapItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsPixmapItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsPixmapItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QGraphicsPixmapItem   /.\\", p->ph ) );
         delete ( ( QGraphicsPixmapItem * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QGraphicsPixmapItem   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGraphicsPixmapItem    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGraphicsPixmapItem    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsPixmapItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsPixmapItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsPixmapItem;
   p->type = HBQT_TYPE_QGraphicsPixmapItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsPixmapItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsPixmapItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSPIXMAPITEM )
{
   QGraphicsPixmapItem * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QPixmap )
      {
         pObj = new QGraphicsPixmapItem( *hbqt_par_QPixmap( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsPixmapItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsPixmapItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsPixmapItem( ( void * ) pObj, true ) );
}

/*
 * QPointF offset () const
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_OFFSET )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->offset() ), true ) );
   }
}

/*
 * QPixmap pixmap () const
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_PIXMAP )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
   }
}

/*
 * void setOffset ( const QPointF & offset )
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETOFFSET )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      ( p )->setOffset( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setOffset ( qreal x, qreal y )
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETOFFSET_1 )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      ( p )->setOffset( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setPixmap ( const QPixmap & pixmap )
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETPIXMAP )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ) );
   }
}

/*
 * void setShapeMode ( ShapeMode mode )
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETSHAPEMODE )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      ( p )->setShapeMode( ( QGraphicsPixmapItem::ShapeMode ) hb_parni( 2 ) );
   }
}

/*
 * void setTransformationMode ( Qt::TransformationMode mode )
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETTRANSFORMATIONMODE )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      ( p )->setTransformationMode( ( Qt::TransformationMode ) hb_parni( 2 ) );
   }
}

/*
 * ShapeMode shapeMode () const
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SHAPEMODE )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      hb_retni( ( QGraphicsPixmapItem::ShapeMode ) ( p )->shapeMode() );
   }
}

/*
 * Qt::TransformationMode transformationMode () const
 */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_TRANSFORMATIONMODE )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
   {
      hb_retni( ( Qt::TransformationMode ) ( p )->transformationMode() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
