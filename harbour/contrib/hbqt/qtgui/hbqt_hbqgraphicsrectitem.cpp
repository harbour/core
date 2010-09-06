/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbqt.h"
#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqgraphicsrectitem.h"

HBQGraphicsRectItem::HBQGraphicsRectItem( QGraphicsItem * parent ) : QGraphicsRectItem( parent )
{
   block = NULL;
   resizeMode = RESIZE_MODE_NONE;
}

HBQGraphicsRectItem::HBQGraphicsRectItem( const QRectF & rect, QGraphicsItem * parent ) : QGraphicsRectItem( rect, parent )
{
   block = NULL;
   resizeMode = RESIZE_MODE_NONE;
}

HBQGraphicsRectItem::HBQGraphicsRectItem( qreal x, qreal y, qreal width, qreal height, QGraphicsItem * parent  ) : QGraphicsRectItem( x, y, width, height, parent )
{
   block = NULL;
   resizeMode = RESIZE_MODE_NONE;
}

HBQGraphicsRectItem::~HBQGraphicsRectItem()
{
   if( block )
   {
      hb_itemRelease( block );
      block = NULL;
   }
}

void HBQGraphicsRectItem::hbSetBlock( PHB_ITEM b )
{
   if( b )
   {
      block = hb_itemNew( b );

      QDesktopWidget * qWid = new QDesktopWidget();

      PHB_ITEM p1 = hb_itemPutNI( NULL, 21001 );
      PHB_ITEM p2 = hb_itemPutNI( NULL, qWid->screen()->physicalDpiX() );
      PHB_ITEM p3 = hb_itemPutNI( NULL, qWid->screen()->physicalDpiY() );
      hb_vmEvalBlockV( block, 3, p1, p2, p3 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      hb_itemRelease( p3 );
   }
}

void HBQGraphicsRectItem::dragEnterEvent( QGraphicsSceneDragDropEvent * event )
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, ( int ) QEvent::GraphicsSceneDragEnter );
      PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
   }
   QGraphicsItem::dragEnterEvent( event );
}
void HBQGraphicsRectItem::dragLeaveEvent( QGraphicsSceneDragDropEvent * event )
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, ( int ) QEvent::GraphicsSceneDragLeave );
      PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
   }
   QGraphicsItem::dragLeaveEvent( event );
}
void HBQGraphicsRectItem::dragMoveEvent( QGraphicsSceneDragDropEvent * event )
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, ( int ) QEvent::GraphicsSceneDragMove );
      PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
   }
   QGraphicsItem::dragMoveEvent( event );
}
void HBQGraphicsRectItem::dropEvent( QGraphicsSceneDragDropEvent * event )
{
   if( block )
   {
      const QMimeData * mime = event->mimeData();

      if( mime->hasFormat( ( QString ) "application/x-qabstractitemmodeldatalist" ) )
      {
         PHB_ITEM p1 = hb_itemPutNI( NULL, ( int ) QEvent::GraphicsSceneDrop );
         PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
         PHB_ITEM p3 = hb_itemNew( NULL );

         QTreeWidget * tree = dynamic_cast< QTreeWidget * >( event->source() );

         QByteArray encoded = mime->data( "application/x-qabstractitemmodeldatalist" );
         QDataStream stream( &encoded, QIODevice::ReadOnly );

         int row, col;
         QMap< int, QVariant > roleDataMap;
         stream >> row >> col >> roleDataMap;

         QTreeWidgetItem * item = tree->topLevelItem( row );

         hb_arrayNew( p3, 3 );
         //
         hb_arraySetC( p3, 1, tree->objectName().toLatin1().data() );
         hb_arraySetC( p3, 2, roleDataMap.value( Qt::DisplayRole ).toString().toLatin1().data() );
         hb_arraySetC( p3, 3, item->text( 0 ).toLatin1().data() );
         //
         hb_vmEvalBlockV( block, 3, p1, p2, p3 );
         //
         hb_itemRelease( p1 );
         hb_itemRelease( p2 );
         hb_itemRelease( p3 );
      }
      else
      {
         PHB_ITEM p1 = hb_itemPutNI( NULL, ( int ) QEvent::GraphicsSceneDrop );
         PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
         hb_vmEvalBlockV( block, 2, p1, p2 );
         hb_itemRelease( p1 );
         hb_itemRelease( p2 );
      }
   }
   QGraphicsItem::dropEvent( event );
}

void HBQGraphicsRectItem::mousePressEvent( QGraphicsSceneMouseEvent * event )
{
   oGeometry = geometry();

   foreach( QGraphicsItem * item, scene()->items() )
   {
      if( item->zValue() == 1 ){
         item->setZValue( 0 );
      }
   }
   setZValue( 1 );

   if( event->buttons() == Qt::LeftButton ){
      resizeMode = determineResizeMode( event->pos() );
   }
   else {
      resizeMode = RESIZE_MODE_NONE;
   }

   if( resizeMode == RESIZE_MODE_NONE ){
      setCursor( QCursor( Qt::ClosedHandCursor ) );
   }

   QGraphicsItem::mousePressEvent( event );

   if( event->buttons() == Qt::LeftButton ){
      // emit( itemSelected( this, event->pos() ) );
   }
}

void HBQGraphicsRectItem::mouseReleaseEvent( QGraphicsSceneMouseEvent * event )
{
   QGraphicsItem::mouseReleaseEvent( event );
   resizeMode = RESIZE_MODE_NONE;

   QRectF nGeometry = geometry();
   if( nGeometry != oGeometry ){
      // emit( geometryChanged( this, nGeometry, oGeometry ) );
   }
}

void HBQGraphicsRectItem::mouseMoveEvent( QGraphicsSceneMouseEvent * event )
{
   if( event->buttons() == Qt::LeftButton )
   {
      if( resizeMode == RESIZE_MODE_NONE ){
         setPos( pos() + QPoint( ( int ) ( event->scenePos().x() - event->lastScenePos().x() ),
                                 ( int ) ( event->scenePos().y() - event->lastScenePos().y() ) ) );
      }
      else
      {
         if( resizeMode & RESIZE_MODE_LEFT ){
            setPos( pos().x() + event->scenePos().x() - event->lastScenePos().x(), pos().y() );
            setWidth( width() + event->lastScenePos().x() - event->scenePos().x() );
         }
         if( resizeMode & RESIZE_MODE_TOP ){
            setPos( pos().x(), pos().y() + event->scenePos().y() - event->lastScenePos().y() );
            setHeight( height() + event->lastScenePos().y() - event->scenePos().y() );
         }
         if( resizeMode & RESIZE_MODE_RIGHT ){
            setWidth( ( int ) ( width() + event->scenePos().x() - event->lastScenePos().x() ) );
         }
         if( resizeMode & RESIZE_MODE_BOTTOM ){
            setHeight( ( int ) ( height() + event->scenePos().y() - event->lastScenePos().y() ) );
         }
         if( width() < 5 ){
            setWidth( 5 );
         }
         if( height() < 5 ){
            setHeight( 5 );
         }
      }
   }
   QGraphicsItem::mouseMoveEvent( event );
}

void HBQGraphicsRectItem::hoverEnterEvent( QGraphicsSceneHoverEvent * event )
{
HB_TRACE( HB_TR_ALWAYS, ( "hoverEnter" ) );
   determineResizeMode( event->pos() );
   QGraphicsItem::hoverEnterEvent( event );
}

int HBQGraphicsRectItem::determineResizeMode( const QPointF & pos )
{
   int resizeModes = RESIZE_MODE_LEFT | RESIZE_MODE_TOP | RESIZE_MODE_RIGHT | RESIZE_MODE_BOTTOM ;
   int mode = RESIZE_MODE_NONE;

   QRectF topRect( 0, 0, width(), 2 );
   QRectF leftRect( 0, 0, 2, height() );
   QRectF bottomRect( 0, height() - 2, width(), 2 );
   QRectF rightRect( width() - 2, 0, width(), height() );

   if( resizeModes & RESIZE_MODE_LEFT && leftRect.contains( pos ) ){
      mode |= RESIZE_MODE_LEFT;
      setCursor( QCursor( Qt::SizeHorCursor ) );
   }
   if( resizeModes & RESIZE_MODE_TOP && topRect.contains( pos ) ){
      mode |= RESIZE_MODE_TOP;
   }
   if( resizeModes & RESIZE_MODE_RIGHT && rightRect.contains( pos ) ){
      mode |= RESIZE_MODE_RIGHT;
   }
   if( resizeModes & RESIZE_MODE_BOTTOM && bottomRect.contains( pos ) ){
      mode |= RESIZE_MODE_BOTTOM;
   }
   return mode;
}

QRectF HBQGraphicsRectItem::geometry()
{
   return QRectF( pos().x(), pos().y(), width(), height() );
}

void HBQGraphicsRectItem::setGeometry( const QRectF & rect )
{
   setPos( rect.x(), rect.y() );
   setWidth( rect.width() );
   setHeight( rect.height() );
}

void HBQGraphicsRectItem::setWidth( qreal width )
{
   // prepareGeometryChange();
   iWidth = width;
   //emit( geometryChanged( geometry() ) );
}

void HBQGraphicsRectItem::setHeight( qreal height )
{
   // prepareGeometryChange();
   iHeight = height;
   //emit( geometryChanged( geometry() ) );
}

qreal HBQGraphicsRectItem::width() const
{
   return iWidth;
}

qreal HBQGraphicsRectItem::height() const
{
   return iHeight;
}

#endif
