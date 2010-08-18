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
}

HBQGraphicsRectItem::HBQGraphicsRectItem( const QRectF & rect, QGraphicsItem * parent ) : QGraphicsRectItem( rect, parent )
{
   block = NULL;
}

HBQGraphicsRectItem::HBQGraphicsRectItem( qreal x, qreal y, qreal width, qreal height, QGraphicsItem * parent  ) : QGraphicsRectItem( x, y, width, height, parent )
{
   block = NULL;
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

#endif
