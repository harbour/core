/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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

#ifndef HBQT_HBQGRAPHICSRECTITEM_H
#define HBQT_HBQGRAPHICSRECTITEM_H

#include "hbqtgui.h"

#include <QtGui/QGraphicsRectItem>
#include <QtGui/QGraphicsScene>
#include <QtGui/QGraphicsSceneDragDropEvent>
#include <QtGui/QTreeWidget>
#include <QtGui/QDesktopWidget>
#include <QtCore/QModelIndex>
#include <QtCore/QEvent>
#include <QtCore/QMimeData>

#define RESIZE_MODE_NONE                          0
#define RESIZE_MODE_LEFT                          1
#define RESIZE_MODE_TOP                           2
#define RESIZE_MODE_RIGHT                         4
#define RESIZE_MODE_BOTTOM                        8

class HBQGraphicsRectItem : public QGraphicsRectItem
{

public:
   HBQGraphicsRectItem( QGraphicsItem * parent = 0 );
   HBQGraphicsRectItem( const QRectF & rect, QGraphicsItem * parent = 0 );
   HBQGraphicsRectItem( qreal x, qreal y, qreal width, qreal height, QGraphicsItem * parent = 0 );
   ~HBQGraphicsRectItem();

   PHB_ITEM       block;
   int            determineResizeMode( const QPointF & pos );

private:
   bool           bYes;
   int            resizeMode;
   QRectF         oGeometry;
   qreal          iWidth;
   qreal          iHeight;

protected:
   void           dragEnterEvent( QGraphicsSceneDragDropEvent * event );
   void           dragLeaveEvent( QGraphicsSceneDragDropEvent * event );
   void           dragMoveEvent( QGraphicsSceneDragDropEvent * event );
   void           dropEvent( QGraphicsSceneDragDropEvent * event );
   //
   void           mousePressEvent( QGraphicsSceneMouseEvent * event );
   void           mouseReleaseEvent( QGraphicsSceneMouseEvent * event );
   void           mouseMoveEvent( QGraphicsSceneMouseEvent * event );

   void           hoverEnterEvent( QGraphicsSceneHoverEvent * event );

public slots:
   void           hbSetBlock( PHB_ITEM block );
   QRectF         geometry();
   void           setGeometry( const QRectF & rect );
   qreal          width() const;
   void           setWidth( qreal width );
   qreal          height() const;
   void           setHeight( qreal height );

};

#endif
