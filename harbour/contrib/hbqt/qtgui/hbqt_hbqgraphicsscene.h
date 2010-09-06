/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2010 Pritpal Bedi <bedipritpal@hotmail.com>
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

#ifndef HBQT_HBQGRAPHICSSCENE_H
#define HBQT_HBQGRAPHICSSCENE_H

#include <QtGui/QGraphicsScene>
#include <QtGui/QPrinter>
#include <QtGui/QGraphicsSceneMouseEvent>
#include <QtGui/QGraphicsLineItem>
#include <QtGui/QCursor>
#include <QtGui/QKeyEvent>
#include <QtGui/QGraphicsView>
#include <QtCore/QList>

#include "hbqt_hbqgraphicsitem.h"

#define RESIZE_MODE_FIXED                         0
#define RESIZE_MODE_LEFT                          1
#define RESIZE_MODE_TOP                           2
#define RESIZE_MODE_RIGHT                         4
#define RESIZE_MODE_BOTTOM                        8
#define RESIZE_MODE_FIXEDPOS                      16

#define hbqt_screen_heightMM (((double)QDesktopWidget().screen()->height() / (double)QDesktopWidget().screen()->physicalDpiY() )*25.4)
#define hbqt_screen_widthMM  (((double)QDesktopWidget().screen()->width()  / (double)QDesktopWidget().screen()->physicalDpiX() )*25.4)

class HBQGraphicsScene : public QGraphicsScene
{
   Q_OBJECT

public:
   enum Magnet
   {
      Left       = 1,
      Right      = 2,
      Top        = 4,
      Bottom     = 8,
      Vertical   = 16,
      Horizontal = 32
   };

public:
   HBQGraphicsScene( QObject * parent = 0 );
   ~HBQGraphicsScene();

   PHB_ITEM                 block;

   void                     hbSetBlock( PHB_ITEM b );

   virtual int              pageSize();
   virtual void             setPageSize( int pageSize );
   QRectF                   paperRect();
   void                     setPaperRect( QRectF paperRect );
   virtual int              orientation();
   virtual void             setOrientation( int orientation );
   virtual QRectF           geometry();
   virtual void             setGeometry( QRectF rect );
   int                      magnetArea();
   void                     setMagnetArea( int magnetArea );
   virtual bool             showGrid();
   virtual void             setShowGrid( bool showGrid );

public slots:
   virtual void             setLeftMagnet( bool magneted );
   virtual void             setRightMagnet( bool magneted );
   virtual void             setTopMagnet( bool magneted );
   virtual void             setBottomMagnet( bool magneted );
   virtual void             setHorizontalMagnet( bool magneted );
   virtual void             setVerticalMagnet( bool magneted );

protected:
   virtual void             contextMenuEvent( QGraphicsSceneContextMenuEvent * event );
   virtual void             mouseMoveEvent( QGraphicsSceneMouseEvent * mouseEvent );
   virtual void             mousePressEvent( QGraphicsSceneMouseEvent *event );
   virtual void             mouseReleaseEvent( QGraphicsSceneMouseEvent * mouseEvent );
   virtual void             keyPressEvent( QKeyEvent * keyEvent );
   virtual void             keyReleaseEvent( QKeyEvent * keyEvent );

   virtual void             dragEnterEvent( QGraphicsSceneDragDropEvent * event );
   virtual void             dragLeaveEvent( QGraphicsSceneDragDropEvent * event );
   virtual void             dragMoveEvent( QGraphicsSceneDragDropEvent * event );
   virtual void             dropEvent( QGraphicsSceneDragDropEvent * event );

   virtual void             updatePageRect();

signals:
   void                     itemSelected( QObject * thisObject, QPointF cursorPos );
   void                     geometryChanged( QRectF newGeometry );
   void                     itemMoved( QObject*, QPointF );

private:
   void                     drawMagnets( HBQGraphicsItem * item );
   void                     drawBorder();

private:
   int                      m_pageSize;
   int                      m_orientation;
   QRectF                   m_geometry,  m_paperRect;
   int                      m_magnets,   m_magnetArea;
   QList< QGraphicsItem * > m_gideLines;
   QGraphicsRectItem *      m_paperBorder;
   QGraphicsRectItem *      m_pageBorder;
   bool                     m_showGrid;
   QGraphicsItem *          movingItem;
   QPointF                  mouseOldPos;
};

#endif


