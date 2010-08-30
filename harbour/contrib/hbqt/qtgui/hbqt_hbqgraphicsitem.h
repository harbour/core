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

#ifndef HBQT_HBQGRAPHICSITEM_H
#define HBQT_HBQGRAPHICSITEM_H

#include "hbqtgui.h"

#include <QtGui/QGraphicsItem>
#include <QtGui/QStyleOptionGraphicsItem>
#include <QtGui/QGraphicsScene>
#include <QtGui/QGraphicsSceneMouseEvent>
#include <QtGui/QPainter>
#include <QtGui/QWidget>
#include <QtGui/QTreeWidget>
#include <QtGui/QDesktopWidget>
#include <QtCore/QModelIndex>
#include <QtCore/QEvent>
#include <QtCore/QMimeData>

#define UNIT                                      0.1

#define RESIZE_MODE_FIXED                         0
#define RESIZE_MODE_LEFT                          1
#define RESIZE_MODE_TOP                           2
#define RESIZE_MODE_RIGHT                         4
#define RESIZE_MODE_BOTTOM                        8
#define RESIZE_MODE_FIXEDPOS                      16

#define HBQT_GRAPHICSITEM_NONE                    0
#define HBQT_GRAPHICSITEM_RECT                    1
#define HBQT_GRAPHICSITEM_LINE                    2
#define HBQT_GRAPHICSITEM_ELLIPSE                 3
#define HBQT_GRAPHICSITEM_ARC                     4
#define HBQT_GRAPHICSITEM_CHORD                   5
#define HBQT_GRAPHICSITEM_POLYGON                 6
#define HBQT_GRAPHICSITEM_PIE                     7
#define HBQT_GRAPHICSITEM_PATH                    8
#define HBQT_GRAPHICSITEM_CHART                   9
#define HBQT_GRAPHICSITEM_GRADIENT                10
#define HBQT_GRAPHICSITEM_PICTURE                 11
#define HBQT_GRAPHICSITEM_BARCODE                 12
#define HBQT_GRAPHICSITEM_TEXT                    13
#define HBQT_GRAPHICSITEM_SIMPLETEXT              14

#define HBQT_GRAPHICSITEM_LINE_HORIZONTAL         0
#define HBQT_GRAPHICSITEM_LINE_VERTICAL           1
#define HBQT_GRAPHICSITEM_LINE_BACKWARDDIAGONAL   2
#define HBQT_GRAPHICSITEM_LINE_FORWARDDIAGONAL    3

#define HBQT_GRAPHICSITEM_TEXT_DRAW_NONE          0
#define HBQT_GRAPHICSITEM_TEXT_DRAW_TOP           1
#define HBQT_GRAPHICSITEM_TEXT_DRAW_BOTTOM        2
#define HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE         3
#define HBQT_GRAPHICSITEM_TEXT_DRAW_BELOW         4

#define HBQT_GRAPHICSITEM_TEXT_SIZEPOLICY_NONE    0
#define HBQT_GRAPHICSITEM_TEXT_SIZEPOLICY_AUTO    1
#define HBQT_GRAPHICSITEM_TEXT_SIZEPOLICY_STRETCH 2

#define HBQT_GRAPHICSITEM_IMAGE_NO_FRAME          0
#define HBQT_GRAPHICSITEM_IMAGE_PICTURE_BIND      1
#define HBQT_GRAPHICSITEM_IMAGE_PICTURE_BOX       2

#define HBQT_GRAPHICSITEM_RESIZE_ITEM_TO_PICTURE  1
#define HBQT_GRAPHICSITEM_CENTER_PICTURE_TO_ITEM  2
#define HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_KEEP_ASPECT_RATIO     3
#define HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_IGNORE_ASPECT_RATIO   4

#define hbqt_screen_heightMM (((double)QDesktopWidget().screen()->height() / (double)QDesktopWidget().screen()->physicalDpiY() )*25.4)
#define hbqt_screen_widthMM  (((double)QDesktopWidget().screen()->width()  / (double)QDesktopWidget().screen()->physicalDpiX() )*25.4)


class HBQGraphicsItem : public QGraphicsItem
{

public:
   HBQGraphicsItem( int type = 0, QGraphicsItem * parent = 0 );
   ~HBQGraphicsItem();

   QRectF         boundingRect() const;
   virtual void   paint( QPainter * painter, const QStyleOptionGraphicsItem * option, QWidget * widget = 0 );

   PHB_ITEM       block;

   int            determineResizeMode( const QPointF & pos );
   QRectF         adjustRect( QRectF & rect );
   virtual void   prepare( QPainter * painter );
   void           drawSelection( QPainter * painter, const QRectF & rect );
   void           setupPainter( QPainter * painter );

private:
   int            iType;
   bool           bYes;

   QBrush         QBrush_brush, QBrush_bgBrush;
   QPen           QPen_pen;
   QFont          QFont_font;
   qreal          dWidth, dHeight;
   QRectF         QRectF_geometry;
   int            iOpacity;
   int            iResizeMode;
   int            iResizeFlags;
   bool           bDrawSelectionBorder;
   int            iResizeHandle;
   int            iBGMode;
   int            iLineStyle;
   int            iStartAngle;
   int            iSpanAngle;
   QString        QString_objectType;
   QString        QString_objectName;
   QString        QString_text;

   /* Image */
   int            m_paintType;
   int            m_frameType;
   int            m_drawTextType;
   QImage         m_image;
   QColor         m_textColor;
   QColor         m_borderColor;
   int            m_borderWidth;
   int            m_sizePolicy;
   int            m_textFlags;

   int            m_barsIdentation;
   bool           m_showLabels;
   qreal          m_toColorFactor;
   bool           m_drawBorder;
   bool           m_showGrid;
   QStringList    m_barValues;
   int            m_legendColorRectWidth;

   QColor         generateNextColor();
   QRectF         adjustOption( QPainter * painter, const QStyleOptionGraphicsItem * option );
   //
   void           drawRect( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawEllipse( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawLine( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawPie( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawArc( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawChord( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawPicture( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawText( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawBarChart( QPainter * painter, const QStyleOptionGraphicsItem * option );
   void           drawBarcode39( QPainter * painter, const QStyleOptionGraphicsItem * option );

protected:
   void           dragEnterEvent( QGraphicsSceneDragDropEvent * event );
   void           dragLeaveEvent( QGraphicsSceneDragDropEvent * event );
   void           dragMoveEvent( QGraphicsSceneDragDropEvent * event );
   void           dropEvent( QGraphicsSceneDragDropEvent * event );

   void           hoverEnterEvent( QGraphicsSceneHoverEvent * event );

   void           mousePressEvent( QGraphicsSceneMouseEvent * event );
   void           mouseReleaseEvent( QGraphicsSceneMouseEvent * event );
   void           mouseMoveEvent( QGraphicsSceneMouseEvent * event );
   void           contextMenuEvent( QGraphicsSceneContextMenuEvent * event );


public slots:
   void           hbSetBlock( PHB_ITEM block );

   QPen           pen();
   void           setPen( const QPen & pen );
   QBrush         brush();
   void           setBrush( const QBrush & brush );
   QBrush         backgroundBrush();
   void           setBackgroundBrush( const QBrush & brush );
   QFont          font();
   void           setFont( const QFont & font );
   int            lineStyle();
   void           setLineStyle( int lineStyle );
   int            startAngle();
   void           setStartAngle( int startAngle );
   int            spanAngle();
   void           setSpanAngle( int spanAngle );
   qreal          width() const;
   void           setWidth( qreal width );
   qreal          height() const;
   void           setHeight( qreal height );
   int            opacity();
   void           setOpacity( const int opacity );
   QRectF         geometry();
   void           setGeometry( const QRectF & rect );
   QString        objectType();
   void           setObjectType( const QString & type );
   QString        objectName();
   void           setObjectName( const QString & name );
   QString        text();
   void           setText( const QString & text );

   int            paintType();
   void           setPaintType( int paintType );
   int            frameType();
   void           setFrameType( int frameType );
   int            drawTextType();
   void           setDrawTextType( int drawTextType );
   QPixmap        pixmap();
   void           setPixmap( const QPixmap & pixmap );
   QColor         textColor();
   void           setTextColor( const QColor & color );
   int            borderWidth();
   void           setBorderWidth( int bWidth );
   QColor         borderColor();
   void           setBorderColor( const QColor & color );
   int            sizePolicy();
   void           setSizePolicy( int sizePolicy );
   int            textFlags();
   void           setTextFlags( int textFlags );
   int            resizeFlags();
   void           setResizeFlags( int resizeFlags );
   int            resizeHandle();
   void           setResizeHandle( int resizeHandle );

   int            barsIdentation();
   void           setBarsIdentation( int barsIdentation );
   bool           drawBorder();
   void           setDrawBorder( bool drawBorder );
   bool           showGrid();
   void           setShowGrid( bool showGrid );
   bool           showLabels();
   void           setShowLabels( bool showLabels );
   qreal          toColorFactor();
   void           setToColorFactor( qreal toColorFactor );
   void           setBarValues( const QStringList & list );
   void           setLegendColorRectWidth( int legendColorRectWidth );

   struct _chartValue
   {
      QString key;
      qreal value;
      QColor color;
   };
};

#endif
