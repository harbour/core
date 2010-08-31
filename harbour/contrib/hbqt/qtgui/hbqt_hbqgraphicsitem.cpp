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
 /***************************************************************************
 *   The parts of this source are borrowed and adopted from eXaro project   *
 *                 Copyright (C) 2008 by BogDan Vatra                       *
 *                         bog_dan_ro@yahoo.com                             *
 ***************************************************************************/

#include "hbqt.h"
#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqgraphicsitem.h"

#include <math.h>

HBQGraphicsItem::HBQGraphicsItem( int type, QGraphicsItem * parent ) : QGraphicsItem( parent )
{
   iType = type;

   block = NULL;

   iResizeMode          = RESIZE_MODE_FIXED;
   iResizeFlags         = RESIZE_MODE_LEFT | RESIZE_MODE_TOP | RESIZE_MODE_RIGHT | RESIZE_MODE_BOTTOM;
   dWidth               = 20 / UNIT; // 20 mm
   dHeight              = 20 / UNIT; // 20 mm
   iOpacity             = 100;
   iResizeHandle        = 2 / UNIT;
   iBGMode              = Qt::TransparentMode;
   iLineStyle           = HBQT_GRAPHICSITEM_LINE_HORIZONTAL;
   iStartAngle          = 30;
   iSpanAngle           = 120;
   QString_objectType   = "";
   QString_text         = iType == HBQT_GRAPHICSITEM_PICTURE ? "picture" : "";

   bDrawSelectionBorder = true;

   QBrush_brush         = iType == HBQT_GRAPHICSITEM_PICTURE ? QBrush( QPixmap( ":/empty.png" ) ) : QBrush();
   QBrush_bgBrush       = QBrush();
   QPen_pen             = QPen( Qt::SolidLine );

   QFont_font           = QFont( "Serif" );
   QFont_font.setPointSizeF( 3.5 );
   QFont_font.setStyleStrategy( QFont::PreferMatch );
   QFont_font.setStyleStrategy( QFont::ForceOutline );
   setFont( QFont_font );

   setFlags( QGraphicsItem::ItemIsSelectable );

   /* Picture */
   m_textFlags          = Qt::AlignCenter;

   m_paintType          = HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_KEEP_ASPECT_RATIO;
   m_frameType          = HBQT_GRAPHICSITEM_IMAGE_NO_FRAME;
   m_textColor          = Qt::black;
   m_borderColor        = Qt::black;
   m_borderWidth        = 0;
   m_drawTextType       = HBQT_GRAPHICSITEM_TEXT_DRAW_NONE;

   m_barsIdentation     = 1 / UNIT;
   m_showLabels         = true;
   m_toColorFactor      = 2.0;
   m_drawBorder         = true;
   m_showGrid           = true;
   m_legendColorRectWidth = 5 / UNIT;

}

HBQGraphicsItem::~HBQGraphicsItem()
{
   if( block ){
      hb_itemRelease( block );
      block = NULL;
   }
}

void HBQGraphicsItem::hbSetBlock( PHB_ITEM b )
{
   if( b ){
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

QString HBQGraphicsItem::objectType()
{
   return QString_objectType;
}
void  HBQGraphicsItem::setObjectType( const QString & type )
{
   QString_objectType = type;
}

QString HBQGraphicsItem::objectName()
{
   return QString_objectName;
}
void  HBQGraphicsItem::setObjectName( const QString & name )
{
   QString_objectName = name;
}

QBrush HBQGraphicsItem::brush()
{
   return QBrush_brush;
}
void HBQGraphicsItem::setBrush( const QBrush & brush )
{
   QBrush_brush = brush;
   update();
}

QBrush HBQGraphicsItem::backgroundBrush()
{
   return QBrush_bgBrush;
}
void HBQGraphicsItem::setBackgroundBrush( const QBrush & brush )
{
   QBrush_bgBrush = brush;
   update();
}

QPen HBQGraphicsItem::pen()
{
   return QPen_pen;
}
void HBQGraphicsItem::setPen( const QPen & pen )
{
   QPen_pen = pen;
   update();
}

QFont HBQGraphicsItem::font()
{
   return QFont_font;
}
void HBQGraphicsItem::setFont( const QFont & font )
{
   QFont_font = font;
   update();
}

/*        TEXT     */
QString HBQGraphicsItem::text()
{
   return QString_text;
}
void  HBQGraphicsItem::setText( const QString & text )
{
   QString_text = text;
   update();
}
int HBQGraphicsItem::sizePolicy()
{
   return m_sizePolicy;
}
void HBQGraphicsItem::setSizePolicy( int sizePolicy )
{
   m_sizePolicy = sizePolicy;
}
int HBQGraphicsItem::textFlags()
{
   return m_textFlags;
}
void HBQGraphicsItem::setTextFlags( int textFlags )
{
   m_textFlags = textFlags;
   update();
}

/*        LINE     */
int HBQGraphicsItem::lineStyle()
{
   return iLineStyle;
}
void HBQGraphicsItem::setLineStyle( int lineStyle )
{
   iLineStyle = lineStyle;
   update();
}

/*        PIE       */
int HBQGraphicsItem::startAngle()
{
   return iStartAngle;
}
void HBQGraphicsItem::setStartAngle( int startAngle )
{
   iStartAngle = startAngle;
   update();
}
int HBQGraphicsItem::spanAngle()
{
   return iSpanAngle;
}
void HBQGraphicsItem::setSpanAngle( int spanAngle )
{
   iSpanAngle = spanAngle;
   update();
}

/*        GEOMETRY      */
qreal HBQGraphicsItem::width() const
{
   return dWidth;
}
void HBQGraphicsItem::setWidth( qreal width )
{
   dWidth = width;
   update();
}
qreal HBQGraphicsItem::height() const
{
   return dHeight;
}
void HBQGraphicsItem::setHeight( qreal height )
{
   dHeight = height;
   update();
}
QRectF HBQGraphicsItem::geometry()
{
   return QRectF( pos().x(), pos().y(), width(), height() );
}
void HBQGraphicsItem::setGeometry( const QRectF & rect )
{
   setPos( rect.x(), rect.y() );
   setWidth( rect.width() );
   setHeight( rect.height() );
}

/*       OPACITY      */
int HBQGraphicsItem::opacity()
{
   return iOpacity;
}
void HBQGraphicsItem::setOpacity( const int opacity )
{
   if( opacity < 0 ){
      iOpacity = 0;
   }
   else {
      if( opacity > 100 ){
         iOpacity = 100;
      }
      else {
         iOpacity = opacity;
      }
   }
   update();
}

int HBQGraphicsItem::paintType()
{
   return m_paintType;
}
void HBQGraphicsItem::setPaintType( int paintType )
{
   m_paintType = paintType;
   update();
}
int HBQGraphicsItem::frameType()
{
   return m_frameType;
}
void HBQGraphicsItem::setFrameType( int frameType )
{
   m_frameType = frameType;
   update();
}
int HBQGraphicsItem::drawTextType()
{
   return m_drawTextType;
}
void HBQGraphicsItem::setDrawTextType( int drawTextType )
{
   m_drawTextType = drawTextType;
   update();
}
QPixmap HBQGraphicsItem::pixmap()
{
   QPixmap pixmap;
   return pixmap.fromImage( m_image ) ;
}
void HBQGraphicsItem::setPixmap( const QPixmap & pixmap )
{
   m_image = QImage( pixmap.toImage() );
   update();
}
QColor HBQGraphicsItem::textColor()
{
   return m_textColor;
}
void HBQGraphicsItem::setTextColor( const QColor & textColor )
{
   m_textColor = textColor;
   update();
}
int HBQGraphicsItem::borderWidth()
{
   return m_borderWidth;
}
void HBQGraphicsItem::setBorderWidth( int borderWidth )
{
   if( borderWidth < 0 ){
      m_borderWidth = 0;
   }
   else {
      if( borderWidth > 5 ){
         m_borderWidth = 5;
      }
      else {
         m_borderWidth = borderWidth;
      }
   }
   update();
}
QColor HBQGraphicsItem::borderColor()
{
   return m_borderColor;
}
void HBQGraphicsItem::setBorderColor( const QColor & borderColor )
{
   m_borderColor = borderColor;
   update();
}

int HBQGraphicsItem::resizeFlags()
{
   return iResizeFlags;
}
void HBQGraphicsItem::setResizeFlags( int resizeFlags )
{
   iResizeFlags = resizeFlags;
}

int HBQGraphicsItem::resizeHandle()
{
   return iResizeHandle;
}
void HBQGraphicsItem::setResizeHandle( int resizeHandle )
{
   iResizeHandle = resizeHandle;
#if 0
   if( m_minWidth < iResizeHandle * 2 + 1 )
      m_minWidth = iResizeHandle * 2 + 1;

   if( m_minHeight < iResizeHandle * 2 + 1 )
      m_minHeight = iResizeHandle * 2 + 1;
#endif
   update( boundingRect() );
}

int HBQGraphicsItem::barsIdentation()
{
   return m_barsIdentation;
}
void HBQGraphicsItem::setBarsIdentation(int barsIdentation)
{
   if( barsIdentation < 1 )
      barsIdentation = 1;
   m_barsIdentation = barsIdentation;
   update();
}

bool HBQGraphicsItem::showLabels()
{
   return m_showLabels;
}
void HBQGraphicsItem::setShowLabels( bool showLabels )
{
   m_showLabels = showLabels;
   update();
}

bool HBQGraphicsItem::showGrid()
{
   return m_showGrid;
}
void HBQGraphicsItem::setShowGrid( bool showGrid )
{
   m_showGrid = showGrid;
   update();
}

qreal HBQGraphicsItem::toColorFactor()
{
   return m_toColorFactor;
}
void HBQGraphicsItem::setToColorFactor( qreal toColorFactor )
{
   if( toColorFactor > 10 )
      toColorFactor = 10;
   if( toColorFactor < 0.1 )
      toColorFactor = 0.1;
   m_toColorFactor = toColorFactor;
   update();
}

bool HBQGraphicsItem::drawBorder()
{
   return m_drawBorder;
}
void HBQGraphicsItem::setDrawBorder( bool drawBorder )
{
   m_drawBorder = drawBorder;
   update();
}

QColor HBQGraphicsItem::generateNextColor()
{
   return QColor( qrand() % 255, qrand() % 255, qrand() % 255, 255 );
}

void HBQGraphicsItem::setBarValues( const QStringList & barValues )
{
   m_barValues = barValues;
}

void HBQGraphicsItem::setLegendColorRectWidth( int legendColorRectWidth )
{
   if( legendColorRectWidth < 1 )
      legendColorRectWidth = 1;
   m_legendColorRectWidth = legendColorRectWidth;
   update();
}

/*----------------------------------------------------------------------*/
//                            Mouse Events
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::contextMenuEvent( QGraphicsSceneContextMenuEvent * event )
{
   if( block ){
      PHB_ITEM p1 = hb_itemPutNI( NULL, QEvent::GraphicsSceneContextMenu );
      PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
      PHB_ITEM p3 = hb_itemPutC( NULL, objectName().toLatin1().data() );
      hb_vmEvalBlockV( block, 3, p1, p2, p3 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      hb_itemRelease( p3 );
   }
   QGraphicsItem::contextMenuEvent( event );
}

void HBQGraphicsItem::mousePressEvent( QGraphicsSceneMouseEvent * event )
{
   QRectF_geometry = geometry();
#if 0  /* Control via user interaction - bring to front - push to back */
   foreach( QGraphicsItem * item, scene()->items() )
   {
      if( item->zValue() == 1 ){
         item->setZValue( 0 );
      }
   }
   setZValue( 1 );
   if( objectType() == ( QString ) "Page" ){
      setZValue( 0 );
   }
#endif

   if( event->buttons() == Qt::LeftButton ){
      iResizeMode = determineResizeMode( event->pos() );
   }
   else {
      iResizeMode = RESIZE_MODE_FIXED;
   }

   if( iResizeMode == RESIZE_MODE_FIXED ){
      setCursor( QCursor( Qt::ClosedHandCursor ) );
   }
   if( objectType() == ( QString ) "Page" ){
      setCursor( QCursor( Qt::ArrowCursor ) );
   }

   QGraphicsItem::mousePressEvent( event );

   if( event->buttons() == Qt::LeftButton ){
      // emit( itemSelected( this, event->pos() ) );
      if( block ){
         PHB_ITEM p1 = hb_itemPutNI( NULL, 21101 );
         PHB_ITEM p2 = hb_itemPutC( NULL, objectName().toLatin1().data() );
         hb_vmEvalBlockV( block, 2, p1, p2 );
         hb_itemRelease( p1 );
         hb_itemRelease( p2 );
      }
   }
}

void HBQGraphicsItem::mouseReleaseEvent( QGraphicsSceneMouseEvent * event )
{
   QGraphicsItem::mouseReleaseEvent( event );
   iResizeMode = RESIZE_MODE_FIXED;

   QRectF nGeometry = geometry();
   if( nGeometry != QRectF_geometry ){
      // emit( geometryChanged( this, nGeometry, QRectF_geometry ) );
      if( block ){
         // Inform geometry is changed
      }
   }
}

void HBQGraphicsItem::mouseMoveEvent( QGraphicsSceneMouseEvent * event )
{
   if( event->buttons() == Qt::LeftButton )
   {
      if( iResizeMode == RESIZE_MODE_FIXED ){
         setPos( pos() + QPoint( ( int ) ( event->scenePos().x() - event->lastScenePos().x() ),
                                 ( int ) ( event->scenePos().y() - event->lastScenePos().y() ) ) );
      }
      else
      {
         if( iResizeMode & RESIZE_MODE_LEFT ){
            setPos( pos().x() + event->scenePos().x() - event->lastScenePos().x(), pos().y() );
            setWidth( width() + event->lastScenePos().x() - event->scenePos().x() );
         }
         if( iResizeMode & RESIZE_MODE_TOP ){
            setPos( pos().x(), pos().y() + event->scenePos().y() - event->lastScenePos().y() );
            setHeight( height() + event->lastScenePos().y() - event->scenePos().y() );
         }
         if( iResizeMode & RESIZE_MODE_RIGHT ){
            scene()->invalidate( geometry() );
            setWidth( ( int ) ( width() + event->scenePos().x() - event->lastScenePos().x() ) );
         }
         if( iResizeMode & RESIZE_MODE_BOTTOM ){
            scene()->invalidate( geometry() );
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
   else
   {
      QGraphicsItem::mouseMoveEvent( event );
   }
}

void HBQGraphicsItem::hoverEnterEvent( QGraphicsSceneHoverEvent * event )
{
   QGraphicsItem::hoverEnterEvent( event );
}

int HBQGraphicsItem::determineResizeMode( const QPointF & pos )
{
   int resizeModes = resizeFlags();
   int mode = RESIZE_MODE_FIXED;

   QRectF topRect( 0, 0, width(), iResizeHandle );
   QRectF leftRect( 0, 0, iResizeHandle, height() );
   QRectF bottomRect( 0, height() - iResizeHandle, width(), iResizeHandle );
   QRectF rightRect( width() - iResizeHandle, 0, width(), height() );

   if( resizeModes & RESIZE_MODE_LEFT && leftRect.contains( pos ) ){
      mode |= RESIZE_MODE_LEFT;
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
   if( resizeModes & RESIZE_MODE_FIXEDPOS ){
      mode |= RESIZE_MODE_FIXEDPOS;
   }

   return mode;
}
/*----------------------------------------------------------------------*/
//                             Drag Events
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::dragEnterEvent( QGraphicsSceneDragDropEvent * event )
{
HB_TRACE( HB_TR_ALWAYS, ( "dragEnterEvent( QGraphicsSceneDragDropEvent * event )" ) );
   if( block )
   {
      #if 0
      PHB_ITEM p1 = hb_itemPutNI( NULL, ( int ) QEvent::GraphicsSceneDragEnter );
      PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      #endif
   }
   QGraphicsItem::dragEnterEvent( event );
}
void HBQGraphicsItem::dragLeaveEvent( QGraphicsSceneDragDropEvent * event )
{
   if( block )
   {
      #if 0
      PHB_ITEM p1 = hb_itemPutNI( NULL, ( int ) QEvent::GraphicsSceneDragLeave );
      PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      #endif
   }
   QGraphicsItem::dragLeaveEvent( event );
}
void HBQGraphicsItem::dragMoveEvent( QGraphicsSceneDragDropEvent * event )
{
   if( block )
   {
      #if 0
      PHB_ITEM p1 = hb_itemPutNI( NULL, ( int ) QEvent::GraphicsSceneDragMove );
      PHB_ITEM p2 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      #endif
   }
   QGraphicsItem::dragMoveEvent( event );
}
void HBQGraphicsItem::dropEvent( QGraphicsSceneDragDropEvent * event )
{
   if( block )
   {
      #if 0
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
      #endif
   }
   QGraphicsItem::dropEvent( event );
}
/*----------------------------------------------------------------------*/
//                              Painting
/*----------------------------------------------------------------------*/

QRectF HBQGraphicsItem::boundingRect() const
{
   return QRectF( 0, 0, width(), height() );
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::prepare( QPainter * painter )
{
   setupPainter( painter );

   switch( iType )
   {
   case HBQT_GRAPHICSITEM_SIMPLETEXT :
   {
      if( m_sizePolicy == HBQT_GRAPHICSITEM_TEXT_SIZEPOLICY_NONE ){
         return;
      }
      QRectF rect = boundingRect();
      adjustRect( rect );
      QFontMetricsF fm( painter->font() );
      if( m_sizePolicy == HBQT_GRAPHICSITEM_TEXT_SIZEPOLICY_AUTO ){
         qreal wd = fm.width( QString_text );
         if( wd > width() ){
            setWidth( wd );
         }
      }
      else {
         QRectF rc = fm.boundingRect( rect, textFlags(), QString_text );
         if( rc.height() > rect.height() ){
            //setStretch( rc.height() - rect.height() );
         }
      }
   }
   break;
   }
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::setupPainter( QPainter * painter )
{
   if( ! painter ){
      return;
   }
   painter->setPen( pen() );
   painter->setBackgroundMode( ( Qt::BGMode ) iBGMode );
   painter->setBackground( backgroundBrush() );

   QFont f=font();
   f.setPixelSize( font().pointSizeF() / UNIT );
   painter->setFont( f );

   painter->setOpacity( ( qreal ) iOpacity / 100.0 );
   painter->setBrush( brush() );
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::paint( QPainter * painter, const QStyleOptionGraphicsItem * option, QWidget * /* widget */ )
{
   switch( iType )
   {
   case HBQT_GRAPHICSITEM_NONE       :
      break;
   case HBQT_GRAPHICSITEM_RECT       :
      drawRect( painter, option );
      break;
   case HBQT_GRAPHICSITEM_LINE       :
      drawLine( painter, option );
      break;
   case HBQT_GRAPHICSITEM_ELLIPSE    :
      drawEllipse( painter, option );
      break;
   case HBQT_GRAPHICSITEM_ARC        :
      drawArc( painter, option );
      break;
   case HBQT_GRAPHICSITEM_CHORD      :
      drawChord( painter, option );
      break;
   case HBQT_GRAPHICSITEM_PIE        :
      drawPie( painter, option );
      break;
   case HBQT_GRAPHICSITEM_POLYGON    :
      break;
   case HBQT_GRAPHICSITEM_PATH       :
      break;
   case HBQT_GRAPHICSITEM_CHART      :
      drawBarChart( painter, option );
      break;
   case HBQT_GRAPHICSITEM_GRADIENT   :
      break;
   case HBQT_GRAPHICSITEM_PICTURE    :
      drawPicture( painter, option );
      break;
   case HBQT_GRAPHICSITEM_BARCODE    :
      //drawBarcode39( painter, option );
      break;
   case HBQT_GRAPHICSITEM_TEXT       :
      break;
   case HBQT_GRAPHICSITEM_SIMPLETEXT :
      drawText( painter, option );
      break;
   }

   if( block ){
      QRectF rect = ( option->type == QStyleOption::SO_GraphicsItem ) ? boundingRect() : option->exposedRect;

      PHB_ITEM p1 = hb_itemPutNI( NULL, 21017 );
      PHB_ITEM p2 = hb_itemPutPtr( NULL, painter );
      PHB_ITEM p3 = hb_itemNew( NULL );

      hb_arrayNew( p3, 2 );

      hb_arraySetPtr( p3, 1, &rect );
      hb_arraySetC( p3, 2, objectName().toLatin1().data() );

      hb_vmEvalBlockV( block, 3, p1, p2, p3 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      hb_itemRelease( p3 );
   }
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawSelection( QPainter * painter, const QRectF & rect )
{
   painter->save();

   if( isSelected() )
   {
      QBrush a;
      //a.setColor( QColor( 200,0,0,150 ) );
      a.setColor( QColor( 255,0,0 ) );
      a.setStyle( Qt::SolidPattern );
      if( bDrawSelectionBorder )
      {
         QPen p( Qt::DashLine );
         p.setBrush( a );
         painter->setPen( p );
         painter->drawRect( rect );
      }
      QPainterPath lt;
      lt.moveTo( 0,0 );
      lt.lineTo( 0,iResizeHandle );
      lt.lineTo( iResizeHandle, 0 );
      painter->fillPath( lt,a );

      QPainterPath rt;
      rt.moveTo( rect.width(),0 );
      rt.lineTo( rect.width(),iResizeHandle );
      rt.lineTo( rect.width()-iResizeHandle,0 );
      painter->fillPath( rt,a );

      QPainterPath lb;
      lb.moveTo( 0,rect.height() );
      lb.lineTo( 0,rect.height()-iResizeHandle );
      lb.lineTo( iResizeHandle,rect.height() );
      painter->fillPath( lb,a );

      QPainterPath rb;
      rb.moveTo( rect.width(),rect.height() );
      rb.lineTo( rect.width(),rect.height()-iResizeHandle );
      rb.lineTo( rect.width()-iResizeHandle,rect.height() );
      painter->fillPath( rb,a );
   }
   else
   {
      if( bDrawSelectionBorder )
      {
         QBrush a;
         a.setColor( QColor( 100,100,100,200 ) );
         a.setStyle( Qt::SolidPattern );

         QPen p( Qt::DashDotDotLine );
         p.setBrush( a );
         painter->setPen( p );
         painter->drawRect( rect );
      }
      else
      {
         painter->setPen( QColor( 0,0,0,100 ) );
         painter->drawLine( 0,0,0,2*iResizeHandle );
         painter->drawLine( 0,0,2*iResizeHandle,0 );
         painter->drawLine( rect.width(),0,rect.width()-2*iResizeHandle,0 );
         painter->drawLine( rect.width(),0,rect.width(),2*iResizeHandle );
         painter->drawLine( rect.width(),rect.height(),rect.width()-2*iResizeHandle, rect.height() );
         painter->drawLine( rect.width(),rect.height(),rect.width(), rect.height()-2*iResizeHandle );
         painter->drawLine( 0,rect.height(), 2*iResizeHandle, rect.height() );
         painter->drawLine( 0,rect.height(), 0, rect.height()-2*iResizeHandle );
      }
   }
   painter->restore();
}
/*----------------------------------------------------------------------*/

QRectF HBQGraphicsItem::adjustRect( QRectF & rect )
{
   qreal penwidth = pen().widthF();
   rect = rect.adjusted( penwidth, penwidth, -penwidth, -penwidth );
   return rect;
}
/*----------------------------------------------------------------------*/

QRectF HBQGraphicsItem::adjustOption( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = ( option->type == QStyleOption::SO_GraphicsItem ) ? boundingRect() : option->exposedRect;

   if( option->type == QStyleOption::SO_GraphicsItem ){
      drawSelection( painter, rect );
   }
   setupPainter( painter );
   adjustRect( rect );

   return rect;
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawRect( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = adjustOption( painter, option );

   //painter->drawRoundRect( rect, xRadius(), yRadius() );
   painter->drawRect( rect );
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawEllipse( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = adjustOption( painter, option );

   painter->drawEllipse( rect );
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawLine( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = adjustOption( painter, option );

   switch( lineStyle() )
   {
   case HBQT_GRAPHICSITEM_LINE_VERTICAL:
      painter->drawLine( rect.x() + rect.width() / 2, rect.y(), rect.x() +  rect.width() / 2, rect.y() + rect.height() );
      break;
   case HBQT_GRAPHICSITEM_LINE_HORIZONTAL:
      painter->drawLine( rect.x(), rect.y() + rect.height() / 2, rect.x() + rect.width(), rect.y() + rect.height() / 2 );
      break;
   case HBQT_GRAPHICSITEM_LINE_BACKWARDDIAGONAL:
      painter->drawLine( rect.right(), rect.y(), rect.x(), rect.bottom() );
      break;
   case HBQT_GRAPHICSITEM_LINE_FORWARDDIAGONAL:
      painter->drawLine( rect.x(), rect.y(), rect.right(), rect.bottom() );
      break;
   }
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawPie( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = adjustOption( painter, option );

   painter->drawPie( rect, iStartAngle * 16, iSpanAngle * 16 );
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawArc( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = adjustOption( painter, option );

   painter->drawArc( rect, iStartAngle * 16, iSpanAngle * 16 );
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawChord( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = adjustOption( painter, option );

   painter->drawChord( rect, iStartAngle * 16, iSpanAngle * 16 );
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawPicture( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = adjustOption( painter, option );

   QString m_text = QString_text;
   qreal   textH  = 0;
   qreal   sw     = 0;
   qreal   sh     = 0;

   if( m_drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE || m_drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_BELOW ){
      textH = QFontMetricsF( painter->font() ).height();
   }
   if( m_image.isNull() )
   {
      ( option->type == QStyleOption::SO_GraphicsItem ) ?
                        painter->drawRect( rect ) : painter->drawText( rect, Qt::AlignCenter, "No Picture" );
   }
   else
   {
      QImage img( 0, 0 /*,QImage::Format_ARGB32_Premultiplied*/ );

      QPointF point = rect.topLeft();
      int cx = 0, cy = 0, cw = m_image.width(), ch = m_image.height();

      switch( m_paintType )
      {
      case HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_KEEP_ASPECT_RATIO:
         img = m_image.scaled( rect.width(), rect.height() - textH, Qt::KeepAspectRatio, Qt::SmoothTransformation );
         break;

      case HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_IGNORE_ASPECT_RATIO:
         img = m_image.scaled( rect.width(), rect.height() - textH, Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
         break;

      case HBQT_GRAPHICSITEM_CENTER_PICTURE_TO_ITEM:
         point.setX( point.x() + ( rect.width() - m_image.width() ) / 2 );
         point.setY( point.y() + ( rect.height() - m_image.height() - textH ) / 2 );
         if( point.x() < 0 )
         {
            cx = abs( point.x() );
            cw -= 2 * cx;
            point.setX( 0 );
         }
         if( point.y() < 0 )
         {
            cy = abs( point.y() );
            ch -= 2 * cy;
            point.setY( 0 );
         }
         img = m_image.copy( cx, cy, cw, ch );
         break;

      case HBQT_GRAPHICSITEM_RESIZE_ITEM_TO_PICTURE:
         img = m_image;
         sw = img.width()-width();
         sh = img.height() - ( height() - textH );
         break;
      }

      if( m_drawTextType == HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE ){
         point.setY( point.y() + textH );
      }

      painter->drawImage( point, img );
   }

   painter->setPen( m_textColor );

   switch( m_drawTextType )
   {
   case HBQT_GRAPHICSITEM_TEXT_DRAW_TOP:
      painter->drawText( rect, Qt::AlignTop | Qt::AlignHCenter, m_text );
      break;

   case HBQT_GRAPHICSITEM_TEXT_DRAW_BOTTOM:
      painter->drawText( rect, Qt::AlignBottom | Qt::AlignHCenter, m_text );
      break;

   case HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE:
      painter->drawText( rect, Qt::AlignTop | Qt::AlignHCenter, m_text );
      break;

   case HBQT_GRAPHICSITEM_TEXT_DRAW_BELOW:
      painter->drawText( rect, Qt::AlignBottom | Qt::AlignHCenter, m_text );
      break;

   default:
      break;
   }

   if( sw || sh ){
      setWidth( width() + sw );
      setHeight( height() + sh );
   }

   if( m_borderWidth > 0 )
   {
      QPen pen;
      pen.setWidth( m_borderWidth );
      pen.setColor( m_borderColor );
      pen.setJoinStyle( Qt::MiterJoin );
      painter->setPen( pen );
      painter->setBrush( Qt::NoBrush );
      painter->drawRect( rect.x() + m_borderWidth / 2, rect.y() + m_borderWidth / 2,
                         rect.width() - m_borderWidth, rect.height() - m_borderWidth );
   }
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawText( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QRectF rect = adjustOption( painter, option );

   painter->setRenderHint( QPainter::TextAntialiasing );
   painter->drawText( rect, textFlags(), QString_text );
}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawBarChart( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   QList< _chartValue > val;

   PHB_ITEM values = NULL;
   if( block )
   {
      // Get Values from Application
   }
   if( values == NULL )
   {
      _chartValue v;

      v.key   = "Bananas";
      v.value = 120.0;
      v.color = generateNextColor();
      val << v;

      v.key   = "Oranges";
      v.value = 150.0;
      v.color = generateNextColor();
      val << v;

      v.key   = "Mangoes";
      v.value = 40.0;
      v.color = generateNextColor();
      val << v;
   }

   if( ! val.size() ){
      return;
   }

   QRectF rect = adjustOption( painter, option );

   qreal maxpv = 0;
   qreal minnv = 0;
   foreach( _chartValue cv, val )
   {
      if( cv.value > 0 && cv.value > maxpv )
         maxpv = cv.value;

      if( cv.value < 0 && cv.value < minnv )
         minnv = cv.value;
   }

   qreal absMaxVal = maxpv - minnv;
   qreal powVal = ( absMaxVal < 1 ) ? pow( 10.0, QString::number( absMaxVal ).right( QString::number( absMaxVal ).indexOf( '.' ) ).length() + 1 ) : 1;
   maxpv *= powVal;
   minnv *= powVal;

   maxpv = ( quint64 ) maxpv;
   minnv = ( quint64 )( -minnv );
   minnv = -minnv;

   painter->fillRect( rect,brush() );

   if( m_drawBorder )
      painter->drawRect( rect );

   int pw = abs( pen().widthF() ) ? abs( pen().widthF() ) : 1;
   QRectF rc = rect.adjusted( pw / 2, pw / 2, -pw, -pw );

   qreal f = 2;

   qreal chartStep = pow( 10.0, ( QString::number( absMaxVal ).left( QString::number( absMaxVal ).indexOf( '.' ) ).length() ) - 1 ) / f;
   qreal powStep = ( chartStep < 1 ) ? 10 : 1;
   chartStep *= powStep;
   maxpv *= powStep;
   minnv *= powStep;
   powVal *= powStep;

   maxpv = maxpv + ( ( (  ( quint64 ) maxpv % ( quint64 ) chartStep ) ? ( ( quint64 ) chartStep - (  ( quint64 ) maxpv % ( quint64 ) chartStep ) ) : 0 ) ) / powVal;
   minnv = minnv - ( ( ( -( quint64 ) minnv % ( quint64 ) chartStep ) ? ( ( quint64 ) chartStep - ( -( quint64 ) minnv % ( quint64 ) chartStep ) ) : 0 ) ) / powVal;
   quint64 maxVal = maxpv - minnv;

   qreal maxHeight = rc.height() - painter->fontMetrics().height();
   qreal valstep = maxHeight / ( maxVal / chartStep );

   if( valstep < painter->fontMetrics().height() )
   {
      chartStep *= ( ( ( quint64 ) ( painter->fontMetrics().height() / valstep ) ) + 1 );
      valstep = ( ( ( quint64 ) ( painter->fontMetrics().height() / valstep ) ) + 1 ) * valstep;
   }

   if( m_showLabels )
   {
      qreal maxLabelWidth = 0;
      for( int i = 0; i < maxVal / chartStep + 1 + ( ( quint64 ) maxVal % ( quint64 ) chartStep ? 1 : 0 ); i++ )
      {
         if( maxLabelWidth < painter->fontMetrics().width( QString::number( ( maxVal * i - chartStep * i ) / powVal ) ) ){
            maxLabelWidth = painter->fontMetrics().width( QString::number( ( maxVal * i - chartStep * i ) / powVal ) );
         }
      }
      int y = 0;
      for( int i = 0; i < maxVal / chartStep + 1 + ( ( quint64 ) maxVal % ( quint64 ) chartStep ? 1 : 0 ); i++ )
      {
         painter->drawText( QRectF( rc.x(), rc.y() + y, maxLabelWidth, painter->fontMetrics().height() ),
                         Qt::AlignRight | Qt::AlignVCenter, QString::number( ( maxpv - chartStep * i ) / powVal ) );
         y += valstep;
      }

      painter->drawLine( rc.x() + maxLabelWidth + 1 / UNIT / 4, rc.y(), rc.x() + maxLabelWidth + 1 / UNIT / 4, rc.y() + rect.height() );
      rc = rc.adjusted( maxLabelWidth + 1 / UNIT / 4, 0, 0, 0 );
   }

   if( m_showGrid )
   {
      int y = ( double ) painter->fontMetrics().height() / 2;
      for( int i = 0; i < maxVal / chartStep + 1 + ( ( quint64 ) maxVal % ( quint64 ) chartStep ? 1 : 0 ); i++ )
      {
         painter->drawLine( rc.x(), rc.y() + y, rc.x() + rc.width(), rc.y() + y );
         y += valstep;
      }
   }

   rc = rc.adjusted( 0, ( double ) painter->fontMetrics().height() / 2, 0, 0 );
   int x = m_barsIdentation;
   qreal barWidth = ( rc.width() - m_barsIdentation * ( val.size() + 1 ) ) / val.size();
   qreal py = maxHeight / maxVal;

   foreach( _chartValue cv, val )
   {
      QLinearGradient lg( QPointF( x + barWidth / 2, 0 ), QPointF( x + barWidth , 0 ) );
      lg.setSpread( QGradient::ReflectSpread );
      lg.setColorAt( 0, cv.color );
      lg.setColorAt( 1, QColor( cv.color.red() * m_toColorFactor, cv.color.green() * m_toColorFactor, cv.color.blue() * m_toColorFactor, cv.color.alpha() ) );
      painter->fillRect( QRectF( rc.x() + x, rc.y() + py * maxpv - py * cv.value * powVal, barWidth, py * cv.value * powVal ), QBrush( lg ) );
      if( m_showLabels ){
         painter->drawText( QRectF( rc.x() + x - m_barsIdentation / 2, rc.y() + py * maxpv - ( ( cv.value >= 0 ) ? painter->fontMetrics().height() : 0 ),
                                      barWidth + m_barsIdentation, painter->fontMetrics().height() ), Qt::AlignCenter, QString( "%1" ).arg( cv.value ) );
      }
      x += barWidth + m_barsIdentation;
   }
   #if 0  /* Legend */
   painter->fillRect( rect, brush() );
   painter->drawRect( rect );
   painter->translate( rect.topLeft() );
   qreal y = 1 / UNIT;
   qreal vstep = ( rect.height() - y - 1 / UNIT * val.size() ) / val.size();
   foreach( _chartValue cv, val )
   {
      painter->fillRect( QRectF( 1 / UNIT / 2, y, m_legendColorRectWidth, vstep ), QBrush( cv.color ) );
      painter->drawText( QRectF( 1 / UNIT + m_legendColorRectWidth, y, rect.width() - ( 1 / UNIT + m_legendColorRectWidth ), vstep ),
                                                                            Qt::AlignVCenter | Qt::AlignLeft, cv.key );
      y += vstep + 1 / UNIT;
   }
   #endif

}
/*----------------------------------------------------------------------*/

void HBQGraphicsItem::drawBarcode39( QPainter * painter, const QStyleOptionGraphicsItem * option )
{
   if( ! m_barValues.size() )
   {
      return;
   }
   QRectF rect = adjustOption( painter, option );
   QRectF rc = rect.adjusted( 5, 5, -10, -10 );

   QColor fl( Qt::white );
   QColor clr( Qt::black );
   int iBars = m_barValues.size();
   qreal w = rc.width() / iBars;
   qreal x = 0.0;
   for( int i = 0; i < iBars; i++ )
   {
      if( m_barValues.at( i ) == "-" )
      {
         painter->fillRect( QRectF( rc.x() + x, rc.y(), w, rc.height() ), clr );
      }
      else
      {
         painter->fillRect( QRectF( rc.x() + x, rc.y(), w, rc.height() ), fl );
      }
      x += w;
   }
}
/*----------------------------------------------------------------------*/


#endif
