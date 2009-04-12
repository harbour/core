/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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

#include <QtGui/QtGui>
#include <QtGui/QColor>

#include "gtqtc.h"
#include "hbapigt.h"

class MainWindow;

ConsoleArea::ConsoleArea(QWidget *parent)
    : QWidget(parent)
{
   setAttribute(Qt::WA_StaticContents);
   modified   = false;
   scribbling = false;
   myPenWidth = 1;
   myPenColor = Qt::blue;

   COLORS[ 0] = BLACK;
   COLORS[ 1] = BLUE;
   COLORS[ 2] = GREEN;
   COLORS[ 3] = CYAN;
   COLORS[ 4] = RED;
   COLORS[ 5] = MAGENTA;
   COLORS[ 6] = BROWN;
   COLORS[ 7] = WHITE;
   COLORS[ 8] = LIGHT_GRAY;
   COLORS[ 9] = BRIGHT_BLUE;
   COLORS[10] = BRIGHT_GREEN;
   COLORS[11] = BRIGHT_CYAN;
   COLORS[12] = BRIGHT_RED;
   COLORS[13] = BRIGHT_MAGENTA;
   COLORS[14] = YELLOW;
   COLORS[15] = BRIGHT_WHITE;
}

void ConsoleArea::sizeByFont(void)
{
   QPainter painter( this );
   qFont = QFont( tr("Courier New"), 12, -1, FALSE );
   qFont = QFont( qFont, painter.device() );
   QFontMetrics fontMetrics( qFont );
   fontHeight = fontMetrics.height();
   fontWidth  = fontMetrics.averageCharWidth();
   resize( fontWidth*80, fontHeight*25 );
}

bool ConsoleArea::openImage(const QString &fileName)
{
    QImage loadedImage;
    if (!loadedImage.load(fileName))
        return false;

    QSize newSize = loadedImage.size().expandedTo(size());
    resizeImage(&loadedImage, newSize);
    image = loadedImage;
    modified = false;
    update();
    return true;
}

bool ConsoleArea::saveImage(const QString &fileName, const char *fileFormat)
{
    QImage visibleImage = image;
    resizeImage(&visibleImage, size());

    if (visibleImage.save(fileName, fileFormat)) {
        modified = false;
        return true;
    } else {
        return false;
    }
}

void ConsoleArea::setPenColor(const QColor &newColor)
{
    myPenColor = newColor;
}

void ConsoleArea::setPenWidth(int newWidth)
{
    myPenWidth = newWidth;
}

void ConsoleArea::clearImage()
{
    image.fill(qRgb(255, 255, 255));
    modified = true;
    update();
}

void ConsoleArea::mousePressEvent(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton) {
        lastPoint = event->pos();
        scribbling = true;
    }
}

void ConsoleArea::mouseMoveEvent(QMouseEvent *event)
{
    if ((event->buttons() & Qt::LeftButton) && scribbling)
        drawLineTo(event->pos());
}

void ConsoleArea::mouseReleaseEvent(QMouseEvent *event)
{
    if (event->button() == Qt::LeftButton && scribbling) {
        drawLineTo(event->pos());
        scribbling = false;
    }
}

void ConsoleArea::paintEvent(QPaintEvent * /* event */)
{
    QPainter painter(this);
    painter.drawImage(QPoint(0, 0), image);
    {
       QFont font( tr( "Courier New" ), 12, -1, FALSE );
       font = QFont(font, painter.device());
       QFontMetrics fontMetrics( font );
       int height = fontMetrics.height();
       painter.setFont(font);
       painter.setBackgroundMode(Qt::OpaqueMode);
       {
          int    iRow,iCol,startCol,len ;
          int    iTop;
          USHORT usChar;
          BYTE   bColor, bAttr, bOldColor = 0;
          char   text[ 4000 ];

          iTop = 0;
          for( iRow = 0; iRow <= 24; iRow++ )
          {
             iCol = startCol = 0;
             len  = 0;
             iTop = ( iRow * height ) + height;

             while( iCol <= 79 )
             {
                if( !HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol, &bColor, &bAttr, &usChar ) )
                   break;

                if( len == 0 )
                {
                   bOldColor = bColor;
                }
                else if( bColor != bOldColor )
                {
                   QBrush brush( COLORS[ bOldColor >> 4 ] );
                   painter.setBackground( brush );
                   QPen pen( COLORS[ bOldColor ] );
                   painter.setPen( pen );

                   painter.drawText( QPointF( startCol,iTop ), QString( text ) );

                   bOldColor = bColor;
                   startCol  = iCol;
                   len       = 0;
                }
                text[ len++ ] = ( char ) usChar;
                iCol++;
             }
             if( len > 0 )
             {
                QBrush brush( COLORS[ bOldColor >> 4 ] );
                painter.setBackground( brush );
                QPen pen( COLORS[ bOldColor ] );
                painter.setPen( pen );

                painter.drawText(QPointF(startCol,iTop),QString(text));
             }
          }
       }
    }
}

void ConsoleArea::resizeEvent(QResizeEvent *event)
{
    if (width() > image.width() || height() > image.height()) {
        int newWidth = qMax(width() + 128, image.width());
        int newHeight = qMax(height() + 128, image.height());
        resizeImage(&image, QSize(newWidth, newHeight));
        update();
    }
    QWidget::resizeEvent(event);
}

void ConsoleArea::drawLineTo(const QPoint &endPoint)
{
    QPainter painter(&image);
    painter.setPen(QPen(myPenColor, myPenWidth, Qt::SolidLine, Qt::RoundCap,
                        Qt::RoundJoin));
    painter.drawLine(lastPoint, endPoint);
    modified = true;

    int rad = (myPenWidth / 2) + 2;
    update(QRect(lastPoint, endPoint).normalized().adjusted(-rad, -rad, +rad, +rad));
    lastPoint = endPoint;
}

void ConsoleArea::resizeImage(QImage *image, const QSize &newSize)
{
    if (image->size() == newSize)
        return;

    QImage newImage(newSize, QImage::Format_RGB32);
    newImage.fill(qRgb(255, 255, 255));
    QPainter painter(&newImage);
    painter.drawImage(QPoint(0, 0), *image);
    *image = newImage;
}

void ConsoleArea::print()
{
#ifndef QT_NO_PRINTER
    QPrinter printer(QPrinter::HighResolution);

    QPrintDialog *printDialog = new QPrintDialog(&printer, this);
    if (printDialog->exec() == QDialog::Accepted) {
        QPainter painter(&printer);
        QRect rect = painter.viewport();
        QSize size = image.size();
        size.scale(rect.size(), Qt::KeepAspectRatio);
        painter.setViewport(rect.x(), rect.y(), size.width(), size.height());
        painter.setWindow(image.rect());
        painter.drawImage(0, 0, image);
    }
#endif // QT_NO_PRINTER
}

/*----------------------------------------------------------------------*/
